package nasko.avrecorder

import java.io.File
import java.util.TimerTask
import java.util.Timer
import java.text.DateFormat
import java.util.Date
import java.text.SimpleDateFormat
import java.util.TimeZone
import java.util.Calendar
import nasko.avrecorder.Scheduler.{vlcAudioTimerTask, rarmaTimerTask}
import org.xml.sax.InputSource
import scala.xml._
import scala.collection.mutable.ListBuffer
import java.util.Scanner
import java.io.InputStream
import org.farng.mp3.{MP3File, TagConstant, TagOptionSingleton}

//import org.blinkenlights.jid3.{MP3File, MediaFile}
abstract class Station(val name:String, val folder:String, val timeZone:TimeZone, val timeAdvance:Int, val extraTime:Int) {
	def getRecorderTimerTask(article:Article) : TimerTask
	
	val bgAlphabet = " 0123456789АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЪЮЯабвгдежзийклмнопрстуфхцчшщьъюя"
	val engAlphabet = Array(
	"_","0","1","2","3","4","5","6","7","8","9",
	"A","B","V","G","D","E","J","Z","I","J","K","L","M","N","O","P","R","S","T","U","F","H","C","CH","SH","ST","A","A","IU","IA",
	"a","b","v","g","d","e","j","z","i","j","k","l","m","n","o","p","r","s","t","u","f","h","c","ch","sh","st","a","a","iu","ia"
	)

	def getFixedString(s:String) = {
		val sb = new StringBuffer();
		var prevC = '*'
		for (c <- s) {
			val index = bgAlphabet.indexOf(c);
			if (index>=0) {
				var nc = engAlphabet(index)
				if (prevC==' ') nc = nc.toUpperCase
				sb.append(nc)
			} 
			else if (('a'<=c && c<='z') || ('A'<=c && c<='Z')) 
				sb.append(c)
			else sb.append('_')
			prevC = c
		}
		sb.toString
	}

  def setTime(c:Calendar, hour:Int, minute:Int) = {
    val c2 = c.clone.asInstanceOf[Calendar]
    c2.set(Calendar.HOUR_OF_DAY, hour)
    c2.set(Calendar.MINUTE, minute)
    c2.set(Calendar.SECOND,0)
    c2.set(Calendar.MILLISECOND,0)
    c2
  }

  def newArticle(name:String, startHour: Int, startMinute:Int, duration: Int) = new Article(this, setTime(Calendar.getInstance(timeZone), startHour, startMinute), duration, name)

	def fixArticleDurations(l: List[Article]):Unit = {
		var h = l.head
		var t = l.tail
		do {
			h.duration = (t.head.start.getTime.getTime - h.start.getTime.getTime)/(1000*60)
			h = t.head
			t = t.tail
		} while (!t.isEmpty)
		h.duration = (Scheduler.nextDay(h.start).getTime.getTime - h.start.getTime.getTime)/(1000*60)
	}

	def dateFormatter(date:Date):String = {
		val df = DateFormat.getInstance
		df.setTimeZone(timeZone)
		df.format(date)
	}
	
}

class Article (val station:Station, val start:Calendar, var duration:Long, val name:String) {

	def getStartText = station.dateFormatter(start.getTime)

	override def toString = station.name + ", " + station.dateFormatter(start.getTime) + ", " + duration + ", " + name

	override def equals(a:Any) = {
		val b = a.asInstanceOf[Article]
		this.station==b.station &&
		this.start==b.start &&
		this.duration==b.duration &&
		this.name==b.name
	}

	object ArticleName {
		// The injection method (optional)
		//def apply(user: String, domain: String) = user +"@"+ domain

		// The extraction method (mandatory)
		def unapply(str: String): Option[String] = {
			Some(str)
		}
	}

	object ArticleLocalTimePeriod {
		def unapply(str: String): Option[(Long, Long)] = {
			val p = """(\d\d)-(\d\d)-(\d\d\d\d) (\d\d):(\d\d)( \d{1,3})?""".r
			str match { 
					case p(d,m,y,h,min,durationStr) =>
						val left = Calendar.getInstance(station.timeZone)
						left.set(y.toInt, m.toInt-1, d.toInt, h.toInt, min.toInt, 0)
						left.set(Calendar.MILLISECOND, 0)
						val start_time = left.getTime.getTime
						val duration = if (durationStr==null) 1 else durationStr.substring(1).toInt  
						println(duration)
						Some(start_time, start_time + duration*(1000*60))  
					case _ => None
			}
		}
	}

	def pick(pattern:String): Boolean = {
		val p = """(\S+)\s(.*)""".r
		val p(stationName, pat) = pattern
		if (!station.name.equals(stationName)) false 
		else {
			pat match {
				case ArticleLocalTimePeriod(left,right) => left <= start.getTime.getTime && start.getTime.getTime < right
				case ArticleName(namePattern) 		 => namePattern.r.findFirstIn(name)!=None
			}
		}
	}

}

class Gobbler(id:String, is:InputStream, suppress:Boolean) extends Thread {
	override def run() = {
		val sc=new Scanner(is)
		while(sc.hasNext) {
      val line = sc.nextLine
      if (!suppress) println(id+line)
    }
	}	 
}


object Scheduler extends App {

	def sorter(a:Article,b:Article) = { a.start.compareTo(b.start)<0 }
	
	def findNodes(node:Node, interesting:Node =>Boolean) : List[Node] = {
	    val res = new ListBuffer[Node]
	    if (interesting(node)) res += node
	    for(n <- node.child) res ++= findNodes(n, interesting)
	    res.toList
	}

	def printDoc(node:Node) : Unit = {
		println("Label: " + node.label)
		println("Text: " + node.text)
		node.attributes.foreach(println _)
    for(n <- node.child) printDoc(n)
	}

	def getBnrArticles(station:Station, url:String) : List[Article] = {

		val monthMap = Map("Януари" -> 0, "Февруари" -> 1, "Март" -> 2, "Април" -> 3, "Май" -> 4, "Юни" -> 5, "Юли" -> 6, "Август" -> 7, "Септември" -> 8, "Октомври" -> 9, "Ноември" -> 10, "Декември" -> 11)

    var articlesList = List.empty[Article]
    try {
      val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
      val parser = parserFactory.newSAXParser
      val source = new org.xml.sax.InputSource(url)
      val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
      val doc = adapter.loadXML(source, parser)

      //println(findNodes(doc, isWeekDay))
      val weekDays = findNodes(doc, (n: Node) => {
          val x = n.attribute("class")
          x!=None && x.get.toString.equals("week-day")
      })

      val articles = new ListBuffer[Article]
      for(day <- weekDays) {
        val date = findNodes(day, (n:Node) => { n.label.equals("span") }).head.text

        val Some(myMatch) = """\((\d{1,2})\s(\S*)\s(\d\d\d\d)\)(.*)""".r.findFirstMatchIn(date)
        val dayInt = myMatch.group(1).toInt
        val monthInt = monthMap(myMatch.group(2))
        val yearInt = myMatch.group(3).toInt

        val scheduleNode = findNodes(day, (n:Node) => { n.label.equals("dl") }).head
        val schedule = findNodes(scheduleNode, (n:Node) => {
          n.label.equals("dt") || n.label.equals("dd")
        }).toArray
        val size = schedule.length

        val p = """(.*):(.*)""".r
        var i = 0
        while (i<size) {
          val p(h,m) = schedule(i).text
          val article_start = Calendar.getInstance(station.timeZone)
          article_start.set(yearInt, monthInt, dayInt, h.toInt, m.toInt, 0)
          article_start.set(Calendar.MILLISECOND, 0)
          articles += new Article(station, article_start, 0, schedule(i+1).text)
          i += 2
        }
      }
      articlesList = articles.toList
      station.fixArticleDurations(articlesList)
    }
    catch { case e => println(e.getMessage)}
		
		articlesList
	}

	def getBntWorldArticles(station:Station) : List[Article] = {

		val monthMap = Map("Януари" -> 0, "Февруари" -> 1, "Март" -> 2, "Април" -> 3, "Май" -> 4, "Юни" -> 5, "Юли" -> 6, "Август" -> 7, "Септември" -> 8, "Октомври" -> 9, "Ноември" -> 10, "Декември" -> 11)

		val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
		val parser = parserFactory.newSAXParser
		val adapter = new scala.xml.parsing.NoBindingFactoryAdapter

		val p = """(.*):(.*)""".r
		val df = new SimpleDateFormat("dd-MM-yyyy")
		df.setTimeZone(station.timeZone)
		val date = Calendar.getInstance(station.timeZone)
		
	  val articles = new ListBuffer[Article]
		for (i <- 0.to(6)) {
			val dayString = df.format(date.getTime)
			val aDay = dayString.substring(0,2).toInt
			val aMonth = dayString.substring(3,5).toInt
			val aYear = dayString.substring(6,10).toInt
			val source = new org.xml.sax.InputSource("http://bnt.bg/bg/bnt_world/index/"+dayString)
			val doc = adapter.loadXML(source, parser)
	
			val weekDay = findNodes(doc, (n: Node) => {
			    val x = n.attribute("class")
			    x!=None && x.get.toString.equals("top_articles_by_category program")
			}).tail.head
	
			var schedule = findNodes(weekDay, (n:Node) => { n.label.equals("tr") })
			var currentHour=0
			var earlyMorning=false
			for (item <- schedule) {
				val timeString = item.child.head.text
				val p(h,m) = timeString
				val hInt=h.toInt
				val mInt=m.toInt
				if (!earlyMorning && hInt<currentHour) earlyMorning=true
				currentHour=hInt
				val article_start = Calendar.getInstance(station.timeZone)
				article_start.set(aYear, aMonth-1, aDay, hInt, mInt, 0)
				article_start.set(Calendar.MILLISECOND, 0)
				if (earlyMorning) article_start.add(Calendar.DATE,1)
				
				var nameTag = item.child.tail.head
				val aTags = findNodes(nameTag, (n:Node) => { n.label.equals("a") })
				if (aTags.nonEmpty) nameTag = aTags.head
				val nameString = nameTag.text.trim
				
				articles += new Article(station, article_start, 0, nameString)
			}
			date.add(Calendar.DATE, 1)
		}
		var articlesList = articles.toList
		articlesList = articlesList.sortWith(Scheduler.sorter)
		station.fixArticleDurations(articlesList)
		articlesList
	}

	def DnevnikBgTvGuide(station:Station, channelId:Int) : List[Article] = {
    val date = Calendar.getInstance(station.timeZone)
    date.add(Calendar.DATE,-1)
    val df = new SimpleDateFormat("dd-MM-yyyy")
    df.setTimeZone(station.timeZone)
    val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
    val parser = parserFactory.newSAXParser
    val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
    val p = """(.*):(.*)""".r
    val articles = new ListBuffer[Article]
    for (i <- 0.to(6)) {
      val url = "http://www.dnevnik.bg/sled5/tv/index.php?channels[]=" + channelId + "&den=" + df.format(date.getTime).replace("-", "%2F")
      val source = new org.xml.sax.InputSource(url)
        //"http://www.potv.bg/tv49.html?fromh=0")
      val doc = adapter.loadXML(source, parser)
      val tvList = findNodes(doc, (n: Node) => {
          val x = n.attribute("class")
          x!=None && x.get.toString.equals("tvlist")
      })
      if (!tvList.isEmpty) {
        val items = findNodes(tvList.head, (n: Node) => n.label=="li" )
        var currentHour=0
        var earlyMorning=false
        items.foreach(it => {
          val timeString = it.child.head.text
          val p(h,m) = timeString
          val hInt=h.toInt
          val mInt=m.toInt
          if (!earlyMorning && hInt<currentHour) earlyMorning=true
          currentHour=hInt
          val article_start = date.clone().asInstanceOf[Calendar]
          article_start.set(Calendar.HOUR_OF_DAY, hInt)
          article_start.set(Calendar.MINUTE, mInt)
          article_start.set(Calendar.SECOND, 0)
          //article_start.set(Calendar.MILLISECOND, 0)
          if (earlyMorning) article_start.add(Calendar.DATE,1)
          val nameString = it.child.tail.head.text.trim
          articles += new Article(station, article_start, 0, nameString)
        })
      }
      date.add(Calendar.DATE, 1)
    }
    var articlesList = articles.toList
    articlesList = articlesList.sortWith(Scheduler.sorter)
    station.fixArticleDurations(articlesList)
    articlesList
	}

	class rarmaTimerTask(article:Article) extends TimerTask {
		val externalID = Map(Horizont -> "10A1A3B1", HristoBotev -> "5EE5268D", BGRadio -> "3D410C8F")

		override def run() = {
      val rarmaRadio = System.getProperty("rarmaRadio")
      assert(rarmaRadio!= null && new File(rarmaRadio).exists, "rarmaRadio executable is not found")

			val df = new SimpleDateFormat("yyMMdd_HHmm")
			df.setTimeZone(article.station.timeZone)
			val timestamp = df.format(article.start.getTime)
			val albumName = article.station.getFixedString(article.name)
			val articleName = albumName + "_" +  timestamp
			val filename = article.station.getFixedString(article.station.name) + "_" + articleName
			val cmd = "\"" + rarmaRadio + "\" StationID=" + externalID(article.station.asInstanceOf[BnrStation]) + " Duration=" + (article.duration+article.station.timeAdvance+article.station.extraTime) + " Mute=true ActionEnd=CloseApp FileName=\"" + filename + "\" Tags=\"<genre>Radio</genre><song>" + articleName + "</song><album>" + albumName + "</album>\" Action=RecordOnlyNoSplit"
			println(cmd)
			val p = Runtime.getRuntime().exec(cmd)
//			p.waitFor
//			println("[" + filename + ", returned " + p.exitValue + ", stopped " + df.format(Calendar.getInstance.getTime) + "]")
		}
	}

  def getPID(fingerprint:String) = scala.io.Source.fromInputStream(Runtime.getRuntime()
      .exec("wmic PROCESS WHERE \"Caption='rtmpdump.exe' AND CommandLine like '%" + fingerprint + "%'\" GET ProcessId /FORMAT:list")
      .getInputStream).getLines.filter(it => it.indexOf("ProcessId")>=0).next.substring(10)

	class rtmpTimerTask(rtmpUrl:String, article:Article, sizePerMinute:Long, pageUrl:String, socks:String) extends TimerTask {

    def this(rtmpUrl:String, article:Article, sizePerMinute:Long) = this(rtmpUrl, article, sizePerMinute, "", "")

		def startGobblers(id:String, p:Process) : Unit = {
			new Gobbler(id+",STDOUT:"  , p.getInputStream, false).start
			new Gobbler(id+",STDERROR:", p.getErrorStream, false).start
		}
	
		override def run() = {
      val rtmpdump = System.getProperty("rtmpdump")
      assert(rtmpdump!= null && new File(rtmpdump).exists, "rtmpdump executable is not found")
      val sendSignal = System.getProperty("sendSignal")
      assert(sendSignal!= null && new File(sendSignal).exists, "sendSignal executable is not found")


			//val sizePerMinute=4257000L
			val df = new SimpleDateFormat("yyMMdd_HHmm")
			df.setTimeZone(article.station.timeZone)
			val timestamp = df.format(article.start.getTime)
			val filename = article.station.getFixedString(article.station.name) + "_" + article.station.getFixedString(article.name) + "_" + timestamp
			val targetDurationInMin=article.duration+article.station.timeAdvance+article.station.extraTime
			val targetSize=targetDurationInMin*sizePerMinute
			val fullFileName = article.station.folder + "\\" + filename + ".flv"
			val cmd = "\"" + rtmpdump + "\" -v -r " + rtmpUrl + " --quiet --stop 14400 --timeout 240 -o \"" + fullFileName + "\"" +
        (if (pageUrl=="") "" else " -p \"" + pageUrl + "\"") +
        (if (socks=="") "" else " -S " + socks)

			// --stop " + durationInSec + " 
			println(cmd + ", targetSize=" + targetSize + ", targetDuration=" + targetDurationInMin)
			val startPoint=Calendar.getInstance.getTimeInMillis
			val p = Runtime.getRuntime().exec(cmd)
			startGobblers(filename, p)
			Thread.sleep(targetDurationInMin*60*1000)
			var done = false
			val file = new File(fullFileName)
			while (!done) {
				val whatIsLeft=targetSize-file.length
				if (whatIsLeft>0) Thread.sleep(((whatIsLeft/sizePerMinute)+1)*60*1000) 
				else done=true
			}
			val endPoint=Calendar.getInstance.getTimeInMillis
			// https://www-304.ibm.com/support/docview.wss?uid=swg21468390&wv=1
			val pid =scala.io.Source.fromInputStream(Runtime.getRuntime()
					.exec("wmic PROCESS WHERE \"Caption='rtmpdump.exe' AND CommandLine like '%" + filename + "%'\" GET ProcessId /FORMAT:list")
					.getInputStream).getLines.filter(it => it.indexOf("ProcessId")>=0).next.substring(10)
			Runtime.getRuntime().exec("\"" + sendSignal + "\" " + pid)
			println("[" + filename + ", completed " + df.format(Calendar.getInstance.getTime) + ", for " + (endPoint-startPoint)/60000 + " minutes]")
		}
	} 

  class vlcAudioTimerTask(vlcUrl:String, article:Article) extends TimerTask {
    def startGobblers(id:String, p:Process) : Unit = {
      new Gobbler(id+",STDOUT:"  , p.getInputStream, true).start
      new Gobbler(id+",STDERROR:", p.getErrorStream, true).start
    }
// C:\Users\nasko>"C:\rtmpdump-2.3\rtmpdump.exe" -v -r rtmp://193.43.26.22/live/livestream1 --quiet --stop 14400 --timeout 240 -o "c:\temp\BntWorldTV_Rtmpdump_T_111206_0529.flv"

    override def run() = {
      val vlc = System.getProperty("vlc")
      assert(vlc!= null && new File(vlc).exists, "vlc executable is not found")

//      val sizePerMinute=4257000L
      val df = new SimpleDateFormat("yyMMdd_HHmm")
      df.setTimeZone(article.station.timeZone)
      val timestamp = df.format(article.start.getTime)
      val filename = article.station.getFixedString(article.station.name) + "_" + article.station.getFixedString(article.name) + "_" + timestamp
      val targetDurationInMin=article.duration+article.station.timeAdvance+article.station.extraTime
//      val targetSize=targetDurationInMin*sizePerMinute
      val fullFileName = article.station.folder + "\\" + filename + ".mp3"
      val cmd = "\"" + vlc + "\" " + vlcUrl + " --sout \"#transcode{acodec=mp3,ab=32,channels=2,samplerate=44100}:std{access=file,mux=dummy,dst=" + fullFileName + "}\" --run-time=" + targetDurationInMin*60 + " --intf=dummy --dummy-quiet vlc://quit"
      println(cmd + ", targetSize=?, targetDuration=" + targetDurationInMin)
      val p = Runtime.getRuntime().exec(cmd)
      startGobblers(filename, p)
/*
      Thread.sleep(targetDurationInMin*60*1000+ 10000)
      val endPoint=Calendar.getInstance.getTimeInMillis

      http://resource.dopus.com/viewtopic.php?f=18&t=12351
      val s: String = "\"c:\\Users\\nasko\\My Apps\\id3.exe\" -t testabc \"" + fullFileName + "\""
      println(s)
      Runtime.getRuntime().exec(s)
*/
/*
      TagOptionSingleton.getInstance().setDefaultSaveMode(TagConstant.MP3_FILE_SAVE_OVERWRITE)

      val oSourceFile = new File(fullFileName);
      val oMediaFile = new MP3File(oSourceFile);
      val oID3V2_3_0Tag = new ID3V2_3_0Tag();
      oID3V2_3_0Tag.setAlbum("Album1");  // sets TALB frame
      //oID3V2_3_0Tag.setArtist("Artist2");  // sets TPE1 frame
      //oID3V2_3_0Tag.setComment("Comment3");  // sets COMM frame with language "eng" and no description
      oID3V2_3_0Tag.setGenre("Blues4");  // sets TCON frame
      oID3V2_3_0Tag.setTitle(article.name);  // sets TIT2 frame
      //oID3V2_3_0Tag.setYear(1999);  // sets TYER frame
      oMediaFile.setID3Tag(oID3V2_3_0Tag);
      oMediaFile.sync();
*/

//"C:\Program Files\VideoLAN\VLC\vlc.exe" http://streaming.bnr.bg/HristoBotev :sout=#transcode{acodec=mp3,ab=128,TIT2="test1"}:std{access=file,mux=dummy,dst="e:\temp\test1.mp3",Title="title2"} --run-time=60 --intf=dummy --dummy-quiet vlc://quit
    }
  }

	class BnrStation(name:String, url:String, folder:String, timeZone:TimeZone) extends Station(name, folder, timeZone, 0, 0) {
		override def getRecorderTimerTask(article:Article) : TimerTask = new vlcAudioTimerTask(url, article)
	}
	
	class BntStation(name:String, folder:String, timeZone:TimeZone) extends Station(name, folder, timeZone, 5, 5) {
		override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask("rtmp://193.43.26.22/live/livestream1", article, 4257000L)
	}

  class BNT1Station(name:String, folder:String, timeZone:TimeZone) extends Station("BNT1", folder, timeZone, 5, 5) {
    override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask("rtmp://edge2.evolink.net:2020/fls/bnt.stream", article, 4257000L, "http://tv.bnt.bg/bnt1/", "vanja.gotdns.com:1080")
  }

// C:\Users\nasko>"C:\rtmpdump-2.3\rtmpdump.exe" -v -r rtmp://edge2.evolink.net:2020/fls/bnt.stream --stop 20 --timeout 240 -o "c:\temp\Test1.flv" -p "http://tv.bnt.bg/bnt1/" -S vanja.gotdns.com:1080 -V

	class NovaStation(name:String, folder:String, timeZone:TimeZone) extends Station(name, folder, timeZone, 5,5) {
//		override def getRecorderTimerTask(article:Article) : TimerTask = new vlcTimerTask("mms://94.156.248.42/nova_live_q3.wmv", article)
    override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask("rtmp://31.13.218.243/rtplive/mp4:nova_1000kbps.stream", article, 8574328L)
    // 8519686L ()
	}

	def findTargets(articles: List[Article], subscriptions: List[String]) : (List[Article],List[String]) = {
		var targets = scala.collection.mutable.HashSet.empty[Article]
		var warnings = List[String]()
		for (s <- subscriptions) {
			var matchFound=false
			for (a <- articles) {
				if (a.pick(s)) {
					matchFound=true
					targets += a
				}
			}
			if (!matchFound) warnings = "WARNING: No match found for subscription (" + s.toString + ")" :: warnings
		}
		(targets.toList.sortWith(Scheduler.sorter),warnings)
	}

  def nextDay(c:Calendar) = {
    val nextDay = c.clone.asInstanceOf[Calendar]
    nextDay.add(Calendar.DATE,1)
    nextDay.set(Calendar.HOUR_OF_DAY,0)
    nextDay.set(Calendar.MINUTE,0)
    nextDay.set(Calendar.SECOND,0)
    nextDay.set(Calendar.MILLISECOND,0)
    nextDay
  }

	val subscriptionFile = System.getProperty("subscriptionFile")
	assert(subscriptionFile!= null && new File(subscriptionFile).exists, "subscriptionFile is not found")
	
	val rootFolder = System.getProperty("rootFolder")
	assert(rootFolder!= null && new File(rootFolder).exists, "rootFolder is not found")

	val verboseProperty = System.getProperty("verbose")
	val verbose = verboseProperty!=null && verboseProperty.toBoolean
	
	val testRarmaProperty = System.getProperty("testRarma")
	val testRarma = testRarmaProperty!=null && testRarmaProperty.toBoolean
	
	val testProxyProperty = System.getProperty("testProxy")
	val testProxy = testProxyProperty!=null && testProxyProperty.toBoolean

  val testRtmpdumpProperty = System.getProperty("testRtmpdump")
  val testRtmpdump = testRtmpdumpProperty!=null && testRtmpdumpProperty.toBoolean

	val testVlcProperty = System.getProperty("testVlc")
	val testVlc = testVlcProperty!=null && testVlcProperty.toBoolean

  val testHorizontProperty = System.getProperty("testHorizont")
  val testHorizont = testHorizontProperty!=null && testHorizontProperty.toBoolean

	val bgTimeZone  = TimeZone.getTimeZone("Europe/Sofia")
	val Horizont 	  = new BnrStation("Horizont"	  , "http://streaming.bnr.bg/Horizont", rootFolder, bgTimeZone)
	val HristoBotev = new BnrStation("HristoBotev", "http://streaming.bnr.bg/HristoBotev", rootFolder, bgTimeZone)
	val BGRadio 	  = new BnrStation("BGRadio"		, "http://62.204.145.218:8000/bgradio128.m3u", rootFolder, bgTimeZone)
	val BntWorldTV 	= new BntStation("BntWorldTV"	, rootFolder, bgTimeZone)
	val NovaTV 		  = new NovaStation("NovaTV"		, rootFolder, bgTimeZone)
  val Bnt1TV 		  = new BNT1Station("Bnt1TV"		, rootFolder, bgTimeZone)
	//"C:\rtmpdump-2.3\rtmpdump.exe" -v -r rtmp://68.68.22.79/live/_definst_/bgtvbtv --stop 14400 --timeout 240 -o "c:\temp\BTV_Rtmpdump_Test.flv"
	
	def main = {
	// Loop forever and wake up every night at 12am BG time
    while (true) {
      // Get all Articles and Subscriptions
/*      
      val articles = List.concat(
        getBnrArticles(Horizont		  , "http://bnr.bg/sites/horizont/Pages/ProgramScheme.aspx")
//        getBnrArticles(HristoBotev	, "http://bnr.bg/sites/hristobotev/Pages/ProgramScheme.aspx"),
//        getBntWorldArticles(BntWorldTV),
//        DnevnikBgTvGuide(NovaTV, 99),
//        DnevnikBgTvGuide(Bnt1TV, 93)
      )
      val subscriptions = scala.io.Source.fromFile(subscriptionFile, "utf-8").getLines.toList.filter(it => !it.startsWith("#") && it!="")
*/
      val subsIter = scala.io.Source.fromFile(subscriptionFile, "utf-8").getLines
      subsIter.next
      var subscriptions = List.empty[String]
      while (subsIter.hasNext) {
        val line:String = subsIter.next
        if (!line.startsWith("#") && line!="") subscriptions = line :: subscriptions
      }
      val targetStations = subscriptions.map((s:String) => s.substring(0,s.indexOf(' '))).toSet
      
      var articles = List.empty[Article]
      try { if (targetStations.contains("Horizont"   )) articles ++= getBnrArticles(Horizont, "http://bnr.bg/sites/horizont/Pages/ProgramScheme.aspx") } catch { case _ => None }
      try { if (targetStations.contains("HristoBotev")) articles ++= getBnrArticles(HristoBotev	, "http://bnr.bg/sites/hristobotev/Pages/ProgramScheme.aspx") } catch { case _ => None }
      try { if (targetStations.contains("BntWorldTV" )) articles ++= getBntWorldArticles(BntWorldTV) } catch { case _ => None }
      try { if (targetStations.contains("NovaTV"     )) articles ++= DnevnikBgTvGuide(NovaTV, 99) } catch { case _ => None }
      try { if (targetStations.contains("BNT1"       )) articles ++= DnevnikBgTvGuide(Bnt1TV, 93) } catch { case _ => None }

      // Get all Targets
      val now = Calendar.getInstance(bgTimeZone)
      val tomorrow = Scheduler.nextDay(now)
      var (allTargets, warnings) = findTargets(articles, subscriptions)
      var targets = allTargets.filter(it => now.getTime.getTime <= it.start.getTime.getTime && it.start.getTime.getTime < tomorrow.getTime.getTime)
      if (testRarma)    targets = new Article(Horizont  , Calendar.getInstance, 1, "Rarma Тест") :: targets
      if (testRtmpdump) targets = new Article(BntWorldTV, Calendar.getInstance, 1, "Rtmpdump Тест") :: targets
      if (testProxy)    targets = new Article(Bnt1TV    , Calendar.getInstance, 1, "Proxy Тест") :: targets
      if (testVlc)      targets = new Article(NovaTV    , Calendar.getInstance, 1, "Vlc Тест") :: targets
      if (testHorizont) targets = new Article(Horizont   , Calendar.getInstance, 1, "Horizont Тест") :: targets

      //targets = HristoBotev.newArticle("Vreme za prikazka", 18, 20, 10) :: targets

      if (verbose) {
        println("--- Articles ---")		;articles.foreach(println)
        //println("--- Subscriptions ---");subscriptions.foreach(println)
        println("--- All Targets ---")		;allTargets.foreach(println)
        //println("--- Warnings ---")		;warnings.foreach(println)
      }

      // schedule all targets
      println("------")
      targets.foreach(a => {
        val adjusted_start = a.start.clone.asInstanceOf[Calendar]
        adjusted_start.add(Calendar.MINUTE, -a.station.timeAdvance)
        println("Scheduled for local time " + new SimpleDateFormat("MM-dd-yyyy HH:mm").format(adjusted_start.getTime) + " (" + a + ")")
        new Timer().schedule(a.station.getRecorderTimerTask(a), adjusted_start.getTime)
      })

      val sleepTime = tomorrow.getTime.getTime-now.getTime.getTime
      println("Sleep for " + sleepTime/(1000*60) + " minutes.")
      Thread.sleep(sleepTime)
    }

	}

	main

}

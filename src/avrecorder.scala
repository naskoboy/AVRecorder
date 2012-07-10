package nasko.avrecorder

import java.io.File
import java.util.TimerTask
import java.util.Timer
import java.text.DateFormat
import java.util.Date
import java.text.SimpleDateFormat
import java.util.TimeZone
import java.util.Calendar
//import nasko.avrecorder.Scheduler.{vlcAudioTimerTask, rarmaTimerTask}
import org.xml.sax.InputSource
import scala.xml._
import scala.collection.mutable.ListBuffer
import java.util.Scanner
import java.io.InputStream
//import org.farng.mp3.{MP3File, TagConstant, TagOptionSingleton}

//import org.blinkenlights.jid3.{MP3File, MediaFile}


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

  def getPID(appName:String, filename:String) = scala.io.Source.fromInputStream(Runtime.getRuntime()
        .exec("wmic PROCESS WHERE \"Caption='" + appName + "' AND CommandLine like '%" + filename + "%'\" GET ProcessId /FORMAT:list")
        .getInputStream).getLines.filter(it => it.indexOf("ProcessId")>=0).next.substring(10)


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
    override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask("rtmp://31.13.218.243/rtplive/mp4:nova_1000kbps.stream", article, 8574328L,"http://novatv.bg/live","")
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
      println("-------")
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

  def test = {
    val r = Runtime.getRuntime
          val tag = new String("title=xcv".getBytes(), "UTF-8")
    val arg = List("c:\\users\\nasko\\My Apps\\tag.exe","-u", tag ,"c:\\temp\\Horizont_Horizont_Test_120626_0557.mp3")
    r.exec(arg.toArray)
  }

	main
//  test

}

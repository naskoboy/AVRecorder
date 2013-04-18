package nasko.avrecorder

import java.io.File
import java.util.TimerTask
import java.util.Timer
import java.text.SimpleDateFormat
import java.util.TimeZone
import java.util.Calendar
import scala.xml._
import scala.collection.mutable.ListBuffer
import org.apache.log4j._

//import org.farng.mp3.{MP3File, TagConstant, TagOptionSingleton}
//import org.blinkenlights.jid3.{MP3File, MediaFile}

object Scheduler extends App {

	def sorter(a:Article,b:Article) = { a.start.compareTo(b.start)<0 }
	
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
      val tvList = Utils.findNodes(doc, (n: Node) => {
          val x = n.attribute("class")
          x!=None && x.get.toString.equals("tvlist")
      })
      if (!tvList.isEmpty) {
        val items = Utils.findNodes(tvList.head, (n: Node) => n.label=="li" )
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
    articlesList = articlesList.sortWith(sorter)
    station.fixArticleDurations(articlesList)
    articlesList
	}


	class BnrStation(name:String, url:String, folder:String, timeZone:TimeZone) extends Station(name, folder, timeZone, 0, 0) {
		override def getRecorderTimerTask(article:Article) : TimerTask = new vlcAudioTimerTask(url, article)
	}
	
	class BntStation(name:String, folder:String, timeZone:TimeZone) extends Station(name, folder, timeZone, 5, 5) {
		override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask("rtmp://193.43.26.22/live/livestream1", article, 0 /*4257000L*/)
	}

  class BNT1Station(name:String, folder:String, timeZone:TimeZone) extends Station("BNT1", folder, timeZone, 5, 5) {
    override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask("rtmp://edge2.evolink.net:2020/fls/bnt.stream", article, 4257000L, "http://tv.bnt.bg/bnt1/", "vanja.gotdns.com:1080")
  }

// C:\Users\nasko>"C:\rtmpdump-2.3\rtmpdump.exe" -v -r rtmp://edge2.evolink.net:2020/fls/bnt.stream --stop 20 --timeout 240 -o "c:\temp\Test1.flv" -p "http://tv.bnt.bg/bnt1/" -S vanja.gotdns.com:1080 -V

	class NovaStation(name:String, folder:String, timeZone:TimeZone) extends Station(name, folder, timeZone, 5,5) {
//		override def getRecorderTimerTask(article:Article) : TimerTask = new vlcTimerTask("mms://94.156.248.42/nova_live_q3.wmv", article)
    override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask("rtmp://31.13.218.242/rtplive/mp4:nova_1000kbps.stream", article, 0 /*8574328L*/,"http://novatv.bg/live","")
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

  var userDir = System.getProperty("my.user.dir")
  if (userDir==null) userDir = System.getProperty("user.dir")

	val subscriptionFile = userDir + "\\subscriptions.txt"
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

	val testNovaProperty = System.getProperty("testNova")
	val testNova = testNovaProperty!=null && testNovaProperty.toBoolean

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
      logger.info("-- START --")
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
      try { if (targetStations.contains("Horizont"   )) articles ++= ArticleCollectors.getBnrArticles(Horizont, "http://bnr.bg/sites/horizont/Pages/ProgramScheme.aspx") } catch { case _ => None }
      try { if (targetStations.contains("HristoBotev")) articles ++= ArticleCollectors.getBnrArticles(HristoBotev	, "http://bnr.bg/sites/hristobotev/Pages/ProgramScheme.aspx") } catch { case _ => None }
      try { if (targetStations.contains("BntWorldTV" )) articles ++= ArticleCollectors.getBntWorldArticles(BntWorldTV) } catch { case _ => None }
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
      if (testNova)     targets = new Article(NovaTV    , Calendar.getInstance, 1, "Nova Тест") :: targets
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
        logger.info("Scheduled for local time " + new SimpleDateFormat("MM-dd-yyyy HH:mm").format(adjusted_start.getTime) + " (" + a + ")")
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

  val appender = new FileAppender
  appender.setFile(userDir + "\\AVRecorder.log", true, false, 10)
  val layout = new org.apache.log4j.PatternLayout("%d - %m%n")
  appender.setLayout(layout)

  val logger = Logger.getLogger(Scheduler.getClass.getCanonicalName)
  logger.addAppender(appender)
  logger.addAppender(new ConsoleAppender(layout))

//  PropertyConfigurator.configure("log4j.properties")
//  logger.debug("Sample debug message")
//  logger.info("info")


//  println(System.getProperty("my.user.dir"))
	main
//  test

}

package nasko.avrecorder

import java.io.File
import java.util._
import org.apache.log4j._
import scala.List

//import org.farng.mp3.{MP3File, TagConstant, TagOptionSingleton}
//import org.blinkenlights.jid3.{MP3File, MediaFile}


object Scheduler extends App {

  object NovaTV extends Station("NovaTV", rootFolder, bgTimeZone, 5, 5, 6*60) {
    def getArticles = ArticleCollectors.StartBgArticles(this, "nova%20tv")
      //ArticleCollectors.Chasa24Articles(this, 17930)

    override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask(
      List(
      "-r", "rtmp://e1.cdn.bg:2060/fls",
      "-a", "fls",
      "-f", "WIN 11,7,700,224",
      "-W", "http://i.cdn.bg/eflash/jwNTV/jplayer.swf",
      "-p", "http://i.cdn.bg/live/0OmMKJ4SgY",
      "-y", "ntv_1.stream",
      "-T", "N0v4TV6#2",
      "-S", "localhost:1080"
      ),
//      "\"rtmp://e1.cdn.bg:2060/fls\" -a \"fls\" -f \"WIN 11,7,700,224\" -W \"http://i.cdn.bg/eflash/jwNTV/jplayer.swf\" -p \"http://i.cdn.bg/live/0OmMKJ4SgY\" -y \"ntv_1.stream\" -T \"N0v4TV6#2\""
      article,
      0 /*8574328L*/)
  }

  object Bnt1TV extends Station("BNT1", rootFolder, bgTimeZone, 5, 5) {
    def getArticles = ArticleCollectors.Chasa24Articles(this, 17931)
    override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask(
      List(),
      article,
      4257000L)
  }

  object BntWorldTV extends Station("BntWorldTV", rootFolder, bgTimeZone, 5, 5, 6*60) {
    def getArticles = ArticleCollectors.StartBgArticles(this, "bnt%20world")
      //ArticleCollectors.getBntWorldArticles(this)

    override def getRecorderTimerTask(article:Article) : TimerTask = new rtmpTimerTask(
      List("-r", "rtmp://193.43.26.22/live/livestream1"),
      article,
      0 /*4257000L*/)
  }

  object Horizont extends Station("Horizont", rootFolder, bgTimeZone, 0, 5, 12*60) {
    override def getArticles = ArticleCollectors.getBnrArticles(this, "http://bnr.bg/sites/horizont/Pages/ProgramScheme.aspx")
    override def getRecorderTimerTask(article:Article) : TimerTask = new vlcAudioTimerTask("http://stream.bnr.bg:8002/horizont.mp3", article)
  }

  object HristoBotev extends Station("HristoBotev", rootFolder, bgTimeZone, 0, 5, 12*60) {
    override def getArticles = ArticleCollectors.getBnrArticles(this, "http://bnr.bg/sites/hristobotev/Pages/ProgramScheme.aspx")
    override def getRecorderTimerTask(article:Article) : TimerTask = new vlcAudioTimerTask("http://stream.bnr.bg:8003/botev.mp3", article)
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
	
	val bgTimeZone  = TimeZone.getTimeZone("Europe/Sofia")
//	val Horizont 	  = new BnrStation("Horizont"	  , "mmsh://84.242.176.20:80/Horizont?MSWMExt=.asf", rootFolder, bgTimeZone)
//  val Horizont 	  = new BnrStation("Horizont"	  , "http://streaming.bnr.bg/Horizont", rootFolder, bgTimeZone)
//	val HristoBotev = new BnrStation("HristoBotev", "http://streaming.bnr.bg/HristoBotev", rootFolder, bgTimeZone)
//	val BGRadio 	  = new BnrStation("BGRadio"		, "http://62.204.145.218:8000/bgradio128.m3u", rootFolder, bgTimeZone)
//	val BntWorldTV 	= new BntStation("BntWorldTV"	, rootFolder, bgTimeZone)
	//val NovaTV 		  = new NovaStation("NovaTV"		, rootFolder, bgTimeZone)
  //val Bnt1TV 		  = new BNT1Station("Bnt1TV"		, rootFolder, bgTimeZone)
	//"C:\rtmpdump-2.3\rtmpdump.exe" -v -r rtmp://68.68.22.79/live/_definst_/bgtvbtv --stop 14400 --timeout 240 -o "c:\temp\BTV_Rtmpdump_Test.flv"
	
	def main {
    //ArticleCollectors.StartBgArticles(BntWorldTV , "bnt%20world").foreach (println _) ; return
    //ArticleCollectors.StartBgArticles(NovaTV , "nova%20tv").foreach (println _) ; return

    val stations = List(NovaTV, Bnt1TV, BntWorldTV, Horizont, HristoBotev)

    val testStationsStr = System.getProperty("testStations")
    if (testStationsStr!="") {
      val testStations = testStationsStr.split(',')
      stations.foreach(station =>
        if (testStations.exists(_==station.name)) station.getRecorderTimerTask(new Article(station, Calendar.getInstance(), 1, "Тest")).run()
      )
    } else {
      //new Timer().schedule(NovaTV.getRecorderTimerTask(new Article(NovaTV, Calendar.getInstance(), 1, "Test")), 0)
      val activeStations = System.getProperty("activeStations").split(',')
      stations.foreach(station => if (activeStations.exists(_==station.name)) station.start )
    }

  }

  def test {

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

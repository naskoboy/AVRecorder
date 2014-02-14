package nasko.avrecorder

import java.io.{InputStream, File}
import java.text.SimpleDateFormat
import java.util.{Scanner, Calendar, TimerTask}
import org.jaudiotagger.audio.AudioFileIO
import org.jaudiotagger.tag.FieldKey
import scala.collection.JavaConversions._

class Gobbler(id:String, is:InputStream, suppress:Boolean) extends Thread {
	override def run() = {
		val sc=new Scanner(is)
		while(sc.hasNext) {
      val line = sc.nextLine
      if (!suppress) println(id+line)
    }
	}
}

class rtmpTimerTask(params: List[String], article: Article, sizePerMinute: Long) extends TimerTask {

  //def this(rtmpUrl: String, article: Article, sizePerMinute: Long) = this(rtmpUrl, article, sizePerMinute)

  def startGobblers(id: String, p: Process): Unit = {
    new Gobbler(id + ",STDOUT:", p.getInputStream, false).start
    new Gobbler(id + ",STDERROR:", p.getErrorStream, false).start
  }

  override def run() = {
    val rtmpdump = Scheduler.userDir + "\\apps\\rtmpdump.exe" //System.getProperty("rtmpdump")
    assert(rtmpdump != null && new File(rtmpdump).exists, "rtmpdump executable is not found")
    val sendSignal = Scheduler.userDir + "\\apps\\sendSignal.exe"
    assert(sendSignal != null && new File(sendSignal).exists, "sendSignal executable is not found")

    //val sizePerMinute=4257000L
    val df = new SimpleDateFormat("yyMMdd_HHmm")
    df.setTimeZone(article.station.timeZone)
    val timestamp = df.format(article.start.getTime)
    val filename = article.station.getFixedString(article.station.name) + "_" + article.station.getFixedString(article.name) + "_" + timestamp
    val targetDurationInMin = article.duration + article.station.timeAdvance + article.station.extraTime
    val targetSize = targetDurationInMin * sizePerMinute
    val fullFileName = article.station.folder + "\\" + filename + ".flv"
    //val cmd = "\"" + rtmpdump + "\" -v -r " + rtmpUrl + " --quiet --stop 14400 --timeout 240 -o \"" + fullFileName + "\""
    //  (if (pageUrl == "") "" else " -p \"" + pageUrl + "\"") +
    //  (if (socks == "") "" else " -S " + socks)

    // --stop " + durationInSec + "
    //Scheduler.logger.info(cmd + ", targetSize=" + targetSize + ", targetDuration=" + targetDurationInMin)
    val startPoint = Calendar.getInstance.getTimeInMillis
    //val p = Runtime.getRuntime().exec(cmd)


    val cmdParams = List(
      rtmpdump,
      "-v",
      "--quiet",
      "--stop", "14400",
      "--timeout", "240",
      "-o", fullFileName
    ) ++ params

    //"C:\Users\nasko\IdeaProjects\AVRecorder\apps\rtmpdump.exe" -v -r "rtmp://e1.cdn.bg:2060/fls" -a "fls" -f "WIN 11,7,700,224" -W "http://i.cdn.bg/eflash/jwNTV/jplayer.swf" -p "http://i.cdn.bg/live/0OmMKJ4SgY" -y "ntv_1.stream" -T "N0v4TV6#2" --quiet --stop 14400 --timeout 240 -o "d:\temp\NovaTV_Test_130825_0558.flv"

    Scheduler.logger.info(Utils.toCommand(cmdParams))
    val pb = new ProcessBuilder(cmdParams)
    val p = pb.start()

    startGobblers(filename, p)
    Thread.sleep(targetDurationInMin * 60 * 1000)
    if (sizePerMinute>0) {
      var done = false
      val file = new File(fullFileName)
      while (!done) {
        val whatIsLeft = targetSize - file.length
        if (whatIsLeft > 0) Thread.sleep(((whatIsLeft / sizePerMinute) + 1) * 60 * 1000)
        else done = true
      }
    }
    val endPoint = Calendar.getInstance.getTimeInMillis
    // https://www-304.ibm.com/support/docview.wss?uid=swg21468390&wv=1
    val pid = Utils.getPID("rtmpdump.exe", filename)
    /*
          val pid =scala.io.Source.fromInputStream(Runtime.getRuntime()
              .exec("wmic PROCESS WHERE \"Caption='rtmpdump.exe' AND CommandLine like '%" + filename + "%'\" GET ProcessId /FORMAT:list")
              .getInputStream).getLines.filter(it => it.indexOf("ProcessId")>=0).next.substring(10)
    */
    Runtime.getRuntime().exec("\"" + sendSignal + "\" " + pid)
    Scheduler.logger.info("[" + filename + ", completed " + df.format(Calendar.getInstance.getTime) + ", for " + (endPoint - startPoint) / 60000 + " minutes]")
  }
}

class vlcAudioTimerTask(vlcUrl: String, article: Article) extends TimerTask {
  def startGobblers(id: String, p: Process, suppress:Boolean = true): Unit = {
    new Gobbler(id + ",STDOUT:", p.getInputStream, suppress).start
    new Gobbler(id + ",STDERROR:", p.getErrorStream, suppress).start
  }

  // C:\Users\nasko>"C:\rtmpdump-2.3\rtmpdump.exe" -v -r rtmp://193.43.26.22/live/livestream1 --quiet --stop 14400 --timeout 240 -o "c:\temp\BntWorldTV_Rtmpdump_T_111206_0529.flv"

  override def run() = {
    val vlc = System.getProperty("vlc")
    assert(vlc != null && new File(vlc).exists, "vlc executable is not found")
    val sendSignal = Scheduler.userDir + "\\apps\\sendSignal.exe"
    assert(sendSignal != null && new File(sendSignal).exists, "sendSignal executable is not found")
    //val id3 = Scheduler.userDir + "\\apps\\id3.exe"
    //assert(id3 != null && new File(id3).exists, "id3 executable is not found")

    //      val sizePerMinute=4257000L
    val df = new SimpleDateFormat("yyMMdd_HHmm")
    df.setTimeZone(article.station.timeZone)
    val timestamp = df.format(article.start.getTime)
    val fixedArticleName: String = article.station.getFixedString(article.name)
    val filename = article.station.getFixedString(article.station.name) + "_" + fixedArticleName + "_" + timestamp
    val targetDurationInMin = article.duration + article.station.timeAdvance + article.station.extraTime
    //      val targetSize=targetDurationInMin*sizePerMinute
    val fullFileName = article.station.folder + "\\" + filename + ".mp3"
    //val cmd = "\"" + vlc + "\" " + vlcUrl + " --sout \"#transcode{acodec=mp3,ab=32,channels=2,samplerate=44100}:std{access=file,mux=dummy,dst=" + fullFileName + "}\" --run-time=" + 120 * 60 + " -I dummy --dummy-quiet vlc://quit"
    //Scheduler.logger.info(cmd + ", targetSize=?, targetDuration=" + targetDurationInMin)
    val startPoint = Calendar.getInstance.getTimeInMillis

//    val pb = new ProcessBuilder(vlc, vlcUrl, "--sout", "#transcode{acodec=mp3,ab=32,channels=2,samplerate=44100}:std{access=file,mux=dummy,dst=" + fullFileName + "}", "--run-time=" + 120 * 60, "-I","dummy", "--dummy-quiet", "vlc://quit")
    val cmdParams =  List(
      vlc,
      vlcUrl,
      "--sout", "#transcode{acodec=mp3,ab=32,channels=2,samplerate=44100}:std{access=file,mux=dummy,dst=" + fullFileName + "}",
      "--run-time=" + 120 * 60,
      "-I", "dummy",
      "--dummy-quiet",
      "vlc://quit"
    )

    println(Utils.toCommand(cmdParams))
    val pb = new ProcessBuilder(cmdParams)
    val p = pb.start()

//    val p = Runtime.getRuntime().exec(cmd)

    startGobblers(filename, p, false)
    val pid = Utils.getPID("vlc.exe", filename)
    Thread.sleep(targetDurationInMin * 60 * 1000)
    new ProcessBuilder(sendSignal, pid).start().waitFor()
    //val proc = Runtime.getRuntime().exec("\"" + sendSignal + "\" " + pid)
    //proc.waitFor()
    //Thread.sleep(2000)
    //val cmd2 = "\"" + id3 + "\" -l \"" + fixedArticleName + "\" -t \"" + fixedArticleName + "_" + timestamp + "\" " + fullFileName.replace("\\", "\\\\")
    //Scheduler.logger.info(cmd2)
    //Runtime.getRuntime().exec(cmd2)

    val f = AudioFileIO.read(new File(fullFileName))
    val tag = new org.jaudiotagger.tag.id3.ID3v23Tag()
    tag.addField(FieldKey.TITLE, fixedArticleName + "_" + timestamp)
    tag.addField(FieldKey.ALBUM, fixedArticleName)
    f.setTag(tag)
    f.commit

    val endPoint = Calendar.getInstance.getTimeInMillis
    Scheduler.logger.info("[" + filename + ", completed " + df.format(Calendar.getInstance.getTime) + ", for " + (endPoint - startPoint) / 60000 + " minutes]")

    //Thread.sleep(1000)

    /*
          Thread.sleep(targetDurationInMin*60*1000+ 10000)
          val endPoint=Calendar.getInstance.getTimeInMillis

          http://resource.dopus.com/viewtopic.php?f=18&t=12351
          val s: String = "\"c:\\Users\\nasko\\My Apps\\id3.exe\" -t testabc \"" + fullFileName + "\""
          println(s)
          Runtime.getRuntime().exec(s)
    */
    //"C:\Program Files\VideoLAN\VLC\vlc.exe" http://streaming.bnr.bg/HristoBotev :sout=#transcode{acodec=mp3,ab=128,TIT2="test1"}:std{access=file,mux=dummy,dst="e:\temp\test1.mp3",Title="title2"} --run-time=60 --intf=dummy --dummy-quiet vlc://quit
  }
}

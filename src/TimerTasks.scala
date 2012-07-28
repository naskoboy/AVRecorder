package nasko.avrecorder

import java.io.{InputStream, File}
import java.text.SimpleDateFormat
import java.util.{Scanner, Calendar, TimerTask}

class Gobbler(id:String, is:InputStream, suppress:Boolean) extends Thread {
	override def run() = {
		val sc=new Scanner(is)
		while(sc.hasNext) {
      val line = sc.nextLine
      if (!suppress) println(id+line)
    }
	}
}

class rtmpTimerTask(rtmpUrl: String, article: Article, sizePerMinute: Long, pageUrl: String, socks: String) extends TimerTask {

  def this(rtmpUrl: String, article: Article, sizePerMinute: Long) = this(rtmpUrl, article, sizePerMinute, "", "")

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
    val cmd = "\"" + rtmpdump + "\" -v -r " + rtmpUrl + " --quiet --stop 14400 --timeout 240 -o \"" + fullFileName + "\"" +
      (if (pageUrl == "") "" else " -p \"" + pageUrl + "\"") +
      (if (socks == "") "" else " -S " + socks)

    // --stop " + durationInSec + "
    println(cmd + ", targetSize=" + targetSize + ", targetDuration=" + targetDurationInMin)
    val startPoint = Calendar.getInstance.getTimeInMillis
    val p = Runtime.getRuntime().exec(cmd)
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
    println("[" + filename + ", completed " + df.format(Calendar.getInstance.getTime) + ", for " + (endPoint - startPoint) / 60000 + " minutes]")
  }
}

class vlcAudioTimerTask(vlcUrl: String, article: Article) extends TimerTask {
  def startGobblers(id: String, p: Process): Unit = {
    new Gobbler(id + ",STDOUT:", p.getInputStream, true).start
    new Gobbler(id + ",STDERROR:", p.getErrorStream, true).start
  }

  // C:\Users\nasko>"C:\rtmpdump-2.3\rtmpdump.exe" -v -r rtmp://193.43.26.22/live/livestream1 --quiet --stop 14400 --timeout 240 -o "c:\temp\BntWorldTV_Rtmpdump_T_111206_0529.flv"

  override def run() = {
    val vlc = System.getProperty("vlc")
    assert(vlc != null && new File(vlc).exists, "vlc executable is not found")
    val sendSignal = Scheduler.userDir + "\\apps\\sendSignal.exe"
    assert(sendSignal != null && new File(sendSignal).exists, "sendSignal executable is not found")

    //      val sizePerMinute=4257000L
    val df = new SimpleDateFormat("yyMMdd_HHmm")
    df.setTimeZone(article.station.timeZone)
    val timestamp = df.format(article.start.getTime)
    val filename = article.station.getFixedString(article.station.name) + "_" + article.station.getFixedString(article.name) + "_" + timestamp
    val targetDurationInMin = article.duration + article.station.timeAdvance + article.station.extraTime
    //      val targetSize=targetDurationInMin*sizePerMinute
    val fullFileName = article.station.folder + "\\" + filename + ".mp3"
    val cmd = "\"" + vlc + "\" " + vlcUrl + " --sout \"#transcode{acodec=mp3,ab=32,channels=2,samplerate=44100}:std{access=file,mux=dummy,dst=" + fullFileName + "}\" --run-time=" + 120 * 60 + " --intf=dummy --dummy-quiet vlc://quit"
    println(cmd + ", targetSize=?, targetDuration=" + targetDurationInMin)
    val startPoint = Calendar.getInstance.getTimeInMillis
    val p = Runtime.getRuntime().exec(cmd)
    startGobblers(filename, p)
    val pid = Utils.getPID("vlc.exe", filename)
    Thread.sleep(targetDurationInMin * 60 * 1000)
    Runtime.getRuntime().exec("\"" + sendSignal + "\" " + pid)
    val endPoint = Calendar.getInstance.getTimeInMillis
    println("[" + filename + ", completed " + df.format(Calendar.getInstance.getTime) + ", for " + (endPoint - startPoint) / 60000 + " minutes]")

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

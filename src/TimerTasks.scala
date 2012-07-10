package nasko.avrecorder

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.TimerTask

class rtmpTimerTask(rtmpUrl: String, article: Article, sizePerMinute: Long, pageUrl: String, socks: String) extends TimerTask {

  def this(rtmpUrl: String, article: Article, sizePerMinute: Long) = this(rtmpUrl, article, sizePerMinute, "", "")

  def startGobblers(id: String, p: Process): Unit = {
    new Gobbler(id + ",STDOUT:", p.getInputStream, false).start
    new Gobbler(id + ",STDERROR:", p.getErrorStream, false).start
  }

  override def run() = {
    val rtmpdump = System.getProperty("rtmpdump")
    assert(rtmpdump != null && new File(rtmpdump).exists, "rtmpdump executable is not found")
    val sendSignal = System.getProperty("sendSignal")
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
    var done = false
    val file = new File(fullFileName)
    while (!done) {
      val whatIsLeft = targetSize - file.length
      if (whatIsLeft > 0) Thread.sleep(((whatIsLeft / sizePerMinute) + 1) * 60 * 1000)
      else done = true
    }
    val endPoint = Calendar.getInstance.getTimeInMillis
    // https://www-304.ibm.com/support/docview.wss?uid=swg21468390&wv=1
    val pid = Scheduler.getPID("rtmpdump.exe", filename)
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
    val sendSignal = System.getProperty("sendSignal")
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
    val pid = Scheduler.getPID("vlc.exe", filename)
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

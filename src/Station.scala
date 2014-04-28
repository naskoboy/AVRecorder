package nasko.avrecorder

import java.text.{SimpleDateFormat}
import java.util._
import scala.Predef._
import scala.List
import java.io.File

object Station

abstract class Station(val name: String, val folder: String, val timeZone: TimeZone, val timeAdvance: Int, val extraTime: Int, refreshRate: Int =24*60) extends Thread {

  def getRecorderTimerTask(article: Article): TimerTask

  val bgAlphabet = " 0123456789АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЪЮЯабвгдежзийклмнопрстуфхцчшщьъюя"
  val engAlphabet = Array(
    "_", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "A", "B", "V", "G", "D", "E", "J", "Z", "I", "J", "K", "L", "M", "N", "O", "P", "R", "S", "T", "U", "F", "H", "C", "CH", "SH", "ST", "A", "A", "IU", "IA",
    "a", "b", "v", "g", "d", "e", "j", "z", "i", "j", "k", "l", "m", "n", "o", "p", "r", "s", "t", "u", "f", "h", "c", "ch", "sh", "st", "a", "a", "iu", "ia"
  )

  var userDir = System.getProperty("my.user.dir")
  if (userDir==null) userDir = System.getProperty("user.dir")

  val subscriptionFile = userDir + "\\subscriptions.txt"
  assert(subscriptionFile!= null && new File(subscriptionFile).exists, "subscriptionFile is not found")

  val rootFolder = System.getProperty("rootFolder")
  assert(rootFolder!= null && new File(rootFolder).exists, "rootFolder is not found")

  val verboseProperty = System.getProperty("verbose")
  val verbose = verboseProperty!=null && verboseProperty.toBoolean

  val bgTimeZone  = TimeZone.getTimeZone("Europe/Sofia")

  def getFixedString(s: String) = {
    val sb = new StringBuffer()
    var prevC = '*'
    for (c <- s) {
      val index = bgAlphabet.indexOf(c)
      if (index >= 0) {
        var nc = engAlphabet(index)
        if (prevC == ' ') nc = nc.toUpperCase
        sb.append(nc)
      }
      else if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
        sb.append(c)
      else sb.append('_')
      prevC = c
    }
    sb.toString
  }

  def setTime(c: Calendar, hour: Int, minute: Int) = {
    val c2 = c.clone.asInstanceOf[Calendar]
    c2.set(Calendar.HOUR_OF_DAY, hour)
    c2.set(Calendar.MINUTE, minute)
    c2.set(Calendar.SECOND, 0)
    c2.set(Calendar.MILLISECOND, 0)
    c2
  }

  val dateFormatter = {
    val df = new SimpleDateFormat("MM-dd-yyyy HH:mm")
    df.setTimeZone(timeZone)
    df
  }

  def getArticles: List[Article]

  override def run {
    val dateFormatterLocal = new SimpleDateFormat("MM-dd-yyyy HH:mm")
    var point = Calendar.getInstance(bgTimeZone)
    var articles = List.empty[Article]
    while (true) try {
      val startMillis = point.getTimeInMillis

      val newArticles = try this.getArticles.filter(a => startMillis<=a.start.getTimeInMillis)
      catch { case _:Throwable  =>
        Scheduler.logger.error(name + " getArticle failed")
        List.empty[Article]
      }
      if (!newArticles.isEmpty) {
        val firstMillis = newArticles.head.start.getTimeInMillis
        articles = articles.filter(a => startMillis<=a.start.getTimeInMillis && a.start.getTimeInMillis<firstMillis) ++ newArticles
        Utils.fixArticleDurations(articles)
      }
      val nextPoint = point.clone().asInstanceOf[Calendar] ; nextPoint.add(Calendar.MINUTE, refreshRate)
      val candidateArticles = articles.filter(_.start.getTime.getTime < nextPoint.getTime.getTime)
      val subscriptions = scala.io.Source.fromFile(subscriptionFile, "utf-8").getLines.filter(_.startsWith(name)).toList

      Scheduler.logger.synchronized {
        if (verbose) articles.foreach(Scheduler.logger.info _)
        Scheduler.logger.info(name + " Wake up (delta " + dateFormatter.format(point.getTime) + " to " + dateFormatter.format(nextPoint.getTime) +  ") --- Articles ---")
        candidateArticles.foreach(a =>
          if (subscriptions.exists(a.pick(_))) {
            val adjusted_start = a.start.clone.asInstanceOf[Calendar]
            adjusted_start.add(Calendar.MINUTE, -a.station.timeAdvance)
            new Timer().schedule(a.station.getRecorderTimerTask(a), adjusted_start.getTime)
            Scheduler.logger.info(">>> " + dateFormatterLocal.format(adjusted_start.getTime) + " " + a)
          }
          else Scheduler.logger.info(a)
        )
        Scheduler.logger.info("\n\n")
      }

      point = nextPoint
      Thread.sleep(refreshRate*60*1000)
    } catch {
      case e:Throwable => println(e.getMessage)
    }
  }

}

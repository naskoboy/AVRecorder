package nasko.avrecorder

import java.text.DateFormat
import java.util.{Date, Calendar, TimerTask, TimeZone}

abstract class Station(val name: String, val folder: String, val timeZone: TimeZone, val timeAdvance: Int, val extraTime: Int) {
  def getRecorderTimerTask(article: Article): TimerTask

  val bgAlphabet = " 0123456789АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЪЮЯабвгдежзийклмнопрстуфхцчшщьъюя"
  val engAlphabet = Array(
    "_", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "A", "B", "V", "G", "D", "E", "J", "Z", "I", "J", "K", "L", "M", "N", "O", "P", "R", "S", "T", "U", "F", "H", "C", "CH", "SH", "ST", "A", "A", "IU", "IA",
    "a", "b", "v", "g", "d", "e", "j", "z", "i", "j", "k", "l", "m", "n", "o", "p", "r", "s", "t", "u", "f", "h", "c", "ch", "sh", "st", "a", "a", "iu", "ia"
  )

  def getFixedString(s: String) = {
    val sb = new StringBuffer();
    var prevC = '*'
    for (c <- s) {
      val index = bgAlphabet.indexOf(c);
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

  def newArticle(name: String, startHour: Int, startMinute: Int, duration: Int) = new Article(this, setTime(Calendar.getInstance(timeZone), startHour, startMinute), duration, name)

  def fixArticleDurations(l: List[Article]): Unit = {
    var h = l.head
    var t = l.tail
    do {
      h.duration = (t.head.start.getTime.getTime - h.start.getTime.getTime) / (1000 * 60)
      h = t.head
      t = t.tail
    } while (!t.isEmpty)
    h.duration = (Scheduler.nextDay(h.start).getTime.getTime - h.start.getTime.getTime) / (1000 * 60)
  }

  def dateFormatter(date: Date): String = {
    val df = DateFormat.getInstance
    df.setTimeZone(timeZone)
    df.format(date)
  }

}

package nasko.avrecorder

import java.util.Calendar

class Article(val station: Station, val start: Calendar, var duration: Long, val name: String) {

  def getStartText = station.dateFormatter(start.getTime)

  override def toString = station.name + ", " + station.dateFormatter(start.getTime) + ", " + duration + ", " + name

  override def equals(a: Any) = {
    val b = a.asInstanceOf[Article]
    this.station == b.station &&
      this.start == b.start &&
      this.duration == b.duration &&
      this.name == b.name
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
        case p(d, m, y, h, min, durationStr) =>
          val left = Calendar.getInstance(station.timeZone)
          left.set(y.toInt, m.toInt - 1, d.toInt, h.toInt, min.toInt, 0)
          left.set(Calendar.MILLISECOND, 0)
          val start_time = left.getTime.getTime
          val duration = if (durationStr == null) 1 else durationStr.substring(1).toInt
          println(duration)
          Some(start_time, start_time + duration * (1000 * 60))
        case _ => None
      }
    }
  }

  def pick(pattern: String): Boolean = {
    val p = """(\S+)\s(.*)""".r
    val p(stationName, pat) = pattern
    if (!station.name.equals(stationName)) false
    else {
      pat match {
        case ArticleLocalTimePeriod(left, right) => left <= start.getTime.getTime && start.getTime.getTime < right
        case ArticleName(namePattern) => namePattern.r.findFirstIn(name) != None
      }
    }
  }

}

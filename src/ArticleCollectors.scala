package nasko.avrecorder

import collection.mutable.ListBuffer
import java.util.Calendar
import xml.Node
import java.text.SimpleDateFormat

/**
 * Created with IntelliJ IDEA.
 * User: nasko
 * Date: 7/9/12
 * Time: 10:16 PM
 * To change this template use File | Settings | File Templates.
 */

object ArticleCollectors {

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
       val weekDays = Utils.findNodes(doc, (n: Node) => {
           val x = n.attribute("class")
           x!=None && x.get.toString.equals("week-day")
       })

       val articles = new ListBuffer[Article]
       for(day <- weekDays) {
         val date = Utils.findNodes(day, (n:Node) => { n.label.equals("span") }).head.text

         val Some(myMatch) = """\((\d{1,2})\s(\S*)\s(\d\d\d\d)\)(.*)""".r.findFirstMatchIn(date)
         val dayInt = myMatch.group(1).toInt
         val monthInt = monthMap(myMatch.group(2))
         val yearInt = myMatch.group(3).toInt

         val scheduleNode = Utils.findNodes(day, (n:Node) => { n.label.equals("dl") }).head
         val schedule = Utils.findNodes(scheduleNode, (n:Node) => {
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

 			val weekDay = Utils.findNodes(doc, (n: Node) => {
 			    val x = n.attribute("class")
 			    x!=None && x.get.toString.equals("top_articles_by_category program")
 			}).tail.head

 			var schedule = Utils.findNodes(weekDay, (n:Node) => { n.label.equals("tr") })
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
 				val aTags = Utils.findNodes(nameTag, (n:Node) => { n.label.equals("a") })
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


}

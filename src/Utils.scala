package nasko.avrecorder

import collection.mutable.ListBuffer
import scala.Predef._
import xml.Node
import java.util.{TimeZone, Calendar}

/**
 * Created with IntelliJ IDEA.
 * User: nasko
 * Date: 7/9/12
 * Time: 10:17 PM
 * To change this template use File | Settings | File Templates.
 */

object Utils {

  def findNodes(node: Node, interesting: Node => Boolean): List[Node] = {
      val res = new ListBuffer[Node]
      if (interesting(node)) res += node
      for (n <- node.child) res ++= findNodes(n, interesting)
      res.toList
    }

    def printDoc(node: Node): Unit = {
      println("Label: " + node.label)
      println("Text: " + node.text)
      node.attributes.foreach(println _)
      for (n <- node.child) printDoc(n)
    }

  def getPID(appName:String, filename:String) = scala.io.Source.fromInputStream(Runtime.getRuntime()
        .exec("wmic PROCESS WHERE \"Caption='" + appName + "' AND CommandLine like '%" + filename + "%'\" GET ProcessId /FORMAT:list")
        .getInputStream).getLines.filter(it => it.indexOf("ProcessId")>=0).next.substring(10)



  def fixArticleDurations(l: List[Article]): Unit = {
    var h = l.head
    var t = l.tail
    do {
      h.duration = (t.head.start.getTime.getTime - h.start.getTime.getTime) / (1000 * 60)
      h = t.head
      t = t.tail
    } while (!t.isEmpty)
    h.duration = 120
  }

  def toCommand(params: List[String]) =
    (for (it <- params) yield if (it.indexOf(' ') > -1) "\"" + it + "\"" else it).mkString(" ")

}

class timeGenerator(timezone: TimeZone, year: Int, month: Int, day: Int) {
  var currentHour=0
  var earlyMorning=false

  def next(hour: Int, minute: Int) = {
    if (!earlyMorning && hour<currentHour) earlyMorning=true
    currentHour=hour
    val moment = Calendar.getInstance(timezone)
    moment.set(year, month, day, hour, minute, 0)
    moment.set(Calendar.MILLISECOND, 0)
    if (earlyMorning) moment.add(Calendar.DATE, 1)
    moment
  }
}



//D:\temp>wmic PROCESS WHERE "CommandLine like '%a%' and not CommandLine like '%wmic%' " GET ProcessId,CommandLine /FORMAT :list:"sortby=PID"
//wmic path Win32_LocalTime Get Day^,Hour^,Minute^,Month^,Second^,Year /Format:table
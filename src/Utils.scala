package nasko.avrecorder

import collection.mutable.ListBuffer
import scala.Predef._
import xml.Node

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

}

//D:\temp>wmic PROCESS WHERE "CommandLine like '%a%' and not CommandLine like '%wmic%' " GET ProcessId,CommandLine /FORMAT :list:"sortby=PID"
//wmic path Win32_LocalTime Get Day^,Hour^,Minute^,Month^,Second^,Year /Format:table
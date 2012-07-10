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


}

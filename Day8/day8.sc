import scala.collection.mutable.ListBuffer
import scala.io.Source
class Tree {
  var globalNodeCounter = 1
  var top = Node()
  case class Node(var metadata: ListBuffer[Int] = ListBuffer.empty, var children: ListBuffer[Node] = ListBuffer.empty)

  def parseIntoTree(str: String): Unit = {
//    val toInt = str.toList.filterNot(_ == ' ').map(_.toInt)
    val noSpaces = str.split(' ').toList.map(_.toInt)
//    print(s"AHHHHH noSpaces = $noSpaces")
    parseIntoTreeHelper(noSpaces)
  }

  def parseIntoTreeHelper(input: List[Int]): Unit = {
//    println(s" passed: ${input}")
    val children = input.head
    val metadata = input(1)
    val metadataEntries = ListBuffer(input.splitAt(input.length-metadata)._2).flatten
    val newInput = input.splitAt(input.length-metadata)._1.splitAt(2)._2
    top.metadata = metadataEntries
    top.children = parseIntoChildrenHelper(newInput, children)._1
//    println(s"there were $metadata metadata entries and $children children entries. input snippet: ${input.splitAt(10)._1}")
  }

  def parseIntoChildrenHelper(input: List[Int], count: Int): (ListBuffer[Node], List[Int]) = {
//    println(s"input recieved = $input")
    var newInput = input
    var list:ListBuffer[Node] = ListBuffer.empty
    for (i <- 0 until count) {
      globalNodeCounter += 1
//      println(s"times executed: $globalNodeCounter asdf input snippet: ${newInput.splitAt(10)._1}")
      val pair = parseIntoChildren(newInput)
//      println(s"adding ${pair._1} to list")
      newInput = pair._2
      list += pair._1
    }

    (list, newInput)
  }

  def parseIntoChildren(input: List[Int]): (Node, List[Int]) = {
    val children = input.head
    val metadata = input(1)
    println(s"Removing... ${input.splitAt(2)._1} ${ListBuffer(input.splitAt(input.length-metadata)._2).flatten} $metadata metadata entries, $children children. input snippet: front ${input.splitAt(5)._1} back ${input.splitAt(input.size - 10)._2}. input size: ${input.length}")
    val metadataEntries = ListBuffer(input.splitAt(input.length-metadata)._2).flatten
    val newInput = input.splitAt(input.length-metadata)._1.splitAt(2)._2
    if (children == 0)
      (Node(metadataEntries), newInput)
    else {
      val o = parseIntoChildrenHelper(newInput, children)
      val n = Node(metadataEntries, o._1)
      (n , o._2)
    }
  }

  def printDis(): Unit = {
     halpMePrintDis(top)
  }

  def halpMePrintDis(node: Node): Unit = {
    println(s"Node has ${node.children.length} children. Node has ${node.metadata.length} metadata values. They are: ${node.metadata}")
    node.children.foreach(halpMePrintDis)
  }
}

val lines = Source.fromFile("/Users/cang.truong/Documents/docs/AdventofCode/Day8/in").getLines.toList.head
val tree = new Tree
tree.parseIntoTree(lines)
tree.printDis()
import scala.collection.mutable.ListBuffer
import scala.io.Source
class Tree {
  var globalNodeCounter = 1
  var top = Node()
  case class Node(var metadata: List[Int] = List.empty, var children: ListBuffer[Node] = ListBuffer.empty)

  def parseIntoTree(str: String): Unit = {
//    val toInt = str.toList.filterNot(_ == ' ').map(_.toInt)
    val noSpaces = str.split(' ').toList.map(_.toInt)
//    print(s"AHHHHH noSpaces = $noSpaces")
//    parseIntoTreeHelper(noSpaces)
    top = parseIntoTreeAlt(noSpaces)._1
  }

  def parseSampleTree(): Unit = {
    parseIntoTree("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
  }

  def parseIntoTreeAlt(input: List[Int]): (Node, List[Int]) = {
    var newInput = input
//    println(newInput)

    val children = newInput.head
    val metadata = newInput(1)
    var childrenz: ListBuffer[Node] = ListBuffer.empty
    newInput = input.splitAt(2)._2
    for (i <- 0 until children) {
      val x = parseIntoTreeAlt(newInput)
      newInput = x._2
      childrenz += x._1
    }
    val metadataList = newInput.splitAt(metadata)._1
    newInput = newInput.splitAt(metadata)._2
    (Node(metadataList, childrenz), newInput)
  }

  def printDis(): Unit = {
     halpMePrintDis(top)
  }

  def printMetadata(): Int = {
    halpMePrintMetadata(top)
  }

  def halpMePrintMetadata(node: Node): Int = {
    var tot = 0
    node.metadata.foreach(tot += _)
    node.children.foreach(tot += halpMePrintMetadata(_))
    tot
  }

  def halpMePrintDis(node: Node): Unit = {
    println(s"Node has ${node.children.length} children. Node has ${node.metadata.length} metadata values. They are: ${node.metadata}")
    node.children.foreach(halpMePrintDis)
  }

  def ffffffffffff(node: Node): Int = {
    var tot = 0
    println(s"metadata = ${node.metadata} # of children = ${node.children.length}")
    node.metadata.foreach{i =>
      if (node.children.isEmpty)
        tot += i
      else if (i <= node.children.length)
        tot += ffffffffffff(node.children(i-1))
    }
    tot
  }

  def ffffff(): Int = {
    ffffffffffff(top)
  }
}

val lines = Source.fromFile("/Users/cang.truong/Documents/docs/AdventofCode/Day8/in").getLines.toList.head
val tree = new Tree
tree.parseIntoTree(lines)
//tree.parseSampleTree()
//tree.printDis()
println(tree.ffffff())
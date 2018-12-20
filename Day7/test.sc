import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class Nodes {

  private var nodes: ListBuffer[Node] = ListBuffer.empty
  private var topNodes: ListBuffer[Node] = ListBuffer.empty
  private var finalString = ""
  private var dummyNode = Node(".")

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  case class Node(name: String, var connections: ListBuffer[Node] = ListBuffer.empty) {
    def add (name: String, nodes: ListBuffer[Node]): Unit = {
      connections += nodes.find{n:Node => n.name == name}.getOrElse{
        println(s"adding node of name:$name")
        val addMe = Node(name)
        nodes += addMe
        addMe
      }
    }
    def connectedTo (node: Node): Boolean = {
      connections.contains(node)
    }
  }

  def += (value: String, connection: String): Unit = {
    if (nodes.exists { n: Node =>
      n.name == value
    }) {
      println(s"already found a node of name $value")
      nodes.find { n: Node => n.name == value }.get.add(connection, nodes)
    }
    else {
      println(s"creating a new node $value")
      nodes += Node(value, ListBuffer(Node(connection)))
    }
  }

  def printNonTree(): Unit = {
    nodes.foreach{ n:Node =>
      println(s"Node ${n.name} is connected to ${n.connections.flatMap(_.name)}")
    }
  }

  def findTop(): ListBuffer[Node] = {
    print(nodes)
    val filtered = nodes.filter { n:Node =>
      val connections = nodes.flatMap(_.connections)
      !connections.exists(_.name == n.name)
    }
    print(filtered)
    filtered
  }

  def setTop(): Unit = {
    topNodes = findTop()
  }

  def getTops(): ListBuffer[Node] = {
    if (topNodes.isEmpty)
      setTop()
    topNodes
  }


  var workers = List((0,dummyNode),(0,dummyNode),(0,dummyNode),(0,dummyNode),(0,dummyNode))

  def findConnections(n:Node): Boolean = {
    val na = !nodes.exists{nx => nx.connections.exists(_.name == n.name)}
    val nb = !workers.exists{nx => nx._2.connections.exists(_.name == n.name)}
    println(s"\nfor node ${n.name} node connections were $na and worker connections were $nb")
    na && nb
  }

  def findWorkerConnections(n:Node): Boolean = {
    //    println(s"\n pre-filtered node: ${n.name}")
    //    println(s"workers node connections are ${workers.flatMap{p => p._2.connections.flatMap(_.name)}}")
    !workers.exists{nx => nx._2.connections.exists(_.name == n.name)}
  }

  def assignWork(n:Node): Boolean = {
    val ascii = 'A' - 1 //this is A - 1 + 60 seconds
    if (workers.exists{p => p._1 == 0}) {
      var cons = true
      workers = workers.map{ i =>
        if (i._1 == 0 && cons) {
          cons = false
          (n.name.charAt(0).toInt - ascii + 60, n)
        } else (i._1, i._2)
      }
      true
    } else {
      false
    }
  }

  def assignWorkers(li: ListBuffer[Node]): Unit = {
    li.foreach{ n =>
      nodes = nodes.filterNot(_.name == n.name)
      finalString += n.name
      assignWork(n)
    }
  }

  def decWorkers(): Unit = {
    workers = workers.map{ i =>
      val p = if (i._1 > 0) (i._1 - 1, i._2) else (i._1, i._2)
      if (p._1 == 0) (p._1, dummyNode) else p
    }
  }

  def printTreeOther(): Int = {
    sortAll()
    val find: Node => Boolean = findWorkerConnections
    var counter = 0
    while (nodes.nonEmpty) {
      counter += 1
      val filtered = findTop().filter(find)
      println(s"\n filtered nodes are: ${filtered.map{n => n.name}}")
      if(counter > 9999)
        nodes = ListBuffer.empty
      if (filtered.nonEmpty) assignWorkers(filtered)
      decWorkers()
      println(s"\ncount:$counter ${workers.map{x => (x._1, x._2.name)}}")
    }
    println(finalString)
    counter + workers.maxBy(_._1)._1
  }

  def sortAll(): Unit = {
    nodes.foreach{n:Node =>
      n.connections = n.connections.sortBy(_.name)
    }
    nodes = nodes.sortBy(_.name)
  }

  def printPair(s1: String, s2: String): Unit = {
    print(s"$s1 ----> $s2")
  }

  def printOrder(): String = {
    finalString
  }
}


val lines = Source.fromFile("/Users/cang.truong/Documents/docs/AdventofCode/Day7/in2").getLines.toList
var nodes:Nodes = new Nodes
var parsedLines = lines.map{l: String =>
  val t = l.split("Step ")(1)
  val x = t.split(" must be finished before step ")
  val first = x(0)
  val second = x(1).split(" can begin.")(0)
  (first, second)
}

parsedLines.foreach{ p => nodes += (p._1,p._2) }

nodes.sortAll()
nodes.printNonTree()
println(nodes.printTreeOther())
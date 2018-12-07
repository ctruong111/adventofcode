import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

var rGrid = ListBuffer[ListBuffer[Cell]]()
var rx = 0
var ry = 0

case class Cell(x: Int,
                y: Int,
                var valid: Boolean = true,
                var size: Int = -1,
                var closestDist: Int = 99999999,
                var closestCell: Char = 0.toChar,
                cellName: Char = 0.toChar) {
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case c: Cell => if (c.x == this.x && c.y == this.y) true else false
      case _ => false
    }
  }
}

def draw(cells: List[Cell]): Unit = {
  val xMax = cells.maxBy(_.x).x + 1
  val yMax = cells.maxBy(_.y).y + 1

  (0 to xMax).foreach{ i: Int =>
    (0 to yMax).foreach{ j: Int =>
      val x = Cell(i, j)
      print (s"${if (cells.contains(x)) cells.find(_ == x).get.cellName else '.'} ")
    }
    println()
  }
}

def otherDraw(grid: ListBuffer[ListBuffer[Cell]]): Unit ={
  grid.foreach{l: ListBuffer[Cell] => l.foreach{c: Cell =>
      print(s"${c.cellName} ")
    }
    println()
  }
}

def createGrid(cells: List[Cell]): ListBuffer[ListBuffer[Cell]] = {
  val xMax = cells.maxBy(_.x).x
  val yMax = cells.maxBy(_.y).y

  var grid = new ListBuffer[ListBuffer[Cell]]()

  (0 to xMax).foreach{ i: Int =>
    var temp = new ListBuffer[Cell]
    (0 to yMax).foreach{ j: Int =>
      val x = Cell(i, j, cellName = '.')
      if (cells.contains(x))
        temp += cells.find(_ == x).get
      else
        temp += x
    }
    grid += temp
  }
  grid
}

def manhattanDistance(c1: Cell, c2: Cell): Int = {
  Math.abs(c1.x-c2.x) + Math.abs(c1.y-c2.y)
}

def fill(cells: List[Cell]): Unit = {
  cells.foreach{ cell: Cell =>

  }
}

def cellHelper(cell: Cell, x: Int, y: Int, dist: Int): Unit = {
  if (rGrid(cell.x)(cell.y).closestDist < dist) {
    rGrid(cell.x)(cell.y).closestDist = dist
    rGrid(cell.x)(cell.y).closestCell = cell.cellName
  }
}

val lines = Source.fromFile("/Users/cang.truong/Documents/docs/AdventofCode/Day6/in").getLines.toList
var cellName = 64.toChar
val cells = lines.map{l:String =>
  val coords = l.split(", ")
  cellName = (cellName.toInt + 1).toChar
  Cell(coords(0).toInt, coords(1).toInt, cellName = cellName)
}
  .sortBy(_.x)
  .sortBy(_.y)
rGrid = createGrid(cells)
rx = rGrid.head.length
ry = rGrid.length
println(s"Grid dimensions: x:$rx, y:$ry")
otherDraw(rGrid)
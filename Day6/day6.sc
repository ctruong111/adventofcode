import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

val file = new File("/Users/cang.truong/Documents/docs/AdventofCode/Day6/out.txt")
val bw = new BufferedWriter(new FileWriter(file))

case class Cell(x: Int,
                y: Int,
                var valid: Boolean = true,
                var size: Int = 0,
                var closestDist: Int = 99999999,
                var closestCell: Char = 0.toChar,
                cellName: Char = 0.toChar,
                input: Boolean = false) {
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case c: Cell => if (c.x == this.x && c.y == this.y) true else false
      case _ => false
    }
  }

  def ++(): Unit = {
    size = size + 1
  }

  def setFalse(): Unit = {
    valid = false
  }
}

var rGrid = ListBuffer[ListBuffer[Cell]]()
var rx = 0
var ry = 0
val maxDist = 10000

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

def otherDrawClosest(grid: ListBuffer[ListBuffer[Cell]]): Unit ={
  grid.foreach{l: ListBuffer[Cell] => l.foreach{c: Cell =>
    print(s"${c.closestCell} ")
  }
    println()
  }
}

def otherDrawDist(grid: ListBuffer[ListBuffer[Cell]]): Unit ={
  grid.foreach{l: ListBuffer[Cell] => l.foreach{c: Cell =>
    print(s"${c.closestDist} ")
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

def compareCell(cell: Cell, x: Int, y: Int): Boolean = {
  val manhattanDist = manhattanDistance(cell, Cell(x, y))
  bw.write(s"cell: ${cell.cellName} x:$x y:$y dist:$manhattanDist compDist=${rGrid(x)(y).closestDist} val=${rGrid(x)(y).closestCell}")
  //already explored
  if (rGrid(x)(y).cellName == cell.cellName || rGrid(x)(y).closestCell == cell.cellName) {
    bw.write(s" the names were equal? ${cell.cellName} \n")
    false
  }
  else if (rGrid(x)(y).input){
    bw.write(s" this cell was input \n")
    true
  }
  else if (rGrid(x)(y).closestDist == manhattanDist && rGrid(x)(y).closestCell != cell.cellName) {
    bw.write(s" distance was equal: $manhattanDist \n")
    rGrid(x)(y).closestCell = '.'
    false
  }
  else if (manhattanDist < rGrid(x)(y).closestDist) {
    bw.write(s" distance was closer... dist: $manhattanDist")
    rGrid(x)(y).closestDist = manhattanDist
    rGrid(x)(y).closestCell = cell.cellName
    bw.write(s" new value should be... ${rGrid(x)(y).closestCell} and new dist should be... ${rGrid(x)(y).closestDist} \n")
    true
  }
  else {
    bw.write(s"somehow i got here? \n")
    false
  }
}

def fillGrid(cell: Cell, x: Int, y: Int): Unit = {
//  if (manhattanDistance(Cell(0,0), Cell(rx, ry)) > dist) {
    if (x-1 > 0 && compareCell(cell, x - 1, y)) {
      fillGrid(cell, x - 1, y)
    }
    if (x+1 < rx && compareCell(cell, x + 1, y)) {
      fillGrid(cell, x + 1, y)
    }
    if (y-1 > 0 && compareCell(cell, x, y - 1)) {
      fillGrid(cell, x, y - 1)
    }
    if (y+1 > ry && compareCell(cell, x, y + 1)) {
      fillGrid(cell, x, y + 1)
    }
//  }

}

def fillOnGrid(cells: List[Cell]): Unit = {
  rGrid.foreach{_.foreach{c:Cell =>
    val sortedCells = cells.sortBy{ci:Cell => manhattanDistance(c, ci)}
    if (manhattanDistance(sortedCells(0), c) == manhattanDistance(sortedCells(1), c)) {
    c.closestCell = '.'
    c.closestDist = -1
  } else {
    val closestCell = sortedCells.head
    c.closestCell = closestCell.cellName
    c.closestDist = manhattanDistance(c, closestCell)
  }

  }}
}

def fillOnGridAlt(cells: List[Cell]): Unit = {
  rGrid.foreach{_.foreach
  {c:Cell =>
      var dist = 0
      cells.foreach{x:Cell =>
        dist = dist + manhattanDistance(x, c)
//        bw.write(s"for cell: ${x.cellName}, the distance was ${manhattanDistance(x, c)}. The running total is $dist\n")
      }
//    bw.write(s"the total distance is: $dist for cell (${c.x}, ${c.y})\n")
      if (dist > maxDist - 1) {
        c.closestCell = '.'
        c.closestDist = -1
      } else {
        c.closestCell = 'Y'
        c.closestDist = dist
      }
    }
  }
}

def fill(cells: List[Cell]): Unit = {
//  cells.foreach{ cell: Cell =>
//    fillGrid(cell, cell.x, cell.y)
//  }
  fillGrid(cells.head, cells.head.x, cells.head.y)
}

def assignSize(cells: List[Cell]): Unit = {
  rGrid.foreach{_.foreach{ c: Cell =>
    if (c.x == 0 || c.x >= rx-1 || c.y == 0 || c.y >= ry-1) {
      cells.find { e: Cell => e.cellName == c.closestCell || e.cellName == c.cellName }.getOrElse(Cell(0,0)).setFalse()
      bw.write(s"Cell was NO... ${c.closestCell} at pos (${c.x}, ${c.y})\n")
    } else {
      bw.write(s"Cell was OK... ${c.closestCell} at pos (${c.x}, ${c.y})\n size is now ${cells.find { e: Cell => e.cellName == c.closestCell || e.cellName == c.cellName }.getOrElse(Cell(0,0)).size + 1}")
      cells.find { e: Cell => e.cellName == c.closestCell || e.cellName == c.cellName }.getOrElse(Cell(0, 0)) ++
    }
  }}
}

val lines = Source.fromFile("/Users/cang.truong/Documents/docs/AdventofCode/Day6/in").getLines.toList
var cellName = 64.toChar
val cells = lines.map{l:String =>
  val coords = l.split(", ")
  cellName = (cellName.toInt + 1).toChar
  Cell(coords(0).toInt, coords(1).toInt, cellName = cellName, input = true)
}
  .sortBy(_.x)
  .sortBy(_.y)
rGrid = createGrid(cells)
ry = rGrid.head.length
rx = rGrid.length
print(s"Cell Names: ${cells.flatMap{c:Cell => s" ${c.cellName}"}}")
//println(s"Grid dimensions: x:$rx, y:$ry")
//otherDraw(rGrid)
fillOnGrid(cells)
//fillOnGridAlt(cells)
println("----------------------------------")
otherDrawClosest(rGrid)
//otherDrawDist(rGrid)
println("----------------------------------")
assignSize(cells)
println(cells.filter(_.valid))
val largestArea = cells.filter(_.valid).maxBy(_.size)
print(s"Largest valid area: ${largestArea.size}, cell name is: ${largestArea.cellName}")
bw.close()

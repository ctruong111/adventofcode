import scala.collection.mutable.ListBuffer
import scala.io.Source

val lines = Source.fromFile("/Users/cang.truong/Documents/docs/AdventofCode/Day1/in").getLines.toList
val ints = lines.map(_.toInt)
var list:ListBuffer[Int] = ListBuffer.empty
var counter = 0
var freq = 0
var found = true

while(found) {
  ints.foreach { i =>
    freq += i
    counter += 1
    print(s" $freq ")
//    if (counter > 99999)
//      found = false
    if (list.contains(freq)) {
      println(s"\nf $freq")
      found = false
    }
    list += freq
  }
}
println(freq)
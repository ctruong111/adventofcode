import scala.annotation.tailrec
import scala.io.Source

val lines = Source.fromFile("/Users/cang.truong/Documents/docs/AdventofCode/Day5/in").getLines.toList.head
println(s"Initial length: ${lines.length}")

def boomed(c1: Char, c2: Char): String = {
  if (c1.toLower == c2 && c1.toLower != c1)
    ""
  else if (c1.toUpper == c2 && c1.toUpper != c1)
    ""
  else
    s"$c1$c2"
}

def boom(s: String, c: Char): String = {
  println(s"pre-booming length = ${s.length}")
  val x = s.filterNot{f:Char => f.toLower.equals(c) || f.toUpper.equals(c)}
  println(s"post-booming length = ${x.length}")
  x
}

@tailrec
def boomIt(s: String, done: String): String = {
//  println(s"before: done=$done s=$s")
  if (s.length == 2) {
//    println(s"1: $done${boomed(s(0), s(1))}")
    s"$done${boomed(s(0), s(1))}"
  } else if (s.length > 2){
    if (boomed(s(0), s(1)).isEmpty) {
//      println(s"2: $done${s.substring(2)}")
      boomIt(s"${s.substring(2)}", s"$done")
    } else if (boomed(s(1), s(2)).isEmpty) {
//      println(s"3: $done${s(0)}${s.substring(3)}")
      boomIt(s"${s(0)}${s.substring(3)}", done)
    } else {
//      println(s"4: $done${s.substring(0, 1)}${s.substring(2)}")
      boomIt(s"${s.substring(2)}", s"$done${s.substring(0, 2)}")
    }
  } else {
//    println(s"5: $done$s")
    s"$done$s"
  }

}
def tryBoomingIt(s: String, c: Char): String = {
  var bigBoom = boom(s, c)
  var trying = 0
  while (trying < 2) {
    val v1 = bigBoom.length
    bigBoom = boomIt(bigBoom, "")
    if (v1 != bigBoom.length) trying = 0
    else trying += 1
  }
  bigBoom
}

val line = "dabAcCaCBAcCcaDA"
val biggest = ('a' to 'z').map{c: Char => val v = tryBoomingIt(lines, c).length
  println(s"trying $c: $v")
  v
}.min
println(s"smallest is: $biggest")
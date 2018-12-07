import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import scala.collection.mutable
import scala.io.Source

val lines = Source.fromFile("/Users/cang.truong/Documents/docs/adventofcodeday4input").getLines.toList

  .sortBy{s: String =>
//    println(s.split(" ")(1).substring(0, (s.split(" ")(1)).length-1))
    s.split(" ")(1).substring(0, (s.split(" ")(1)).length-1)}
  .sortBy(_.split(" ")(0).substring(1))
val militaryTime = new SimpleDateFormat("yyyy-MM-dd HH:mm")
var currentGuard = "-1"
var guardSchedules: mutable.Map[String, List[(Date, Date)]] = scala.collection.mutable.Map.empty[String, List[(Date, Date)]]
var guardTotalSleepTime: mutable.Map[String, Long] = scala.collection.mutable.Map.empty[String, Long]
var asleepTime = militaryTime.parse("0000-00-00 00:00")
lines.foreach{line: String =>
  val splitString = line.split(" ").toList
  val time = militaryTime.parse(s"${splitString.head.substring(1)} ${splitString(1).substring(0, (splitString(1):String).length-1)}")
  val foundGuard = splitString.contains("Guard")
  if (foundGuard)
    currentGuard = splitString(3).substring(1)
  else if (splitString.contains("falls")) {
    asleepTime = time
  }
  else {
    guardSchedules += (currentGuard ->
      ((if (guardSchedules.contains(currentGuard)) guardSchedules(currentGuard) else List.empty[(Date, Date)]) ::: List((asleepTime, time)))
    )
    guardTotalSleepTime += (currentGuard -> ((if (guardTotalSleepTime.contains(currentGuard)) guardTotalSleepTime(currentGuard) else 0) + ((time.getTime - asleepTime.getTime) / 60000)))
  }
}
val sleepiestGuard = guardTotalSleepTime.maxBy(_._2)._1
val timeSleeping = guardTotalSleepTime.maxBy(_._2)._2
val schedules = guardSchedules(sleepiestGuard)
println(s"guard that slept the most: $sleepiestGuard\n " +
  s"total time slept by that guard $timeSleeping minutes \n " +
  s"sleep time ranges for that guard $schedules")
//guardTotalSleepTime.foreach{kv: (String, Long) => println(s"Guard #${kv._1} was asleep for ${kv._2} minutes")}

def formatSchedules(map: mutable.Map[String, List[(Date, Date)]]) = {
  map.map{kv:(String,List[(Date, Date)]) =>
    val v = kv._2.map{p:(Date, Date) =>
      val cal = Calendar.getInstance()

      cal.setTime(p._1)
      val t1hour = cal.get(Calendar.HOUR_OF_DAY)
      val t1min = cal.get(Calendar.MINUTE)

      cal.setTime(p._2)
      val t2min = cal.get(Calendar.MINUTE)
      val t2hour = cal.get(Calendar.HOUR_OF_DAY)
      (s"$t1hour:$t1min", s"$t2hour:$t2min")}
    kv._1 -> v
  }
}
val schedFormatted = formatSchedules(guardSchedules)

def getTimesForOneGuard(l: List[(Date, Date)]) = {
  var times = scala.collection.mutable.Map.empty[String, Int]
  l.foreach{kv: (Date, Date) =>
    val cal = Calendar.getInstance()
    cal.setTime(kv._1)
    //  println(kv._1)
    val t1h = cal.get(Calendar.HOUR_OF_DAY)
    val t1m = cal.get(Calendar.MINUTE) + (t1h * 60)
    //  println(t1h)
    //  println(s"${t1m/60}:${t1m%60}")
    cal.setTime(kv._2)
    val t2h = cal.get(Calendar.HOUR_OF_DAY)
    val t2m = cal.get(Calendar.MINUTE) + (t2h * 60)

    for (i <- t1m to t2m) times += (s"${i/60}:${i%60}" -> (times.getOrElse(s"${i/60}:${i%60}", 0) + 1))
  }
  times
}

val things = getTimesForOneGuard(guardSchedules(sleepiestGuard))
val minRes = things.maxBy(_._2)._1.substring(2).toInt
println(s"the best time to go is...: ${things.maxBy(_._2)._1} with guard $sleepiestGuard. the guard slept ${things.maxBy(_._2)._2} times!!! with result ${minRes * sleepiestGuard.toInt}")

val schedWMinutes = guardSchedules.map{kv: (String, List[(Date, Date)]) => (kv._1 -> getTimesForOneGuard(kv._2))}
schedWMinutes.foreach{kv:(String, mutable.Map[String, Int]) =>
  println(s"Guard #${kv._1} sleeping on... ")
  kv._2.toList.sortBy(_._2).foreach{ kv: (String, Int) =>
    println(s"minute: ${kv._1} times slept ${kv._2}")
  }
}
val sortedS = schedWMinutes.toList.sortBy(_._2.maxBy(_._2)._2)
//sortedS.foreach{kv:(String, mutable.Map[String, Int]) => println(s"guard #${kv._1} slept ${kv._2.maxBy(_._2)._2} times at minute ${kv._2.maxBy(_._2)._1}")}
//  schedWMinutes.maxBy{kv:(String, mutable.Map[String, Int]) => kv._2.maxBy(_._2)._2}
val answer = sortedS.reverse.head
println(s"sleepiest guard is... ${answer._1}. The guard's sleepiest time is: ${answer._2.maxBy(_._2)._1} and the guard slept ${answer._2.maxBy(_._2)._2} times \n " +
  s"the resulting answer is: ${answer._1.toInt * answer._2.maxBy(_._2)._1.substring(2).toInt}")
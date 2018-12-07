import java.text.SimpleDateFormat

import scala.io.Source
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object adventOfCodeDay4Main {
    def main(args: Array[String]): Unit = {

    }

    def readFromFile(filename: String): List[List[String, String, String, String]] = {
      val lines = Source.fromFile(filename).getLines.toList
        .sortBy(_.split(" ")(0).toString().substring(1))
        .sortBy(_.split(" ")(1).toString().substring(0, (splitString(1):String).length-2))
      val militaryTime = new SimpleDateFormat("HH:mm")
      var currentGuard = "-1"
      var guardSchedules: Map[String, (Date, Date)] = scala.collection.mutable.Map.empty[String, (Date, Date)]
      var guardTotalSleepTime: Map[String, Int] = scala.collection.mutable.Map.empty[String, Int]
      var asleepTime = militaryTime.parse("00:00")
      lines.foreach{line: String =>
        val splitString = line.split(" ").toList
        val time = militaryTime.parse((splitString(1):String).substring(0, (splitString(1):String).length-2))
        val foundGuard = splitString.contains("Guard")
        if (foundGuard)
          currentGuard = splitString(3).toString().substring(1)
        else if (splitString(3) == "falls")
          asleepTime = time
        else {
          guardSchedules += (currentGuard -> (asleepTime, time))
          guardTotalSleepTime += guardTotalSleepTime(currentGuard) + ((time.getTime - asleepTime.getTime) * 60000)
        }
      }
      val sleepiestGuard = guardTotalSleepTime.max._1
      val timeSleeping = guardTotalSleepTime.max._2
      val schedules = guardSchedules(sleepiestGuard)
      println(s"guard that slept the most: ${sleepiestGuard}\n " +
        s"total time slept by that guard ${timeSleeping} \n " +
        s"sleep time ranges for that guard ${schedules}")
    }
}
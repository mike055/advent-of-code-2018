package advent.advent01

import scala.Option
import scala.collection.immutable
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {

    args match {
      case Array(filepath) =>

        println(filepath)

        val frequencies: Array[Int] =
          Source.fromFile(filepath).getLines().mkString("\n").split("\n").map(n => n.toInt)
        println(frequencies.mkString(" "))

        /////// Part #1
        val totalFrequencyResult: Int = frequencies.foldLeft(0)(_ + _)
        println("-------PART 1-------")
        println(totalFrequencyResult)
        println("-------PART 1-------")
        //////

        /// Part #2

//        def findDuplicates(list:List[Int]): Option[Int] = {
//          //list.groupBy(crit) filter {case (_,l) => l.size > 1 } keys
//
//          val dupes: immutable.Iterable[Int] = list.groupBy(identity).collect { case (x,ys) if ys.lengthCompare(1) > 0 => x }
//
//          println(dupes.mkString(" "))
//
//          for (l <- list) {
//
//            dupes.find(d => d == l) match {
//              case Some(n) => {
//                return Some(n)
//              }
//              case None => None
//            }
//          }
//
//          return None
//
//        }
//
//        def getFirstDuplicate(frequencies: Array[Int], accumFrequencyTotals: Array[Int]= Array[Int](), loopNo: Int = 1): Unit = {
//
//
//
//
//
//          var allTotals: Array[Int] = Array[Int]()
//
//
//
//
//          for (f <- frequencies) {
//            val totalFreq = f + allTotals.last
//
//            if (allTotals.contains(totalFreq)) {
//              something
//            }
//            else {
//              allTotals = allTotals :+ totalFreq
//            }
//          }
//
//          frequencies.map(f)
//
//
//
//
//
//          println("loop number: " + loopNo)
//          //println("accum array length total:" + accumFrequencyTotals.length)
//
//          val startingPoint: Int =
//            accumFrequencyTotals.length match {
//              case 0 => 0
//              case _ => accumFrequencyTotals.last
//            }
//
//          //println("starting point: " + startingPoint)
//
//          val result: Array[Int] = frequencies.scanLeft(startingPoint) { case (acc, item) => acc + item }
//          val filteredResult: Array[Int] = result.drop(1)
//
//          val newAccumFrequencyTotals: Array[Int] = accumFrequencyTotals ++ filteredResult
//
//          val allDuplicates: Option[Int] = findDuplicates(newAccumFrequencyTotals.toList)
//
//          allDuplicates match {
//            case None => getFirstDuplicate(frequencies, newAccumFrequencyTotals, loopNo + 1)
//            case Some(n) => {
//
//              println(n)
//
//            }
//          }
//        }


        case class ProcessedFrequency(currentFrequencyIndex: Int, newTotalFrequency: Int, isDuplicate: Boolean, accumulatedFrequencyTotals: Array[Int])

        def processFrequency(frequencies: Array[Int], currentFrequencyIndex: Int, accumlatedFrequencyTotals: Array[Int]): ProcessedFrequency = {

          //println("index: " + currentFrequencyIndex)

          val nextFrequency = {
            accumlatedFrequencyTotals.length > 0 match {
              case true => accumlatedFrequencyTotals.last + frequencies (currentFrequencyIndex)
              case false => frequencies (currentFrequencyIndex)
            }
          }
          val isDuplicate = accumlatedFrequencyTotals.contains(nextFrequency)

          return ProcessedFrequency(currentFrequencyIndex, nextFrequency, isDuplicate, accumlatedFrequencyTotals)
        }


        def processor(currentFrequencyIndex: Int, accumlatedFrequencyTotals: Array[Int], repeatsOfFrequencies: Int): Int = {
          println("repeats of frequency: " + repeatsOfFrequencies)

          val lastProcessedFrequency: ProcessedFrequency = processFrequency(frequencies, currentFrequencyIndex, accumlatedFrequencyTotals)
          if (lastProcessedFrequency.isDuplicate) {
            return lastProcessedFrequency.newTotalFrequency
          }
          else {
            val newAccumTotals = accumlatedFrequencyTotals :+ lastProcessedFrequency.newTotalFrequency
            //println(newAccumTotals.mkString(" "))
            if (currentFrequencyIndex + 1 == frequencies.length) {
              processor(0, newAccumTotals, repeatsOfFrequencies + 1)
            }
            else {
              processor(currentFrequencyIndex + 1, newAccumTotals, repeatsOfFrequencies)
            }
          }
        }


      println("-------PART 2-------")
      println(processor(0, Array[Int](), 0))
      println("-------PART 2-------")





      case _ => println("""sbt "runMain advent.advent01.Main src/main/resources/advent01.txt"""")
    }

  }

}
//https://adventofcode.com/2018/day/2
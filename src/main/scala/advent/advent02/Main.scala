package advent.advent02

import scala.io.Source

object Main {


  case class Row(hasTwoLetters: Boolean, hasThreeLetters: Boolean)

  def main(args: Array[String]): Unit = {

    args match {
      case Array(filepath) =>

        println(filepath)
        val fileContent: Iterator[String] = Source.fromFile(filepath).getLines()

        val counts: Iterator[Map[Char, Int]] = fileContent.map(l => l.groupBy(identity).mapValues(_.size))
        //println(counts.mkString("\n"))

        val rowResults: List[Row] = counts.map( (m: Map[Char, Int]) => {
            val twos: Iterable[Int] = m.values.filter(i=> i==2)
            val threes: Iterable[Int] = m.values.filter(i=> i==3)

            Row(twos.nonEmpty, threes.nonEmpty)
          }
        ).toList

        val noOfTwos: Int = rowResults.map(t => t.hasTwoLetters).count(r => r)
        val noOfThrees: Int = rowResults.map(t => t.hasThreeLetters).count(r => r)

        println("Twos: " + noOfTwos)
        println("Threes: " + noOfThrees)

        val result = noOfTwos * noOfThrees

        println("Equation: " + noOfTwos + "*" + noOfThrees)
        println("Result: " + result)


      case _ => println("""sbt "runMain advent.advent01.Main src/main/resources/advent01.txt"""")
    }

  }

}
//https://adventofcode.com/2018/day/2
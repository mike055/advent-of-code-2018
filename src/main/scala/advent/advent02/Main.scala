package advent.advent02

import scala.io.Source

object Main {


  case class Row(hasTwoLetters: Boolean, hasThreeLetters: Boolean)

  def partOne(fileContent: List[String]): Unit = {
    // PART ONE
    val counts: Seq[Map[Char, Int]] = fileContent.map(l => l.groupBy(identity).mapValues(_.size))
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
    // PART ONE
  }


  def outputForLine(currentLine: String, otherLine: String,  chars: Array[CharResult]): Unit = {

    if (chars.filter( cr=> !cr.isSameChar ).length == 1) {
      println("-------------")
      println("LINE: " + currentLine)
      println("MATCHES LINE: " + otherLine)

      val matchingChars = chars.filter(cr=> cr.isSameChar).map(cr => cr.character)

      println("MATCHING CHARS: " + matchingChars.mkString(""))
      println("-------------")
    }

  }

  case class CharResult(character: Char, isSameChar: Boolean)

  def getCharResult(lineChar: Char, otherLineChar: Char): CharResult = {
    CharResult(lineChar, lineChar == otherLineChar)
  }

  def getCharResults(line: String, fileContent: List[String]): Unit = {


    // go through each of the other lines
    for(otherLine <- fileContent.filter(ol=> ol != line)) {

      val charResults: Array[CharResult] = line.toCharArray.zipWithIndex.map(c => getCharResult(c._1, otherLine(c._2)))
      outputForLine(line, otherLine, charResults)

    }
  }

  def partTwo(fileContent: List[String]) : Unit = {
    //PART TWO

    //loop each line
    for(currentLine <- fileContent) {
      getCharResults(currentLine, fileContent)
    }

    //PART TWO
  }

  def main(args: Array[String]): Unit = {

    args match {
      case Array(filepath) =>

        println(filepath)
        val fileContent: List[String] = Source.fromFile(filepath).getLines().toList

        partOne(fileContent)

        partTwo(fileContent)
      case _ => println("""sbt "runMain advent.advent01.Main src/main/resources/advent01.txt"""")
    }

  }

}
//https://adventofcode.com/2018/day/2
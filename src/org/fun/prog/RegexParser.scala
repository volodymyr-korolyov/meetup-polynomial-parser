package org.fun.prog

import scala.collection.immutable.List

/**
  * Requires Scala 2.10.0 for StringContext
  */
object RegexParser extends App {

  val constExp = """([\+-]?\d+)(.*)""".r
  val termExp = """([\+-]?\d*)([a-zA-Z]+)\^?(\d*)(.*)""".r

  def parse(expression: String): List[Member] = {
    expression.replaceAll("\\s", "") match {

      case "" => List()

      // Standard Scala approach:
      case termExp(const, symbol, power, tail) =>
        Term(parseConst(const), symbol, parsePower(power)) :: parse(tail)

      case constExp(const, tail) =>
        Const(const.toInt) :: parse(tail)

      // Combine RegEx and interpolation:
      case exp"""([\+-]?\d*)${const}([a-zA-Z]+)${symbol}\^?(\d*)${power}(.*)${tail}""" =>
        Term(parseConst(const), symbol, parsePower(power)) :: parse(tail)

      case exp"""([\+-]?\d+)${const}(.*)${tail}""" =>
        Const(const.toInt) :: parse(tail)
    }
  }

  /**
    * See explanation in http://blog.xebia.com/matching-strings-in-scala/
    */
  implicit class StringContextExtension(sc: StringContext) {
    def exp = sc.parts.mkString.r
  }

  def parseConst(const: String) = const match {
    case "+" => 1
    case "-" => -1
    case number => number.toInt
  }

  def parsePower(power: String) = power match {
    case "" => 1
    case number => number.toInt
  }

  println(parse("43x^3 - 23x^2 - x + 10x^1 - 8"))
}

package org.fun.prog

import scala.collection.immutable.List

/**
  * Straightforward parser, heavily relies on RegEx.
  * Requires Scala 2.10.0 for StringContext
  */
object RegexParser extends App {

  val constExp = """([\+-]?\d+)(.*)""".r
  val termExp = """([\+-]?\d*)([a-zA-Z]+)\^?(\d*)(.*)""".r

  /**
    * Standard Scala approach to RegEx matching
    */
  def parseRegex(expression: String): List[Member] = expression.replaceAll("\\s", "") match {

    case "" => List()

    case termExp(const, symbol, power, tail) =>
      Term(parseConst(const), symbol, parsePower(power)) :: parseRegex(tail)

    case constExp(const, tail) =>
      Const(const.toInt) :: parseRegex(tail)
  }

  /**
    * RegEx groups are defined and bound inside RegEx<br/>
    * See explanation in http://blog.xebia.com/matching-strings-in-scala/
    */
  def parseInlinedRegex(expression: String): List[Member] = expression.replaceAll("\\s", "") match {

    case "" => List()

    case exp"""([\+-]?\d*)${const}([a-zA-Z]+)${symbol}\^?(\d*)${power}(.*)${tail}""" =>
      Term(parseConst(const), symbol, parsePower(power)) :: parseInlinedRegex(tail)

    case exp"""([\+-]?\d+)${const}(.*)${tail}""" =>
      Const(const.toInt) :: parseInlinedRegex(tail)
  }

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

  println(parseRegex("43x^3 - 23x^2 - x + 10x^1 - 8"))

  println(parseInlinedRegex("43x^3 - 23x^2 - x + 10x^1 - 8"))
}

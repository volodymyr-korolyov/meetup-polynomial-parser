package org.fun.prog

import scala.collection.immutable.List

object GrammarParser extends App {
  type Parser[T] = String => Option[(T, String)]

  val grammar = sequenceGrammar(
    oneOf(
      composite(
        number("""([\+-]?\d*)"""),
        identifier("""([a-zA-Z]+)"""),
        number("""\^?(\d*)"""),
        convertToTerm
      ),
      composite(
        number("""([\+-]?\d+)"""),
        convertConst
      )
    )
  ) _

  def parse(expression: String) = grammar(expression)

  println(parse("43x^3 - 23x^2 + 10x^1 - 8"))

  def convertToTerm(const: Int, symbol: String, power: Int) = Term(const, symbol, power)

  def convertConst(const: Int) = Const(const)

  def sequenceGrammar[T](f: Parser[T])(expression: String): List[T] = expression match {
    case "" => List[T]()
    case _ => f(expression) match {
      case Some((value, tail)) => value :: sequenceGrammar(f)(tail)
      case None => throw new IllegalArgumentException(expression)
    }
  }

  def oneOf[T](f1: Parser[T], f2: Parser[T])(expression: String) = {
    f1(expression).orElse(f2(expression))
  }

  def composite[T1, V](f1: Parser[T1], convert: (T1) => V)(expression: String) = {
    for {
      (v1, expression) <- f1(expression)
    } yield (convert(v1), expression)
  }

  def composite[T1, T2, T3, V](f1: Parser[T1], f2: Parser[T2], f3: Parser[T3], convert: (T1, T2, T3) => V)(expression: String) = {
    for {
      (v1, expression) <- f1(expression)
      (v2, expression) <- f2(expression)
      (v3, expression) <- f3(expression)
    } yield (convert(v1, v2, v3), expression)
  }

  def identifier(pattern: String) = value(pattern)(identity) _

  def number(pattern: String) = value(pattern)(parseNumber) _

  def parseNumber(const: String) = const match {
    case "" => 1
    case "+" => 1
    case "-" => -1
    case number => number.toInt
  }

  def value[T](pattern: String)(convert: String => T)(expression: String): Option[(T, String)] = {
    val exp = (pattern + "(.*)").r
    expression.replaceAll("\\s", "") match {
      case exp(value, tail) => Some((convert(value), tail))
      case _ => None
    }
  }
}

package org.fun.prog

import org.fun.prog.Tokenizer.{NumberToken, SymbolToken, VariableToken}

import scala.collection.immutable.List

/**
  * Grammar is defined with parsing functions.<br/>
  * See idea explanation at http://stackoverflow.com/questions/2245962/is-there-an-alternative-for-flex-bison-that-is-usable-on-8-bit-embedded-systems/2336769#2336769
  */
object GrammarParser extends App {
  type Expression = List[Tokenizer.Token]
  type Parser[T] = Expression => Option[(T, Expression)]
  type NoValueParser = Expression => Option[Expression]
  implicit val defaultIntValue = 1

  // Polynomial BNF-like grammar:

  val grammar = sequenceGrammar(term | const) _

  def term = number.* & variable & (char('^') & number).*   as TermType

  def const = number   as ConstType

  def parse(expression: String) = {
    val tokens = Tokenizer.tokenize(expression)
    grammar(tokens)
  }

  println(parse("43x^3 - 23x^2 + 10x^1 - 8"))
  println(parse("x"))


  def ConstType(value: Int): Member = Const(value)

  def TermType(value: ((Int, String), Int)): Member = Term(value._1._1, value._1._2, value._2)


  implicit class ParserExtension[T](f1: Parser[T]) {
    def |(f2: Parser[T]) = oneOf(f1, f2) _

    def &[T2](f2: Parser[T2]) = composite(f1, f2) _

    def as[V](converter: T => V) = convert(f1, converter) _

    def *(implicit defaultValue: T) = optional(f1, defaultValue)
  }

  implicit class NoValueParserExtension(f1: NoValueParser) {
    def &[T](f2: Parser[T]) = compositeNoValue(f1, f2) _
  }

  def convertToTerm(const: Int, symbol: String, power: Int) = Term(const, symbol, power)

  def convertConst(const: Int) = Const(const)

  def sequenceGrammar[T](f: Parser[T])(expression: Expression): List[T] = expression match {
    case Nil => List[T]()
    case _ => f(expression) match {
      case Some((value, tail)) => value :: sequenceGrammar(f)(tail)
      case None => throw new IllegalArgumentException(expression.toString)
    }
  }

  def oneOf[T](f1: Parser[T], f2: Parser[T])(expression: Expression) = {
    f1(expression).orElse(f2(expression))
  }

  def optional[T](f: Parser[T], defaultValue: T) = (expression: Expression) => f(expression) match {
    case parsed: Some[(T, Expression)] => parsed
    case _ => Some((defaultValue, expression))
  }

  def convert[T1, V](f1: Parser[T1], convert: (T1) => V)(expression: Expression) = {
    for {
      (v1, expression) <- f1(expression)
    } yield (convert(v1), expression)
  }

  def composite[T1, T2, V](f1: Parser[T1], f2: Parser[T2])(expression: Expression) = {
    for {
      (v1, expression) <- f1(expression)
      (v2, expression) <- f2(expression)
    } yield ((v1, v2), expression)
  }

  def compositeNoValue[T, V](f1: NoValueParser, f2: Parser[T])(expression: Expression) = {
    for {
      expression <- f1(expression)
      (v2, expression) <- f2(expression)
    } yield (v2, expression)
  }

  def number: Parser[Int] = {
    case SymbolToken('+') :: NumberToken(value) :: tail => Some((value, tail))
    case SymbolToken('-') :: NumberToken(value) :: tail => Some((-value, tail))
    case NumberToken(value) :: tail => Some((value, tail))
    case _ => None
  }

  def variable: Parser[String] = {
    case VariableToken(variable) :: tail => Some((variable, tail))
    case _ => None
  }

  def char(char: Char): NoValueParser = {
    case SymbolToken(symbol) :: tail if symbol == char => Some(tail)
    case _ => None
  }
}

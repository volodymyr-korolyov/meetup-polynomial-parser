package org.fun.prog

import org.fun.prog.Tokenizer.{NumberToken, SymbolToken, VariableToken}

import scala.collection.immutable.List

/**
  * Grammar is defined with parsing functions.<br/>
  * See idea explanation at http://stackoverflow.com/questions/2245962/is-there-an-alternative-for-flex-bison-that-is-usable-on-8-bit-embedded-systems/2336769#2336769
  */
//noinspection EmptyParenMethodAccessedAsParameterless
object GrammarParser extends App {
  type Expression = List[Tokenizer.Token]
  type Parser[T] = Expression => Option[(T, Expression)]

  implicit val defaultTermConst = 1
  implicit val defaultTermPower = ('^', 1)

  // Polynomial BNF-like grammar:

  def grammar = (term | const) +

  def term = number.? & variable & (char('^') & number).? as TermType

  def const = number as ConstType


  def parse(expression: String) = {
    val tokens = Tokenizer.tokenize(expression)
    grammar(tokens) match {
      case Some((values, Nil)) => values
      case None => throw new RuntimeException("Failed to parse " + expression)
    }
  }

  println(parse("43x^3 - 23x^2 + 10x^1 - 8"))
  println(parse("x"))


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

  def char(char: Char): Parser[Char] = {
    case SymbolToken(symbol) :: tail if symbol == char => Some((symbol, tail))
    case _ => None
  }

  implicit class ParserExtension[T](f1: Parser[T]) {

    def |(f2: Parser[T]) = (expression: Expression) => {
      f1(expression).orElse(f2(expression))
    }

    def &[T2](f2: Parser[T2]) = (expression: Expression) => {
      for {
        (v1, expression) <- f1(expression)
        (v2, expression) <- f2(expression)
      } yield ((v1, v2), expression)
    }

    def ?(implicit defaultValue: T) = (expression: Expression) => {
      f1(expression).orElse(Some((defaultValue, expression)))
    }

    def +(): Parser[List[T]] = {
      case Nil => Some((Nil, Nil))
      case expression => for {
        (headValue, tail) <- f1(expression)
        (tailValue, unparsedTail) <- this.+()(tail)
      } yield (headValue :: tailValue, unparsedTail)
    }

    def as[V](converter: T => V) = (expression: Expression) => {
      for {
        (v1, expression) <- f1(expression)
      } yield (converter(v1), expression)
    }
  }

  def ConstType(value: Int): Member = Const(value)

  def TermType(value: ((Int, String), (Char, Int))): Member = Term(value._1._1, value._1._2, value._2._2)
}

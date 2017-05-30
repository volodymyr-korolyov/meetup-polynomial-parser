package org.fun.prog

import org.fun.prog.Tokenizer.{NumberToken, SymbolToken, VariableToken}

import scala.collection.immutable.List
import scala.util.{Failure, Success, Try}

/**
  * Grammar is defined with parsing functions.<br/>
  * See idea explanation at http://stackoverflow.com/questions/2245962/is-there-an-alternative-for-flex-bison-that-is-usable-on-8-bit-embedded-systems/2336769#2336769
  */
//noinspection EmptyParenMethodAccessedAsParameterless
object GrammarParser extends App {
  type Expression = List[Tokenizer.Token]
  type Parser[T] = Expression => Try[(T, Expression)]

  implicit val defaultTermConst = 1
  implicit val defaultTermPower = ('^', 1)

  // Polynomial BNF-like grammar:

  def grammar = (term | const) *

  def term = number.? & variable & (char('^') & number).? as TermType

  def const = number as ConstType


  def parse(expression: String) = {
    val tokens = Tokenizer.tokenize(expression)
    grammar(tokens).get._1
  }

  println(parse("43x^3 - 23x^2 + 10x^1 - 8"))
  println(parse("x"))


  def number: Parser[Int] = {
    case SymbolToken('+') :: NumberToken(value) :: tail => Success((value, tail))
    case SymbolToken('-') :: NumberToken(value) :: tail => Success((-value, tail))
    case NumberToken(value) :: tail => Success((value, tail))
    case expression => parseFailure(expression)
  }

  def variable: Parser[String] = {
    case VariableToken(variable) :: tail => Success((variable, tail))
    case expression => parseFailure(expression)
  }

  def char(char: Char): Parser[Char] = {
    case SymbolToken(symbol) :: tail if symbol == char => Success((symbol, tail))
    case expression => parseFailure(expression)
  }

  private def parseFailure(expression: Expression) = {
    Failure(new Exception(s"Failed to parse: $expression"))
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
      f1(expression).orElse(Success((defaultValue, expression)))
    }

    def *(): Parser[List[T]] = {
      case Nil => Success((Nil, Nil))
      case expression => for {
        (headValue, tail) <- f1(expression)
        (tailValue, unparsedTail) <- this.*()(tail)
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

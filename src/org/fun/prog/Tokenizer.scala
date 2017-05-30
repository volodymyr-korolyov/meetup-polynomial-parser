package org.fun.prog

import java.io.StreamTokenizer._
import java.io.{StreamTokenizer, StringReader}

object Tokenizer {

  def tokenize(expression: String): List[Token] = {
    val tokenizer = new StreamTokenizer(new StringReader(expression))
    tokenize(tokenizer)
  }

  def tokenize(tokenizer: StreamTokenizer): List[Token] = tokenizer.nextToken() match {
    case TT_NUMBER => NumberToken(tokenizer.nval.toInt) :: tokenize(tokenizer)
    case TT_WORD => VariableToken(tokenizer.sval) :: tokenize(tokenizer)
    case TT_EOL => tokenize(tokenizer)
    case TT_EOF => Nil
    case _ => SymbolToken(tokenizer.ttype.toChar) :: tokenize(tokenizer)
  }


  class Token {}

  case class NumberToken(value: Int) extends Token

  case class VariableToken(name: String) extends Token

  case class SymbolToken(value: Char) extends Token

}

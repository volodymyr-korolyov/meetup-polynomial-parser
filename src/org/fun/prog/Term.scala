package org.fun.prog

case class Term(value: Int, symbol: String, power: Int) extends Member {

  override def multiply(multiplier: Int): Member = Term(value * multiplier, symbol, power)
}

package org.fun.prog

case class Const(value: Int) extends Member {

  override def multiply(multiplier: Int): Member = Const(value * multiplier)
}

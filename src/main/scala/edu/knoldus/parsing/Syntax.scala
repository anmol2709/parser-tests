package edu.knoldus.parsing

trait Syntax {

  sealed abstract class Expression

  case class IntegerLiteral(i: Int) extends Expression
  case class Sum(e1: Expression, e2: Expression) extends Expression
  case class Minus(e1: Expression, e2: Expression) extends Expression
  case class Product(e1: Expression, e2: Expression) extends Expression
  case class Divide(e1: Expression, e2: Expression) extends Expression
}

package edu.knoldus.parsing

object Evaluator extends Syntax {
  def evaluate(e: Expression): Int = e match {
    case IntegerLiteral(i) => i
    case Sum(e1, e2) => evaluate(e1) + evaluate(e2)
    case Minus(e1, e2) => evaluate(e1) - evaluate(e2)
    case Product(e1, e2) => evaluate(e1) * evaluate(e2)
    case Divide(e1, e2) => evaluate(e1) / evaluate(e2)
    case _ => 0
  }
}

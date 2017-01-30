package edu.knoldus.parsing

import ArithmeticParser._
import org.scalatest.FlatSpec

class ArithmeticParserSpec extends FlatSpec  {

  "The parser" should "parse an integer literal" in  {
    assert(parseExpr("42") == IntegerLiteral(42))
  }

  it should "parse a negative integer literal" in  {
    assert(parseExpr("-42") == IntegerLiteral(-42))
  }

  it should "parse a simple sum" in {
    assert(parseExpr("2 + 3") ==
      Sum(
        IntegerLiteral(2),
        IntegerLiteral(3)
      )
    )
  }

  it should "parse a simple product" in {
    assert(parseExpr("2 * 3") ==
      Product(
        IntegerLiteral(2),
        IntegerLiteral(3)
      )
    )
  }

  it should "parse a parenthesized sum" in {
    assert(parseExpr("(2 + 3)") ==
      Sum(
        IntegerLiteral(2),
        IntegerLiteral(3)
      )
    )
  }

  it should "parse a parenthesized product" in {
    assert(parseExpr("(2 * 3)") ==
      Product(
        IntegerLiteral(2),
        IntegerLiteral(3)
      )
    )
  }

  it should "parse a complex expression" in {
    assert(parseExpr("(2 * 3) * (4 + 5)") ==
      Product(
        Product(
          IntegerLiteral(2),
          IntegerLiteral(3)),
        Sum(
          IntegerLiteral(4),
          IntegerLiteral(5)
        )
      )
    )
  }

  it should "throw exception given incomplete input" in  {
    intercept[IllegalArgumentException] {
      parseExpr("2 +")
    }
  }

}

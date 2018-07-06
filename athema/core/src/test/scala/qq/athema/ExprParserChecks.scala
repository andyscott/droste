package qq.athema

import cats.syntax.all._

import org.scalacheck.Properties
import org.scalacheck.Prop._

import qq.droste.data.Fix

class ExprParserChecks extends Properties("ExprParser") {

  val pairs: List[(String, Expr.Fixed[Double])] = List(
    "1 / 2 + 3" ->
      Add(
        Div(
          Fix(Const(1.0)),
          Fix(Const(2.0))),
        Fix(Const(3.0))),

    "1 / (2 + 3)" ->
      Div(
        Fix(Const(1.0)),
        Add(
          Fix(Const(2.0)),
          Fix(Const(3.0)))),

    "1 + 2 / 3" ->
      Add(
        Fix(Const(1.0)),
        Div(
          Fix(Const(2.0)),
          Fix(Const(3.0)))),

    "(1 + 2) / 3" ->
      Div(
        Add(
          Fix(Const(1.0)),
          Fix(Const(2.0))),
        Fix(Const(3.0))),

    "(1 + 2) / 3 + 4" ->
      Add(
        Div(
          Add(
            Fix(Const(1.0)),
            Fix(Const(2.0))),
          Fix(Const(3.0))),
        Fix(Const(4.0))),

    "x + y" ->
      Add(
        Fix(Var("x")),
        Fix(Var("y"))),

    "x + 1 * y / 2 - 3" ->
      Sub(
        Add(
          Fix(Var("x")),
          Div(
            Prod(
              Fix(Const(1.0)),
              Fix(Var("y"))),
            Fix(Const(2.0)))),
        Fix(Const(3.0)))

  )

  def parse(input: String): Either[String, Expr.Fixed[Double]] =
    ExprParser.parse(input).map(_.map(_.toDouble))

  pairs.foreach { case (input, output) =>
    property(input) = parse(input) ?= output.asRight
  }

}

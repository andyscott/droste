package qq.athema

import cats.syntax.all._
import algebra.instances.all._

import org.scalacheck.Properties
import org.scalacheck.Prop._

class Checks extends Properties("athema") {

  val fe = Evaluate.evaluate[Double]
  val fd = Differentiate.differentiate[Double]
  val fs = Simplify.simplify[Double]
  val fds = fd andThen fs

  lazy val fePairs: List[(Expr.Fixed[Double], Option[Double])] = List(
    Const.fix(1.0) -> 1.0.some,
    Add(Const.fix(1.0), Const.fix(2.0)) -> 3.0.some,
    Div(Sub(Prod(Const.fix(2.0), Const.fix(3.3)), Const.fix(0.6)), Const.fix(2.0)) -> 3.0.some
  )

  fePairs.foreach { case (in, out) =>
    property(s"evaluate $in") = {
      fe(in).toOption ?= out
    }
  }

  lazy val varx = Var.fix[Double]("x")

  lazy val fdPairs: List[(Expr.Fixed[Double], Expr.Fixed[Double])] = List(
    Const.fix(1.0) -> Const.fix(0.0),
    Prod(varx, varx) -> Add(Prod(varx, Const.fix(1.0)), Prod(Const.fix(1.0), varx))
  )

  fdPairs.foreach { case (in, out) =>
    property(s"differentiate $in") = {
      fd(in) ?= out
    }
  }

  lazy val fdsPairs: List[(Expr.Fixed[Double], Expr.Fixed[Double])] = List(
    Const.fix(1.0) -> Const.fix(0.0),
    Prod(varx, varx) -> Prod(Const.fix(2.0), varx)
  )

  fdsPairs.foreach { case (in, out) =>
    property(s"differentiate and simplify $in") = {
      fds(in) ?= out
    }
  }

}

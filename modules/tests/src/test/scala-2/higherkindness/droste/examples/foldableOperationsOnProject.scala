package higherkindness.droste
package examples

import cats.kernel.Eq

import org.scalacheck.Properties
import org.scalacheck.Prop._

import higherkindness.droste.macros.deriveFixedPoint
import higherkindness.droste.syntax.all._

@deriveFixedPoint sealed trait LExpr
object LExpr {
  case class Var(name: String)               extends LExpr
  case class App(fn: LExpr, param: LExpr)    extends LExpr
  case class Lam(name: String, param: LExpr) extends LExpr

  def `var`[A](name: String): fixedpoint.LExprF[A]  = fixedpoint.VarF(name)
  def app[A](fn: A, param: A): fixedpoint.LExprF[A] = fixedpoint.AppF(fn, param)
  def lam[A](name: String, param: A): fixedpoint.LExprF[A] =
    fixedpoint.LamF(name, param)

  implicit val eq: Eq[LExpr] = Eq.fromUniversalEquals
}

final class FoldableOpsChecks
    extends Properties("foldableOperationsOnProject") {

  import LExpr._
  import LExpr.fixedpoint._

  def tru[T: Basis[LExprF, *]]: T =
    lam[T](
      "a",
      lam[T](
        "b",
        lam[T](
          "c",
          lam[T]("d", lam[T]("e", lam[T]("f", `var`[T]("c").embed).embed).embed).embed).embed).embed).embed

  property("collect") =
    tru[LExpr].collect[List[String], String] {
      case Lam(name, _) => name
    } ?= List("a", "b", "c", "d", "e", "f")

  property("any") =
    tru[LExpr].any {
      case Var(_) => true
      case _      => false
    } ?= true

  property("contains") =
    tru[LExpr].contains(Var("c")) ?= true

  property("contains") =
    tru[LExpr].contains(App(Lam("name", Var("x")), Var(""))) ?= false

}

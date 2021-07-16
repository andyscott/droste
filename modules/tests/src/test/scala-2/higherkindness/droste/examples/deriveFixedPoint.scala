package higherkindness.droste
package examples

import org.scalacheck.Properties
import org.scalacheck.Prop._

import higherkindness.droste.macros.deriveFixedPoint

@deriveFixedPoint sealed trait RecursiveExpr
object RecursiveExpr {
  final case class Dummy()

  final case class Const(value: BigDecimal)                extends RecursiveExpr
  final case class Add(x: RecursiveExpr, y: RecursiveExpr) extends RecursiveExpr
  final case class AddList(list: List[RecursiveExpr])      extends RecursiveExpr
}

final class RecursiveExprChecks extends Properties("deriveFixedPoint") {
  import RecursiveExpr._
  import RecursiveExpr.fixedpoint._

  val evaluateAlgebra: Algebra[RecursiveExprF, BigDecimal] = Algebra {
    case ConstF(v)   => v
    case AddF(x, y)  => x + y
    case AddListF(l) => l.reduce(_ + _)
  }

  val evaluate: RecursiveExpr => BigDecimal = scheme.cata(evaluateAlgebra)

  property("1") =
    evaluate(Const(1)) ?= 1

  property("1 + 1") =
    evaluate(Add(Const(1), Const(1))) ?= 2

  property("1 + 2 + 5") =
    evaluate(Add(Add(Const(1), Const(2)), Const(5))) ?= 8

  property("1 + 2 + 3 + 4 + 5") =
    evaluate(AddList(List(Const(1), Const(2), Const(3), Const(4), Const(5)))) ?= 15
}

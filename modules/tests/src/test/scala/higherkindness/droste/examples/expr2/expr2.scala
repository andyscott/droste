package higherkindness.droste
package examples

import org.scalacheck.Properties
import org.scalacheck.Prop._

import cats._
import cats.syntax.all._

import higherkindness.droste.util.DefaultTraverse

// demos recursion schemes between a regular AST and a fixed
// point AST without using Fix
final class Expr2Checks extends Properties("Expr2") {

  val evaluateAlgebra: Algebra[ExprF, BigDecimal] = Algebra {
    case ConstF(v)  => v
    case AddF(x, y) => x + y
  }

  val evaluate: Expr => BigDecimal = scheme.cata(evaluateAlgebra)

  property("1") = evaluate(Const(1)) ?= 1

  property("1 + 1") = evaluate(Add(Const(1), Const(1))) ?= 2

  property("1 + 2 + 5") = evaluate(Add(Add(Const(1), Const(2)), Const(5))) ?= 8
}

sealed trait Expr
final case class Const(value: BigDecimal) extends Expr
final case class Add(x: Expr, y: Expr)    extends Expr

sealed trait ExprF[A]
final case class ConstF[A](value: BigDecimal) extends ExprF[A]
final case class AddF[A](x: A, y: A)          extends ExprF[A]

object ExprF {
  implicit val traverseExprF: Traverse[ExprF] =
    new DefaultTraverse[ExprF] {
      def traverse[G[_]: Applicative, A, B](
          fa: ExprF[A]
      )(f: A => G[B]): G[ExprF[B]] =
        fa match {
          case c: ConstF[B] @unchecked => (c: ExprF[B]).pure[G]
          case AddF(x, y)              => (f(x), f(y)).mapN(AddF(_, _))
        }
    }

  val embedAlgebra: Algebra[ExprF, Expr] = Algebra {
    case ConstF(v)  => Const(v)
    case AddF(x, y) => Add(x, y)
  }

  val projectCoalgebra: Coalgebra[ExprF, Expr] = Coalgebra {
    case Const(v)  => ConstF(v)
    case Add(x, y) => AddF(x, y)
  }

  implicit val basisExprF: Basis[ExprF, Expr] =
    Basis.Default(embedAlgebra, projectCoalgebra)
}

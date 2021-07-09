package higherkindness.athema

import cats.Applicative
import cats.Traverse
import cats.syntax.all._

import higherkindness.droste.data._
import higherkindness.droste.util.DefaultTraverse

sealed trait Expr[V, A]
final case class Var[V, A](name: String) extends Expr[V, A]
final case class Const[V, A](value: V)   extends Expr[V, A]
final case class Neg[V, A](x: A)         extends Expr[V, A]
final case class Add[V, A](x: A, y: A)   extends Expr[V, A]
final case class Sub[V, A](x: A, y: A)   extends Expr[V, A]
final case class Prod[V, A](x: A, y: A)  extends Expr[V, A]
final case class Div[V, A](x: A, y: A)   extends Expr[V, A]

object Expr extends ExprInstances {
  type Fixed[V] = Fix[Expr[V, *]]
}

object Var {
  def fix[V](name: String): Expr.Fixed[V] = Fix(Var(name))
}

object Const {
  def fix[V](value: V): Expr.Fixed[V] = Fix(Const(value))
}

object Neg {
  def apply[V](x: Expr.Fixed[V]): Expr.Fixed[V] = Fix(Neg(x))
}

object Add {
  def apply[V](x: Expr.Fixed[V], y: Expr.Fixed[V]): Expr.Fixed[V] =
    Fix(Add(x, y))
}

object Sub {
  def apply[V](x: Expr.Fixed[V], y: Expr.Fixed[V]): Expr.Fixed[V] =
    Fix(Sub(x, y))
}

object Prod {
  def apply[V](x: Expr.Fixed[V], y: Expr.Fixed[V]): Expr.Fixed[V] =
    Fix(Prod(x, y))
}

object Div {
  def apply[V](x: Expr.Fixed[V], y: Expr.Fixed[V]): Expr.Fixed[V] =
    Fix(Div(x, y))
}

private[athema] sealed trait ExprInstances {
  implicit def traverseExpr[V]: Traverse[Expr[V, *]] =
    new DefaultTraverse[Expr[V, *]] {
      def traverse[G[_]: Applicative, A, B](
          fa: Expr[V, A]
      )(f: A => G[B]): G[Expr[V, B]] = fa match {
        case v: Var[V, A]   => (v.asInstanceOf[Expr[V, B]]).pure[G]
        case c: Const[V, A] => (c.asInstanceOf[Expr[V, B]]).pure[G]
        case e: Neg[V, A]   => f(e.x) map (Neg(_))
        case e: Add[V, A]   => (f(e.x), f(e.y)) mapN (Add(_, _))
        case e: Sub[V, A]   => (f(e.x), f(e.y)) mapN (Sub(_, _))
        case e: Prod[V, A]  => (f(e.x), f(e.y)) mapN (Prod(_, _))
        case e: Div[V, A]   => (f(e.x), f(e.y)) mapN (Div(_, _))
      }
    }
}

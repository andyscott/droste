package qq.athema

import cats.Functor

import qq.droste._
import qq.droste.data.Fix

object `package` {

  implicit val functorExprFixed: Functor[Expr.Fixed] = new Functor[Expr.Fixed] {
    def map[A, B](fa: Expr.Fixed[A])(f: A => B): Expr.Fixed[B] =
      scheme.cata(mapAlgebra(f)).apply(fa)
  }

  private def mapAlgebra[A, B](f: A => B): Algebra[Expr[A, ?], Expr.Fixed[B]] = {
    case Const(c)             => Fix(Const(f(c)))
    case other: Expr.Fixed[B] => other
  }

}

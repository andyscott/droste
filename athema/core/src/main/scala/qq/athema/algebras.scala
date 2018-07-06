package qq.athema

import algebra.ring.Field
import algebra.ring.Ring
import cats.implicits._

import qq.droste._
import qq.droste.data._
import qq.droste.syntax._

object Evaluate {
  def evaluate[V: Field]: Expr.Fixed[V] => Either[String, V] =
    scheme.cataM(algebraM(Map.empty))

  def algebraM[V](
    variables: Map[String, V]
  )(implicit V: Field[V]): AlgebraM[String | ?, Expr[V, ?], V] = {
    case Var  (name)  => variables.get(name).toRight(s"unknown variable: $name")
    case Const(v)     => v.asRight
    case Neg  (x)     => V.negate(x).asRight
    case Add  (x, y)  => V.plus(x, y).asRight
    case Sub  (x, y)  => V.plus(x, V.negate(y)).asRight
    case Prod (x, y)  => V.times(x, y).asRight
    case Div  (x, y)  => V.div(x, y).asRight
  }
}

object Differentiate {
  def differentiate[V: Ring]: Expr.Fixed[V] => Expr.Fixed[V] =
    scheme.para(algebra[V])

  def algebra[V](implicit V: Ring[V]): RAlgebra[Expr.Fixed[V], Expr[V, ?], Expr.Fixed[V]] = {
    case _: Var  [_, _]          => Const.fix(V.one)
    case _: Const[_, _]          => Const.fix(V.zero)
    case Neg  ((_, xx))          => Neg(xx)
    case Add  ((_, xx), (_, yy)) => Add(xx, yy)
    case Sub  ((_, xx), (_, yy)) => Sub(xx, yy)
    case Prod ((x, xx), (y, yy)) => Add(Prod(x, yy), Prod(xx, y))
    case Div  ((x, xx), (y, yy)) => Div(Sub(Prod(xx, y), Prod(x, yy)), Prod(y, y))
  }
}

object Simplify {
  def simplify[V: Field]: Expr.Fixed[V] => Expr.Fixed[V] =
    scheme.cata(algebra[V])

  def algebra[V](implicit V: Field[V]): Algebra[Expr[V, ?], Expr.Fixed[V]] = { fa =>

    val Zero: Expr.Fixed[V] = Const.fix(V.zero)
    val One : Expr.Fixed[V] = Const.fix(V.one)
    val Two : Expr.Fixed[V] = Const.fix(V.plus(V.one, V.one))

    fa match {
      case Prod(Zero, _)        => Zero
      case Prod(_, Zero)        => Zero
      case Prod(One, v)         => v
      case Prod(v, One)         => v
      case Sub (Zero, v)        => Neg(v)
      case Add (x, y) if x == y => Prod(Two, x)
      case Add (x, Prod(Const(n: V), y)) if x == y => Prod(Const.fix(V.plus(n, V.one)), x)
      case other                => Fix(other)
    }
  }
}

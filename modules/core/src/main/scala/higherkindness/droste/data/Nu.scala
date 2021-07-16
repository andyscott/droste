package higherkindness.droste
package data

import cats.Functor
import cats.syntax.functor._

/** Nu is the greatest fixed point of a functor `F`. It is a
  * computation that can generate a coinductive infinite
  * structure on demand.
  *
  * In Haskell this can more aptly be expressed as:
  * `data Nu g = forall s . Nu (s -> g s) s`
  */
sealed abstract class Nu[F[_]] extends Serializable {
  type A
  def unfold: Coalgebra[F, A]
  def a: A

  override final def toString: String = s"Nu($unfold, $a)"
}

object Nu {
  def apply[F[_], A](unfold0: Coalgebra[F, A], a0: A): Nu[F] =
    new Default(unfold0, a0)

  def algebra[F[_]: Functor]: Algebra[F, Nu[F]] =
    Algebra(t => Nu(Coalgebra[F, F[Nu[F]]](_ map coalgebra.run), t))

  def coalgebra[F[_]: Functor]: Coalgebra[F, Nu[F]] =
    Coalgebra(nf => nf.unfold(nf.a).map(Nu(nf.unfold, _)))

  def apply[F[_]: Functor](fnf: F[Nu[F]]): Nu[F] = algebra[F].apply(fnf)
  def un[F[_]: Functor](nf: Nu[F]): F[Nu[F]]     = coalgebra[F].apply(nf)

  def unapply[F[_]: Functor](nf: Nu[F]): Some[F[Nu[F]]] = Some(un(nf))

  private final class Default[F[_], A0](val unfold: Coalgebra[F, A0], val a: A0)
      extends Nu[F] {
    type A = A0
  }

  implicit def drosteBasisForNu[F[_]: Functor]: Basis[F, Nu[F]] =
    Basis.Default[F, Nu[F]](Nu.algebra, Nu.coalgebra)

  implicit val drosteBasisSolveForNu: Basis.Solve.Aux[
    Nu,
    ({ type L[F[_], A] = F[A] })#L
  ] = null
}

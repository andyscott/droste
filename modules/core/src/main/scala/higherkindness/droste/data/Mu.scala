package higherkindness.droste
package data

import cats.~>
import cats.Functor
import cats.Id
import cats.syntax.functor._

/** Mu is the least fixed point of a functor `F`. It is a
  * computation that can consume a inductive noninfinite
  * structure in one go.
  *
  * In Haskell this can more aptly be expressed as:
  * `data Mu f = Mu (forall x . (f x -> x) -> x)`
  */
sealed abstract class Mu[F[_]] extends Serializable {
  def apply[A](fold: Algebra[F, A]): A

  def toFunctionK: Algebra[F, *] ~> Id =
    new (Algebra[F, *] ~> Id) {
      def apply[A](fa: Algebra[F, A]): Id[A] = Mu.this.apply(fa)
    }
}

object Mu {
  def algebra[F[_]: Functor]: Algebra[F, Mu[F]] =
    Algebra(fmf => Default(fmf))

  def coalgebra[F[_]: Functor]: Coalgebra[F, Mu[F]] =
    Coalgebra[F, Mu[F]](mf => mf[F[Mu[F]]](Algebra(_ map algebra.run)))

  def apply[F[_]: Functor](fmf: F[Mu[F]]): Mu[F] = algebra[F].apply(fmf)
  def un[F[_]: Functor](mf: Mu[F]): F[Mu[F]]     = coalgebra[F].apply(mf)

  def unapply[F[_]: Functor](mf: Mu[F]): Some[F[Mu[F]]] = Some(un(mf))

  private final case class Default[F[_]: Functor](fmf: F[Mu[F]]) extends Mu[F] {
    def apply[A](fold: Algebra[F, A]): Id[A] =
      fold(fmf map (mf => mf(fold)))

    override def toString: String = s"Mu($fmf)"
  }

  implicit def drosteBasisForMu[F[_]: Functor]: Basis[F, Mu[F]] =
    Basis.Default[F, Mu[F]](Mu.algebra, Mu.coalgebra)

  implicit val drosteBasisSolveForMu: Basis.Solve.Aux[
    Mu,
    ({ type L[F[_], A] = F[A] })#L
  ] = null
}

package higherkindness.droste
package data
package list

import cats.Applicative
import cats.Eq
import cats.Monoid
import cats.Traverse
import cats.syntax.all._

import higherkindness.droste.util.DefaultTraverse

sealed trait ListF[+A, +B]
final case class ConsF[A, B](head: A, tail: B) extends ListF[A, B]
case object NilF                               extends ListF[Nothing, Nothing]

object ListF {

  def toScalaList[A, PatR[_[_]]](list: PatR[ListF[A, *]])(implicit
      ev: Project[ListF[A, *], PatR[ListF[A, *]]]
  ): List[A] =
    scheme.cata(toScalaListAlgebra[A]).apply(list)

  def toScalaListAlgebra[A]: Algebra[ListF[A, *], List[A]] = Algebra {
    case ConsF(head, tail) => head :: tail
    case NilF              => Nil
  }

  def fromScalaList[A, PatR[_[_]]](list: List[A])(implicit
      ev: Embed[ListF[A, *], PatR[ListF[A, *]]]
  ): PatR[ListF[A, *]] =
    scheme.ana(fromScalaListCoalgebra[A]).apply(list)

  def fromScalaListCoalgebra[A]: Coalgebra[ListF[A, *], List[A]] = Coalgebra {
    case head :: tail => ConsF(head, tail)
    case Nil          => NilF
  }

  implicit def drosteTraverseForListF[A]: Traverse[ListF[A, *]] =
    new DefaultTraverse[ListF[A, *]] {
      def traverse[F[_]: Applicative, B, C](
          fb: ListF[A, B]
      )(f: B => F[C]): F[ListF[A, C]] =
        fb match {
          case ConsF(head, tail) => f(tail).map(ConsF(head, _))
          case NilF              => (NilF: ListF[A, C]).pure[F]
        }
    }

  implicit def basisListFMonoid[T, A](implicit
      T: Basis[ListF[A, *], T]
  ): Monoid[T] =
    new Monoid[T] {
      def empty = T.algebra(NilF)
      def combine(f1: T, f2: T): T = {
        scheme
          .cata(Algebra[ListF[A, *], T] {
            case NilF => f2
            case cons => T.algebra(cons)
          })
          .apply(f1)
      }
    }

  import cats.~>
  import syntax.compose._

  implicit def drosteDelayEqListF[A](implicit
      eh: Eq[A]
  ): Delay[Eq, ListF[A, *]] =
    new (Eq ~> (Eq ∘ ListF[A, *])#λ) {
      def apply[B](et: Eq[B]): (Eq ∘ ListF[A, *])#λ[B] =
        Eq.instance((x, y) =>
          x match {
            case ConsF(hx, tx) =>
              y match {
                case ConsF(hy, ty) => eh.eqv(hx, hy) && et.eqv(tx, ty)
                case NilF          => false
              }
            case NilF =>
              y match {
                case NilF => true
                case _    => false
              }
          }
        )
    }
}

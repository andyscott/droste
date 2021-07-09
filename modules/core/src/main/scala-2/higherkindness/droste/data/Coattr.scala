package higherkindness.droste
package data

import cats.Functor
import cats.syntax.functor._

import meta.Meta

object Coattr {
  def apply[F[_], A](f: Either[A, F[Coattr[F, A]]]): Coattr[F, A] =
    macro Meta.fastCast
  def un[F[_], A](f: Coattr[F, A]): Either[A, F[Coattr[F, A]]] =
    macro Meta.fastCast

  def pure[F[_], A](a: A): Coattr[F, A]                = apply(Left(a))
  def roll[F[_], A](fa: F[Coattr[F, A]]): Coattr[F, A] = apply(Right(fa))

  def algebra[F[_], A]: Algebra[CoattrF[F, A, *], Coattr[F, A]] =
    Algebra(fa => Coattr(CoattrF.un(fa)))

  def coalgebra[F[_], A]: Coalgebra[CoattrF[F, A, *], Coattr[F, A]] =
    Coalgebra(a => CoattrF(Coattr.un(a)))

  def fromCats[F[_]: Functor, A](free: cats.free.Free[F, A]): Coattr[F, A] =
    free.fold(pure, { ffree =>
      roll(ffree.map(fromCats(_)))
    })

  object Pure {
    def unapply[F[_], A](f: Coattr[F, A]): Option[A] = un(f) match {
      case Left(a) => Some(a)
      case _       => None
    }
  }

  object Roll {
    def unapply[F[_], A](f: Coattr[F, A]): Option[F[Coattr[F, A]]] =
      un(f) match {
        case Right(fa) => Some(fa)
        case _         => None
      }
  }
}

trait CoattrImplicits {
  implicit final class CoattrOps[F[_], A](coattr: Coattr[F, A]) {
    def fold[B](f: A => B, fffa: F[Coattr[F, A]] => B): B =
      Coattr.un(coattr).fold(f, fffa)

    def toCats(implicit ev: Functor[F]): cats.free.Free[F, A] =
      fold(cats.free.Free.pure, { fcoattr =>
        cats.free.Free.roll(fcoattr.map(_.toCats))
      })
  }
}

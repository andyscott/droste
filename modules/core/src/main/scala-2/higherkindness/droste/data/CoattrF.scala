package higherkindness.droste
package data

import cats.Applicative
import cats.Eq
import cats.Functor
import cats.Traverse

import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.traverse._

import meta.Meta
import higherkindness.droste.util.DefaultTraverse

object CoattrF {
  def apply[F[_], A, B](f: Either[A, F[B]]): CoattrF[F, A, B] =
    macro Meta.fastCast
  def un[F[_], A, B](f: CoattrF[F, A, B]): Either[A, F[B]] = macro Meta.fastCast

  def pure[F[_], A, B](a: A): CoattrF[F, A, B]     = CoattrF(Left(a))
  def roll[F[_], A, B](fb: F[B]): CoattrF[F, A, B] = CoattrF(Right(fb))

  object Pure {
    def unapply[F[_], A, B](f: CoattrF[F, A, B]): Option[A] = un(f) match {
      case Left(a) => Some(a)
      case _       => None
    }
  }

  object Roll {
    def unapply[F[_], A, B](f: CoattrF[F, A, B]): Option[F[B]] = un(f) match {
      case Right(fa) => Some(fa)
      case _         => None
    }
  }
}

private[data] trait CoattrFImplicits extends CoenvtTImplicits0 {
  implicit def drosteCoattrFTraverse[F[_]: Traverse, A]: Traverse[
    CoattrF[F, A, *]] =
    new CoattrFTraverse[F, A]
}

private[data] sealed trait CoenvtTImplicits0 {

  implicit def drosteCoattrFDelayEq[F[_], A](
      implicit eqa: Eq[A],
      deqf: Delay[Eq, F]): Delay[Eq, CoattrF[F, A, *]] =
    new Delay[Eq, CoattrF[F, A, *]] {
      def apply[B](eqb: Eq[B]): Eq[CoattrF[F, A, B]] = Eq.instance { (x, y) =>
        (CoattrF.un(x), CoattrF.un(y)) match {
          case (Left(xx), Left(yy))   => eqa.eqv(xx, yy)
          case (Right(xx), Right(yy)) => deqf(eqb).eqv(xx, yy)
          case _                      => false
        }
      }
    }

  implicit def drosteCoattrFEq[F[_], A, B](
      implicit ev: Eq[Either[A, F[B]]]): Eq[CoattrF[F, A, B]] =
    Eq.by(CoattrF.un(_))

  implicit def drosteCoattrFFunctor[F[_]: Functor, A]: Functor[
    CoattrF[F, A, *]] =
    new CoattrFFunctor[F, A]
}

private[data] sealed class CoattrFFunctor[F[_]: Functor, A]
    extends Functor[CoattrF[F, A, *]] {
  def map[B, C](fb: CoattrF[F, A, B])(f: B => C): CoattrF[F, A, C] =
    CoattrF.un(fb) match {
      case Right(fbb)             => CoattrF.roll(fbb.map(f))
      case other: Either[A, F[C]] => CoattrF(other)
    }
}

private[data] final class CoattrFTraverse[F[_]: Traverse, A]
    extends CoattrFFunctor[F, A]
    with DefaultTraverse[CoattrF[F, A, *]] {
  def traverse[G[_]: Applicative, B, C](fb: CoattrF[F, A, B])(
      f: B => G[C]): G[CoattrF[F, A, C]] =
    CoattrF.un(fb) match {
      case Right(fbb)             => fbb.traverse(f).map(CoattrF.roll)
      case other: Either[A, F[C]] => CoattrF(other).pure[G]
    }
}

package higherkindness.droste
package data

import cats.Applicative
import cats.Eq
import cats.Functor
import cats.Traverse

import cats.syntax.functor._
import cats.syntax.traverse._

import meta.Meta
import higherkindness.droste.data.prelude._
import higherkindness.droste.util.DefaultTraverse

object AttrF {
  def apply[F[_], A, B](ask: A, lower: F[B]): AttrF[F, A, B] =
    apply((ask, lower))
  def apply[F[_], A, B](f: (A, F[B])): AttrF[F, A, B] = macro Meta.fastCast
  def un[F[_], A, B](f: AttrF[F, A, B]): (A, F[B]) = macro Meta.fastCast
  def unapply[F[_], A, B](f: AttrF[F, A, B]): Some[(A, F[B])] = Some(f.tuple)
}

private[data] trait AttrFImplicits extends AttrFImplicits0 {
  implicit final class AttrFOps[F[_], A, B](attrf: AttrF[F, A, B]) {
    def tuple: (A, F[B]) = AttrF.un(attrf)
    def ask: A           = tuple._1
    def lower: F[B]      = tuple._2
  }

  implicit def drosteAttrFTraverse[F[_]: Traverse, A]: Traverse[
    AttrF[F, A, *]] =
    new AttrFTraverse[F, A]
}

private[data] sealed trait AttrFImplicits0 {

  implicit def drosteAttrFDelayEq[F[_], A](
      implicit eqa: Eq[A],
      deqf: Delay[Eq, F]): Delay[Eq, AttrF[F, A, *]] =
    new Delay[Eq, AttrF[F, A, *]] {
      def apply[B](eqb: Eq[B]): Eq[AttrF[F, A, B]] = Eq.instance { (x, y) =>
        val xx = AttrF.un(x)
        val yy = AttrF.un(y)

        eqa.eqv(xx._1, yy._1) && deqf(eqb).eqv(xx._2, yy._2)
      }
    }

  implicit def drosteAttrFEq[F[_], A, B](
      implicit ev: Eq[(A, F[B])]): Eq[AttrF[F, A, B]] =
    Eq.by(AttrF.un(_))

  implicit def drosteAttrFFunctor[F[_]: Functor, A]: Functor[AttrF[F, A, *]] =
    new AttrFFunctor[F, A]
}

private[data] sealed class AttrFFunctor[F[_]: Functor, A]
    extends Functor[AttrF[F, A, *]] {
  def map[B, C](fb: AttrF[F, A, B])(f: B => C): AttrF[F, A, C] =
    AttrF(fb.ask, fb.lower.map(f))
}

private[data] final class AttrFTraverse[F[_]: Traverse, A]
    extends AttrFFunctor[F, A]
    with DefaultTraverse[AttrF[F, A, *]] {
  def traverse[G[_]: Applicative, B, C](fb: AttrF[F, A, B])(
      f: B => G[C]): G[AttrF[F, A, C]] =
    fb.lower.traverse(f).map(AttrF(fb.ask, _))
}

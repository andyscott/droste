package higherkindness.droste
package data

import cats.Comonad
import cats.Eval
import cats.Functor

import cats.syntax.functor._

import prelude._

object Attr {
  def apply[F[_], A](head: A, tail: F[Attr[F, A]]): Attr[F, A] =
    apply((head, tail))
  def apply[F[_], A](f: (A, F[Attr[F, A]])): Attr[F, A]         = f.asInstanceOf
  def un[F[_], A](f: Attr[F, A]): (A, F[Attr[F, A]])            = f.asInstanceOf
  def unapply[F[_], A](f: Attr[F, A]): Some[(A, F[Attr[F, A]])] = Some(un(f))

  def algebra[F[_], A]: Algebra[AttrF[F, A, *], Attr[F, A]] =
    Algebra(fa => Attr(AttrF.un(fa)))

  def coalgebra[F[_], A]: Coalgebra[AttrF[F, A, *], Attr[F, A]] =
    Coalgebra(a => AttrF(Attr.un(a)))

  def fromCats[F[_]: Functor, A](cofree: cats.free.Cofree[F, A]): Attr[F, A] =
    ana(cofree)(_.tail.value, _.head)

  def unfold[F[_]: Functor, A](a: A)(coalgebra: A => F[A]): Attr[F, A] =
    ana(a)(coalgebra, identity)

  /** An inlined anamorphism to `Attr` with a fused map */
  def ana[F[_]: Functor, A, C](
      a: A
  )(coalgebra: A => F[A], f: A => C): Attr[F, C] =
    Attr(f(a), coalgebra(a).map(ana(_)(coalgebra, f)))
}

private[data] trait AttrImplicits {
  implicit final class AttrOps[F[_], A](attr: Attr[F, A]) {
    def tuple: (A, F[Attr[F, A]]) = Attr.un(attr)
    def head: A                   = tuple._1
    def tail: F[Attr[F, A]]       = tuple._2

    def toCats(implicit ev: Functor[F]): cats.free.Cofree[F, A] =
      cats.free.Cofree(head, Eval.later(tail.map(_.toCats)))

    def forget(implicit ev: Functor[F]): Fix[F] =
      Fix(tail.map(_.forget))
  }

  implicit def drosteAttrComonad[F[_]: Functor]: Comonad[Attr[F, *]] =
    new AttrComonad[F]
}

private[data] final class AttrComonad[F[_]: Functor]
    extends Comonad[Attr[F, *]] {
  def coflatMap[A, B](fa: Attr[F, A])(f: Attr[F, A] => B): Attr[F, B] =
    Attr.ana(fa)(_.tail, f)

  def extract[A](fa: Attr[F, A]): A = fa.head

  def map[A, B](fa: Attr[F, A])(f: A => B): Attr[F, B] =
    Attr(f(fa.head), fa.tail.map(_.map(f)))
}

package qq.droste
package data

import cats.~>
import cats.Applicative
import cats.Id
import cats.Eval
import cats.Functor
import cats.Traverse
import cats.syntax.functor._
import cats.syntax.traverse._

import meta._

object `package` {

  type Fix[F[_]] // = F[Fix[F]]

  object Fix {
    def apply[F[_]](f: F[Fix[F]]): Fix[F]    = macro Meta.fastCast
    def fix  [F[_]](f: F[Fix[F]]): Fix[F]    = macro Meta.fastCast
    def unfix[F[_]](f: Fix[F])   : F[Fix[F]] = macro Meta.fastCast

    def algebra  [F[_]]: Algebra  [F, Fix[F]] = fix(_)
    def coalgebra[F[_]]: Coalgebra[F, Fix[F]] = unfix(_)
  }

  type Cofree[F[_], A] // = (A, F[Cofree[F, A]])

  object Cofree {
    def apply   [F[_], A](head: A, tail: F[Cofree[F, A]]): Cofree[F, A] = apply((head, tail))
    def apply   [F[_], A](f: (A, F[Cofree[F, A]])): Cofree[F, A] = macro Meta.fastCast
    def cofree  [F[_], A](f: (A, F[Cofree[F, A]])): Cofree[F, A] = macro Meta.fastCast
    def uncofree[F[_], A](f: Cofree[F, A]): (A, F[Cofree[F, A]]) = macro Meta.fastCast

    def algebra[E, F[_]]: Algebra[EnvT[E, F, ?], Cofree[F, E]] =
      fa => Cofree(EnvT.unenvT(fa))

    def coalgebra[E, F[_]]: Coalgebra[EnvT[E, F, ?], Cofree[F, E]] =
      a => EnvT(Cofree.uncofree(a))

    def fromCats[F[_]: Functor, A](cofree: cats.free.Cofree[F, A]): Cofree[F, A] =
      Cofree(cofree.head, cofree.tail.value.map(fromCats(_)))
  }

  implicit final class CofreeOps[F[_], A](val cofree: Cofree[F, A]) extends AnyVal {
    def tuple: (A, F[Cofree[F, A]]) = Cofree.uncofree(cofree)
    def head: A = tuple._1
    def tail: F[Cofree[F, A]] = tuple._2

    def toCats(implicit ev: Functor[F]): cats.free.Cofree[F, A] =
      cats.free.Cofree(head, Eval.later(tail.map(_.toCats)))
  }

  type EnvT[E, W[_], A] // = (E, W[A])

  object EnvT {
    def apply [E, W[_], A](ask: E, lower: W[A]): EnvT[E, W, A] = apply((ask, lower))
    def apply [E, W[_], A](f: (E, W[A])): EnvT[E, W, A] = macro Meta.fastCast
    def envT  [E, W[_], A](f: (E, W[A])): EnvT[E, W, A] = macro Meta.fastCast
    def unenvT[E, W[_], A](f: EnvT[E, W, A]): (E, W[A]) = macro Meta.fastCast
  }

  implicit class EnvTOps[E, W[_], A](val envT: EnvT[E, W, A]) extends AnyVal {
    def tuple: (E, W[A]) = EnvT.unenvT(envT)
    def ask: E = tuple._1
    def lower: W[A] = tuple._2
  }

  implicit def drosteEnvTFunctor[E, W[_]: Functor]: Functor[EnvT[E, W, ?]] =
    new Functor[EnvT[E, W, ?]] {
      def map[A, B](fa: EnvT[E, W, A])(f: A => B): EnvT[E, W, B] =
        EnvT(fa.ask, fa.lower.map(f))
    }

  implicit def drosteEnvTTraverse[E, W[_]: Traverse]: Traverse[EnvT[E, W, ?]] =
    new DefaultTraverse[EnvT[E, W, ?]] {
      def traverse[G[_]: Applicative, A, B](fa: EnvT[E, W, A])(f: A => G[B]): G[EnvT[E, W, B]] =
        fa.lower.traverse(f).map(EnvT(fa.ask, _))

      override def map[A, B](fa: EnvT[E, W, A])(f: A => B): EnvT[E, W, B] =
        EnvT(fa.ask, fa.lower.map(f))
    }

  /** Mu is the least fixed point of a functor `F`. It is a
    * computation that can consume a inductive noninfinite
    * structure in one go.
    *
    * In Haskell this can more aptly be expressed as:
    * `data Mu f = Mu (forall x . (f x -> x) -> x)`
    */
  abstract class Mu[F[_]] extends (Algebra[F, ?] ~> Id)

  object Mu {
    def algebra[F[_]: Functor]: Algebra[F, Mu[F]] =
      fmf => new Mu[F] {
        def apply[A](fold: Algebra[F, A]): Id[A] =
          fold(fmf map (mf => mf(fold)))
      }

    def coalgebra[F[_]: Functor]: Coalgebra[F, Mu[F]] =
      mf => mf[F[Mu[F]]](_ map algebra)
  }

  /** Nu is the greatest fixed point of a functor `F`. It is a
    * computation that can generate a coinductive infinite
    * structure on demand.
    *
    * In Haskell this can more aptly be expressed as:
    * `data Nu g = forall s . Nu (s -> g s) s`
    */
  sealed abstract class Nu[F[_]] {
    type A = F[Nu[F]]
    def  unfold: Coalgebra[F, A]
    def  a     : A
  }

  object Nu {
    def apply[F[_]](unfold0: Coalgebra[F, F[Nu[F]]], a0: F[Nu[F]]): Nu[F] = new Nu[F] {
      val  unfold = unfold0
      val  a      = a0
    }

    // [F, F[Nu[F]]]
    def algebra[F[_]: Functor]: Algebra[F, Nu[F]] =
      t => Nu((_: F[Nu[F]]) map coalgebra, t)

    def coalgebra[F[_]: Functor]: Coalgebra[F, Nu[F]] =
      nf => nf.unfold(nf.a) map (Nu(nf.unfold, _))
  }

}

package qq.droste
package data

import cats.~>
import cats.Functor

/** The Yoneda aka cofree functor for `F`.
  *
  * `Yo[F,A]` is isomorphic to `F[A]` but helps optimize recursion
  * schemes by fusing multiple map operations on a functor `F`.
  *
  * This is named `Yo` instead of `Yoneda` to avoid conflicts with
  * other libraries.
  */
abstract class Yo[F[_], A] extends Serializable { self =>
  def apply[B](f: A => B): F[B]

  def run: F[A] = apply(a => a)

  def map[B](f: A => B): Yo[F, B] =
    new Yo[F, B] {
      def apply[C](g: B => C): F[C] = self(f andThen g)
    }

  def mapK[G[_]](f: F ~> G): Yo[G, A] =
    new Yo[G, A] {
      def apply[B](g: A => B): G[B] = f(self(g))
    }
}

object Yo {
  def apply[F[_], A](fa: F[A])(implicit F: Functor[F]): Yo[F, A] =
    new Yo[F, A] {
      def apply[B](f: A => B): F[B] = F.map(fa)(f)
    }

  implicit def yonedaFunctor[F[_]]: Functor[Yo[F, ?]] =
    new Functor[Yo[F, ?]] {
      def map[A, B](ya: Yo[F, A])(f: A => B): Yo[F, B] = ya map f
    }
}

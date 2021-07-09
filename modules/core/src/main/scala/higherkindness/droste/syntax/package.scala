package higherkindness.droste
package syntax

import cats.Applicative
import cats.Foldable
import cats.Monad
import cats.kernel.Monoid
import cats.kernel.Eq

import higherkindness.droste.data.AttrF
import higherkindness.droste.data.Fix
import higherkindness.droste.data.list._

object all
    extends ComposeSyntax
    with AttrSyntax
    with LiftSyntax
    with FixSyntax
    with UnfixSyntax
    with EmbedSyntax
    with ProjectSyntax

object compose extends ComposeSyntax
object attr    extends AttrSyntax
object lift    extends LiftSyntax
object fix     extends FixSyntax
object unfix   extends UnfixSyntax
object embed   extends EmbedSyntax
object project extends ProjectSyntax

sealed trait ComposeSyntax {

  /** Compose two functors `F` and `G`.
    *
    * This allows you to inline what would otherwise require
    * a type alias.
    *
    * Consider the method
    * {{{
    * def method[F[_]]: Unit = ()
    * }}}
    *
    * Calling method with a simple type is easy:
    * {{{
    * method[Option]
    * }}}
    *
    * Calling method with a nested type is more complicated.
    * The traditional way to do this is to use an alias:
    * {{{
    * type ListOption[A] = List[Option[A]]
    * method[ListOption]
    * }}}
    *
    * This type provides a convenient (maybe?) way to inline
    * the above type:
    * {{{
    * method[(List ∘ Option)#λ]
    * }}}
    *
    * Note: If you have the kind projector plugin enabled, this alias
    * is equivalent to:
    * {{{
    * method[λ[α => List[Option[α]]]]
    * }}}
    */
  type ∘[F[_], G[_]] = { type λ[α] = F[G[α]] }
}

sealed trait AttrSyntax {
  implicit def toAttrySyntaxOps[V[_], A](lower: V[A]): AttrSyntax.Ops[V, A] =
    new AttrSyntax.Ops(lower)
}

object AttrSyntax {
  final class Ops[F[_], B](private val lower: F[B]) extends AnyVal {
    implicit def attr[G[a] >: F[a], A](ask: A): AttrF[G, A, B] =
      AttrF(ask, lower)
  }
}

sealed trait LiftSyntax {
  implicit def toLiftSyntaxOps[A, B](f: A => B): LiftSyntax.Ops[A, B] =
    new LiftSyntax.Ops(f)
}

object LiftSyntax {
  final class Ops[A, B](private val f: A => B) extends AnyVal {
    def lift[F[_]](implicit F: Applicative[F]): A => F[B] = a => F.pure(f(a))
  }
}

sealed trait FixSyntax {
  implicit def toFixSyntaxOps[F[_], A](unfix: F[A]): FixSyntax.Ops[F, A] =
    new FixSyntax.Ops(unfix)
}

object FixSyntax {
  final class Ops[F[_], A](private val unfix: F[A]) extends AnyVal {
    def fix[G[a] >: F[a]](implicit ev: A =:= Fix[G]): Fix[G] = {
      val _ =
        ev // suppress 'unused' warnings; can't use `nowarn` because the warning doesn't appear on all cross versions
      Fix(unfix.asInstanceOf[G[Fix[G]]])
    }
  }
}

sealed trait UnfixSyntax {
  implicit def toUnfixSyntaxOps[F[_]](fix: Fix[F]): UnfixSyntax.Ops[F] =
    new UnfixSyntax.Ops(fix)
}

object UnfixSyntax {
  final class Ops[F[_]](private val fix: Fix[F]) extends AnyVal {
    def unfix: F[Fix[F]] = Fix.un(fix)
  }
}

sealed trait EmbedSyntax {
  implicit def toEmbedSyntaxOps[F[_], T](
      t: F[T]
  )(implicit E: Embed[F, T]): EmbedSyntax.Ops[F, T] =
    new EmbedSyntax.Ops[F, T] {
      def Embed = E
      def self  = t
    }
}

object EmbedSyntax {
  trait Ops[F[_], T] {
    def Embed: Embed[F, T]
    def self: F[T]

    def embed: T = Embed.algebra(self)
  }
}

sealed trait ProjectSyntax {
  implicit def toProjectSyntaxOps[F[_], T](
      t: T
  )(implicit PFT: Project[F, T]): ProjectSyntax.ProjectOps[F, T] =
    new ProjectSyntax.ProjectOps[F, T] {
      def P    = PFT
      def self = t
    }

  implicit def toFoldableProjectSyntaxOps[F[_], T](t: T)(implicit
      PFT: Project[F, T],
      FF: Foldable[F]
  ): ProjectSyntax.ProjectFoldableOps[F, T] =
    new ProjectSyntax.ProjectFoldableOps[F, T] {
      def P    = PFT
      def F    = FF
      def self = t
    }
}

object ProjectSyntax {
  trait ProjectOps[F[_], T] {

    implicit def P: Project[F, T]

    def self: T

    def project: F[T] = P.coalgebra(self)

  }

  trait ProjectFoldableOps[F[_], T] extends ProjectOps[F, T] {

    implicit def F: Foldable[F]

    def all(p: T => Boolean): Boolean =
      Project.all(self)(p)

    def any(p: T => Boolean): Boolean =
      Project.any(self)(p)

    def collect[U: Monoid, B](pf: PartialFunction[T, B])(implicit
        U: Basis[ListF[B, *], U]
    ): U =
      Project.collect[F, T, U, B](self)(pf)

    def contains(c: T)(implicit T: Eq[T]): Boolean =
      Project.contains(self, c)

    def foldMap[Z: Monoid](f: T => Z): Z =
      Project.foldMap(self)(f)

    def foldMapM[M[_], Z](
        f: T => M[Z]
    )(implicit M: Monad[M], Z: Monoid[Z]): M[Z] =
      Project.foldMapM(self)(f)
  }
}

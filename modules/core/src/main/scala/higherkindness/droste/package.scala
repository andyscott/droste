package higherkindness.droste

import cats.~>
import cats.Eq

import higherkindness.droste.data.Attr
import higherkindness.droste.data.Coattr
import higherkindness.droste.syntax.compose._

object `package` {

  type Algebra[F[_], A]   = GAlgebra[F, A, A]
  type Coalgebra[F[_], A] = GCoalgebra[F, A, A]

  type AlgebraM[M[_], F[_], A]   = GAlgebraM[M, F, A, A]
  type CoalgebraM[M[_], F[_], A] = GCoalgebraM[M, F, A, A]

  type RAlgebra[R, F[_], A]   = GAlgebra[F, (R, A), A]
  type RCoalgebra[R, F[_], A] = GCoalgebra[F, A, Either[R, A]]

  type RAlgebraM[R, M[_], F[_], A]   = GAlgebraM[M, F, (R, A), A]
  type RCoalgebraM[R, M[_], F[_], A] = GCoalgebraM[M, F, A, Either[R, A]]

  type CVAlgebra[F[_], A]   = GAlgebra[F, Attr[F, A], A]
  type CVCoalgebra[F[_], A] = GCoalgebra[F, A, Coattr[F, A]]

  type Gather[F[_], S, A]  = (A, F[S]) => S
  type Scatter[F[_], A, S] = S => Either[A, F[S]]

  object Algebra {
    def apply[F[_], A](f: F[A] => A): Algebra[F, A] = GAlgebra(f)
  }

  object Coalgebra {
    def apply[F[_], A](f: A => F[A]): Coalgebra[F, A] = GCoalgebra(f)
  }

  object AlgebraM {
    def apply[M[_], F[_], A](f: F[A] => M[A]): AlgebraM[M, F, A] = GAlgebraM(f)
  }

  object CoalgebraM {
    def apply[M[_], F[_], A](f: A => M[F[A]]): CoalgebraM[M, F, A] =
      GCoalgebraM(f)
  }

  object RAlgebra {
    def apply[R, F[_], A](f: F[(R, A)] => A): RAlgebra[R, F, A] = GAlgebra(f)
  }

  object RCoalgebra {
    def apply[R, F[_], A](f: A => F[Either[R, A]]): RCoalgebra[R, F, A] =
      GCoalgebra(f)
  }

  object RAlgebraM {
    def apply[R, M[_], F[_], A](f: F[(R, A)] => M[A]): RAlgebraM[R, M, F, A] =
      GAlgebraM(f)
  }

  object RCoalgebraM {
    def apply[R, M[_], F[_], A](
        f: A => M[F[Either[R, A]]]
    ): RCoalgebraM[R, M, F, A] =
      GCoalgebraM(f)
  }

  object CVAlgebra {
    def apply[F[_], A](f: F[Attr[F, A]] => A): CVAlgebra[F, A] = GAlgebra(f)
  }

  object CVCoalgebra {
    def apply[F[_], A](f: A => F[Coattr[F, A]]): CVCoalgebra[F, A] =
      GCoalgebra(f)
  }

  type Trans[F[_], G[_], A]        = GTrans[F, G, A, A]
  type TransM[M[_], F[_], G[_], A] = GTransM[M, F, G, A, A]

  object Trans {
    def apply[F[_], G[_], A](f: F[A] => G[A]): Trans[F, G, A] = GTrans(f)
  }

  object TransM {
    def apply[M[_], F[_], G[_], A](f: F[A] => M[G[A]]): TransM[M, F, G, A] =
      GTransM(f)
  }

  type Delay[F[_], G[_]] = F ~> (F ∘ G)#λ
}

object prelude {
  implicit def drosteDelayedEq[Z, F[_]](implicit
      p: Project[F, Z],
      delay: Delay[Eq, F]
  ): Eq[Z] = {
    lazy val knot: Eq[Z] =
      Eq.instance((x, y) => delay(knot).eqv(p.coalgebra(x), p.coalgebra(y)))
    knot
  }

  // todo: where should this live?
  implicit val drosteDelayEqOption: Delay[Eq, Option] =
    new (Eq ~> (Eq ∘ Option)#λ) {
      def apply[A](eq: Eq[A]): (Eq ∘ Option)#λ[A] =
        Eq.instance((x, y) =>
          x.fold(y.fold(true)(_ => false))(xx =>
            y.fold(false)(yy => eq.eqv(xx, yy))
          )
        )
    }
}

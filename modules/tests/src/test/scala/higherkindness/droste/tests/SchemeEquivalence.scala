package higherkindness.droste
package tests

import cats.Functor

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop._

import examples.histo.MakeChange

final class SchemeEquivalence extends Properties("SchemeEquivalence") {

  // TODO: see about generalizing this for testing more scheme equivalences
  trait AlgebraFamily {
    type F[_]
    type R
    type B

    object implicits {
      implicit def implicitFunctorF: Functor[F]     = functorF
      implicit def implicitProjectFR: Project[F, R] = projectFR
      implicit def implicitArbitraryR: Arbitrary[R] = arbitraryR
    }

    def functorF: Functor[F]
    def projectFR: Project[F, R]
    def arbitraryR: Arbitrary[R]

    def cvalgebra: CVAlgebra[F, B]
  }

  object AlgebraFamily {
    case class Default[FF[_], RR, BB](
        genR: Gen[RR],
        cvalgebra: CVAlgebra[FF, BB]
    )(implicit
        val functorF: Functor[FF],
        val projectFR: Project[FF, RR]
    ) extends AlgebraFamily {
      type F[A] = FF[A]
      type R    = RR
      type B    = BB

      val arbitraryR = Arbitrary(genR)
    }
  }

  implicit val arbitraryAlgebraFamily: Arbitrary[AlgebraFamily] =
    Arbitrary(
      Gen.const(
        AlgebraFamily.Default(
          Gen.choose(0, 50).map(MakeChange.toNat),
          MakeChange.makeChangeAlgebra
        )
      )
    )

  property("histo") = {

    forAll { (z: AlgebraFamily) =>
      import z.implicits._

      val f = scheme.zoo.histo(z.cvalgebra)
      val g = scheme.gcata(z.cvalgebra.gather(Gather.histo))
      val h = scheme.ghylo(
        z.cvalgebra.gather(Gather.histo),
        z.projectFR.coalgebra.scatter(Scatter.ana)
      )

      forAll { (r: z.R) =>
        val x = f(r)
        val y = g(r)
        val z = h(r)

        (x ?= y) && (x ?= z)
      }
    }
  }

}

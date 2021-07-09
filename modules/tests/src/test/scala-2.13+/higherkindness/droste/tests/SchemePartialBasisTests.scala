package higherkindness.droste
package tests

import org.scalacheck.Properties
import org.scalacheck.Prop._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.scalacheck.numeric._

import cats.Eval
import cats.syntax.eq._
import higherkindness.droste.data.prelude._
import higherkindness.droste.data.Attr
import higherkindness.droste.data.CoattrF
import higherkindness.droste.data.Coattr
import higherkindness.droste.data.Fix
import higherkindness.droste.data.Mu
import higherkindness.droste.data.Nu
import higherkindness.droste.data.list._

import higherkindness.droste.syntax.attr._
import higherkindness.droste.prelude._

class SchemePartialBasisTests extends Properties("SchemePartialBasis") {
  val sumListFIntAlgebra: Algebra[ListF[Int, *], Int] = Algebra {
    case ConsF(x, y) => x + y
    case NilF        => 0
  }

  def sumListInt(l: List[Int]): Int = l.foldLeft(0)(_ + _)

  property("scheme[Fix].ana") = {

    val f =
      scheme[Fix].ana(Coalgebra((n: Int) => if (n > 0) Some(n - 1) else None))

    def expected(n: Int): Fix[Option] =
      if (n > 0) Fix(Some(expected(n - 1)))
      else Fix(None: Option[Fix[Option]])

    forAll((n: Int Refined Less[100]) => f(n) ?= expected(n))
  }

  property("scheme[Attr[*[_], Int]].ana") = {

    val f = scheme[({ type L[F[_]] = Attr[F, Int] })#L]
      .ana(Coalgebra((n: Int) => (if (n > 0) Some(n - 1) else None) attr n))

    def expected(n: Int): Attr[Option, Int] =
      if (n > 0) Attr(n, Some(expected(n - 1)))
      else Attr(n, None: Option[Attr[Option, Int]])

    forAll((n: Int Refined Less[100]) => f(n) ?= expected(n))
  }

  property("scheme[cats.free.Cofree[*[_], Int]].ana") = {

    val f = scheme[({ type L[F[_]] = cats.free.Cofree[F, Int] })#L]
      .ana(Coalgebra((n: Int) => (if (n > 0) Some(n - 1) else None) attr n))

    def expected(n: Int): cats.free.Cofree[Option, Int] =
      if (n > 0) cats.free.Cofree(n, Eval.now(Some(expected(n - 1))))
      else
        cats.free
          .Cofree(n, Eval.now(None: Option[cats.free.Cofree[Option, Int]]))

    forAll((n: Int Refined Less[100]) => f(n) ?= expected(n))
  }

  property("scheme[cats.free.Free[*[_], Int]].ana") = {

    val f = scheme[({ type L[F[_]] = cats.free.Free[F, Int] })#L].ana(
      Coalgebra((n: Int) =>
        if (n > 0) CoattrF.roll[Option, Int, Int](Some(n - 1))
        else CoattrF.pure[Option, Int, Int](n)
      )
    )

    def expected(n: Int): cats.free.Free[Option, Int] =
      if (n > 0)
        cats.free.Free.roll(Some(expected(n - 1)))
      else
        cats.free.Free.pure(n)

    forAll((n: Int Refined Less[100]) =>
      Coattr.fromCats(f(n)) ?= Coattr.fromCats(expected(n))
    )
  }

  property("scheme[Mu].ana") = {

    val f =
      scheme[Mu].ana(Coalgebra((n: Int) => if (n > 0) Some(n - 1) else None))

    def expected(n: Int): Mu[Option] =
      if (n > 0) Mu(Some(expected(n - 1)))
      else Mu(None: Option[Mu[Option]])

    forAll((n: Int Refined Less[100]) => f(n) ?= expected(n))
  }

  property("scheme[Nu].ana") = {

    val f =
      scheme[Nu].ana(Coalgebra((n: Int) => if (n > 0) Some(n - 1) else None))

    def expected(n: Int): Nu[Option] =
      if (n > 0) Nu(Some(expected(n - 1)))
      else Nu(None: Option[Nu[Option]])

    forAll((n: Int Refined Less[100]) => f(n) === expected(n))
  }

  property("scheme[Mu].cata") = {

    val f = scheme[Mu].cata(sumListFIntAlgebra)

    forAll((l: List[Int]) =>
      f(ListF.fromScalaList[Int, Mu](l)) ?= sumListInt(l)
    )
  }

  property("scheme[Mu].gcata") = {

    val f = scheme[Mu].gcata(sumListFIntAlgebra.gather(Gather.cata))

    forAll((l: List[Int]) =>
      f(ListF.fromScalaList[Int, Mu](l)) ?= sumListInt(l)
    )
  }

  property("scheme[Mu].gcataM") = {

    val f = scheme[Mu].gcataM(sumListFIntAlgebra.lift[Eval].gather(Gather.cata))

    forAll((l: List[Int]) =>
      f(ListF.fromScalaList[Int, Mu](l)).value ?= sumListInt(l)
    )
  }

  property("scheme[Mu].gana") = {

    val f = scheme[Mu].gana(
      Coalgebra((n: Int) => if (n > 0) Some(n - 1) else None)
        .scatter(Scatter.ana)
    )

    def expected(n: Int): Mu[Option] =
      if (n > 0) Mu(Some(expected(n - 1)))
      else Mu(None: Option[Mu[Option]])

    forAll((n: Int Refined Less[100]) => f(n) ?= expected(n))
  }

  property("scheme[Mu].ganaM") = {

    val f = scheme[Mu].ganaM(
      Coalgebra((n: Int) => if (n > 0) Some(n - 1) else None)
        .lift[Eval]
        .scatter(Scatter.ana)
    )

    def expected(n: Int): Mu[Option] =
      if (n > 0) Mu(Some(expected(n - 1)))
      else Mu(None: Option[Mu[Option]])

    forAll((n: Int Refined Less[100]) => f(n).value ?= expected(n))
  }
}

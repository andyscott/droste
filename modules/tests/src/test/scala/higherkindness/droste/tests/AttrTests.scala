package higherkindness.droste
package tests

import prelude._
import higherkindness.droste.data.Attr
import higherkindness.droste.data.AttrF
import higherkindness.droste.data.prelude._
import laws.BasisLaws
import scalacheck._

import org.scalacheck.Properties
import org.scalacheck.Prop._

import cats.laws.discipline.TraverseTests

final class AttrTests extends Properties("Attr/AttrF") {

  include(
    BasisLaws.props[AttrF[Option, Int, *], Attr[Option, Int]](
      "AttrF[Int, Option, *]",
      "Attr[Option, Int]"
    )
  )

  include(
    TraverseTests[AttrF[Option, Int, *]]
      .traverse[Int, Int, Int, Int, Option, Option]
      .all
  )

  property("unapply") = {
    forAll((x: Attr[Option, Int]) =>
      x match {
        case Attr((i, fa)) => x ?= Attr(i, fa)
      }
    )
  }

}

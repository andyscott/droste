package higherkindness.droste
package tests

import prelude._
import higherkindness.droste.data.prelude._
import higherkindness.droste.data.AttrF

import cats.free.Cofree

import laws.BasisLaws
import scalacheck._

import org.scalacheck.Properties

final class CofreeTests extends Properties("Cofree") {

  include(
    BasisLaws.props[AttrF[Option, Int, *], Cofree[Option, Int]](
      "AttrF[Option, Int, *]",
      "Cofree[Option, Int]"
    )
  )
}

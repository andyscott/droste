package higherkindness.droste
package tests

import prelude._
import higherkindness.droste.data.prelude._
import higherkindness.droste.data.CoattrF

import cats.free.Free

import laws.BasisLaws
import scalacheck._

import org.scalacheck.Properties

final class FreeTests extends Properties("Free") {

  include(
    BasisLaws.props[CoattrF[Option, Int, *], Free[Option, Int]](
      "CoattrF[Option, Int, *]",
      "Free[Option, Int]"
    )
  )

}

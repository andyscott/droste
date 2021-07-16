package higherkindness.droste
package tests

import prelude._
import higherkindness.droste.data.Fix
import laws.BasisLaws
import scalacheck._

import org.scalacheck.Properties
import org.scalacheck.Prop._

final class FixTests extends Properties("Fix") {

  include(BasisLaws.props[Option, Fix[Option]]("Option", "Fix[Option]"))

  property("unapply") = {
    forAll((x: Fix[Option]) =>
      x match {
        case Fix(y) => y ?= Fix.un(x)
      }
    )
  }
}

package higherkindness.droste
package tests

import prelude._
import higherkindness.droste.data.Nu
import laws.BasisLaws
import scalacheck._

import org.scalacheck.Properties
import org.scalacheck.Prop._

import cats.syntax.eq._

final class NuTests extends Properties("Nu") {

  include(BasisLaws.props[Option, Nu[Option]]("Option", "Nu[Option]"))

  property("unapply") = {
    forAll((x: Nu[Option]) =>
      x match {
        case Nu(y) => y === Nu.un(x)
      }
    )
  }
}

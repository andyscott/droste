package higherkindness.droste
package tests

import higherkindness.droste.data.CoattrF
import scalacheck._

import org.scalacheck.Properties
import org.scalacheck.Prop._

import scala.annotation.nowarn

@nowarn("msg=match may not be exhaustive")
final class CoattrFTests extends Properties("CoattrF") {

  property("unapply") = {
    forAll((x: CoattrF[Option, Int, Long]) =>
      x match {
        case CoattrF.Pure(i)  => CoattrF.un(x).left.toOption ?= Some(i)
        case CoattrF.Roll(fi) => CoattrF.un(x).toOption ?= Some(fi)
      }
    )
  }
}

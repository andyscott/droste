package higherkindness.droste
package tests

import higherkindness.droste.data.Coattr
import scalacheck._

import org.scalacheck.Properties
import org.scalacheck.Prop._

import scala.annotation.nowarn

@nowarn("msg=match may not be exhaustive")
final class CoattrTests extends Properties("Coattr") {

  property("unapply") = {
    forAll((x: Coattr[Option, Int]) =>
      x match {
        case Coattr.Pure(i)  => Coattr.un(x).left.toOption ?= Some(i)
        case Coattr.Roll(fi) => Coattr.un(x).toOption ?= Some(fi)
      }
    )
  }
}

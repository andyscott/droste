package higherkindness.droste
package tests

import org.scalacheck.Properties
import org.scalacheck.Prop._

import higherkindness.droste.data.list._
import higherkindness.droste.data.:<

final class WorkDone extends Properties("WorkDone") {

  final class Sketch(var value: Int = 0)

  def cataAlgebra(sketch: Sketch): Algebra[ListF[Unit, *], Int] = Algebra {
    case NilF =>
      sketch.value += 1
      0
    case ConsF(_, n) =>
      sketch.value += 1
      n + 1
  }

  property("cata vs gcata") = {
    forAll { (list: List[Unit]) =>
      val sf = new Sketch()
      val sg = new Sketch()

      val rf = scheme.cata(cataAlgebra(sf)).apply(list)
      val rg = scheme.gcata(cataAlgebra(sg))(Gather.cata).apply(list)

      ((rf ?= rg) :| "same result") &&
      ((sf.value ?= sg.value) :| "same work")
    }
  }

  def histoAlgebra(sketch: Sketch): CVAlgebra[ListF[Unit, *], Int] = CVAlgebra {
    case NilF =>
      sketch.value += 1
      0
    case ConsF(_, n :< _) =>
      sketch.value += 1
      n + 1
  }

  property("histo vs gcata") = {

    forAll { (list: List[Unit]) =>
      val sf = new Sketch()
      val sg = new Sketch()

      val rf = scheme.zoo.histo(histoAlgebra(sf)).apply(list)
      val rg = scheme.gcata(histoAlgebra(sg))(Gather.histo).apply(list)

      ((rf ?= rg) :| "same result") &&
      ((sf.value ?= sg.value) :| "same work")
    }

  }

}

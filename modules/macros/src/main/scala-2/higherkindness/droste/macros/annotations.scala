package higherkindness.droste.macros

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

import impl.Macros

@compileTimeOnly("enable macro paradise to expand macro annotations")
class deriveFixedPoint extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.deriveFixedPoint
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class deriveTraverse extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.deriveTraverse
}

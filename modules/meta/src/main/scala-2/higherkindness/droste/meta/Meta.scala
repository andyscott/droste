package higherkindness.droste
package meta

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

class Meta(val c: blackbox.Context) {
  import c.universe._

  @tailrec private def unroll(f: Tree): Tree = {
    val ff = f match {
      case Typed(u, _) => u
      case _           => f
    }
    val fff = ff match {
      case TypeApply(Select(u, TermName("asInstanceOf")), _) => u
      case _                                                 => ff
    }

    if (fff != f) unroll(fff) else f
  }

  def fastCast(f: Tree): Tree =
    if (f.tpe <:< c.macroApplication.tpe)
      q"${unroll(f)}"
    else
      q"${unroll(f)}.asInstanceOf[${c.macroApplication.tpe}]"
}

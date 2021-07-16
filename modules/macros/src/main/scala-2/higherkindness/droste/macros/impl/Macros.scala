package higherkindness.droste.macros.impl

import scala.reflect.macros.blackbox
import scala.annotation.nowarn
import scala.annotation.tailrec

object Macros {
  def deriveTraverse(c: blackbox.Context)(
      annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    val inputs          = annottees.map(_.tree).toList
    val clait: ClassDef = inputs.collect({ case c: ClassDef => c }).head
    val companion: ModuleDef = inputs
      .collectFirst { case c: ModuleDef => c }
      .getOrElse(
        ModuleDef(
          NoMods,
          clait.name.toTermName,
          Template(
            List(TypeTree(typeOf[AnyRef])),
            noSelfType,
            List(
              DefDef(
                Modifiers(),
                termNames.CONSTRUCTOR,
                List(),
                List(List()),
                TypeTree(),
                Block(List(pendingSuperCall), Literal(Constant(())))))
          )
        )
      )

    def isAdt(classOrTrait: ClassDef): Boolean =
      // is a trait
      classOrTrait.mods.hasFlag(TRAIT) ||
        // is an abstract class
        (classOrTrait.mods.hasFlag(ABSTRACT) && classOrTrait.mods.hasFlag(
          SEALED))

    def canDerive(classOrTrait: ClassDef): Boolean =
      isAdt(classOrTrait) ||
        // is a case class
        classOrTrait.mods.hasFlag(CASE)

    /**
      * returns wether c extends d
      */
    def xtends(c: ClassDef, d: ClassDef): Boolean =
      c.impl.parents
        .collect({ case a: AppliedTypeTree => a })
        .map(_.tpt)
        .collect({ case i: Ident => i })
        .exists { a =>
          a.name == d.name
        }

    val isCase: PartialFunction[Tree, ClassDef] = {
      case c: ClassDef if c.mods.hasFlag(CASE) && xtends(c, clait) => c
    }

    val AdtCases =
      if (isAdt(clait))
        // Get all the cases of the ADT
        companion.impl.body.collect(isCase)
      else
        // we're annotating a case class then
        List(clait)

    def getCaseClassParams(caseClass: ClassDef): List[ValDef] =
      caseClass.impl.body
        .collect({
          case v: ValDef
              if v.mods.hasFlag(PARAMACCESSOR) && v.mods.hasFlag(
                CASEACCESSOR) =>
            v
        })

    def traverseInstance(λ: TypeName): ValDef = {
      val G  = c.freshName(TypeName("G"))
      val AA = c.freshName(TypeName("AA"))
      val B  = c.freshName(TypeName("B"))

      val cases = AdtCases map { origin =>
        val name       = TermName(origin.name.toString)
        val params     = getCaseClassParams(origin)
        val arity      = params.length
        val freshTerms = List.fill(arity)(TermName(c.freshName()))
        val binds      = freshTerms.map(x => Bind(x, Ident(termNames.WILDCARD)))
        val args = params.zip(freshTerms).map {
          case (valDef, t) =>
            val RecType = origin.tparams.head.name.toString

            if (valDef.tpt.toString == RecType) {
              q"fn($t)"
            } else if (valDef.tpt.toString.contains(RecType)) {
              val T = valDef.tpt.asInstanceOf[AppliedTypeTree]

              /**
                * Used to get the correct Traverse instance.  In case of
                * finding nested AppliedTypeTrees, tries to compose the
                * Traverse instances to get a suitable traverse.
                *
                * For example, finding List[Option[Future[Try[A]]]] will
                * generate something like:
                *
                * Traverse[List].compose[Option].compose[Future].compose[Try]
                */
              def getTraverseInstance(tt: AppliedTypeTree): Tree = {
                @nowarn("msg=match may not be exhaustive")
                @tailrec def go(ttt: AppliedTypeTree, acc: Tree): Tree =
                  ttt match {
                    case AppliedTypeTree(
                        _: Ident,
                        List(tttt @ AppliedTypeTree(b: Ident, _))) =>
                      go(tttt, q"$acc.compose[$b]")
                    case AppliedTypeTree(_: Ident, _) =>
                      acc
                  }

                go(tt, q"_root_.cats.Traverse[${tt.tpt}]")
              }

              q"${getTraverseInstance(T)}.traverse($t)(fn)"
            } else {
              q"_root_.cats.Applicative[$G].pure($t)"
            }
        }
        val body = if (arity > 1) {
          val mapN: TermName = TermName(s"map${arity.toString}")
          q"_root_.cats.Applicative[$G].$mapN(..$args)($name.apply[$B])"
        } else if (arity == 1) {
          q"_root_.cats.Applicative[$G].map($args.head)($name.apply[$B])"
        } else {
          q"_root_.cats.Applicative[$G].pure($name[$B]())"
        }

        cq"$name(..$binds) => $body"
      }

      val mtch = Match(Ident(TermName("fa")), cases)

      q"""
      implicit val traverseInstance: _root_.cats.Traverse[$λ] = new _root_.higherkindness.droste.util.DefaultTraverse[$λ] {
        def traverse[$G[_]: _root_.cats.Applicative, $AA, $B](fa: $λ[$AA])(fn: $AA => $G[$B]): $G[$λ[$B]] = $mtch
      }
      """
    }

    val paramNames = clait.tparams match {
      case Nil      => Nil
      case nonEmpty => nonEmpty.reverse.tail.reverse.map(_.name)
    }

    val λ: TypeDef = q"type λ[α] = ${clait.name}[..$paramNames, α]"

    val outputs =
      clait match {
        case classOrTrait: ClassDef if canDerive(classOrTrait) =>
          List(
            clait,
            ModuleDef(
              companion.mods,
              companion.name,
              Template(
                companion.impl.parents,
                companion.impl.self,
                companion.impl.body :+ λ :+ traverseInstance(λ.name)
              )
            )
          )

        case _ =>
          sys.error("@deriveTraverse should only annotate ADTs or case classes")
      }

    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }

  def deriveFixedPoint(c: blackbox.Context)(
      annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val inputs = annottees.map(_.tree).toList

    require(
      inputs.length == 2,
      "@deriveFixedPoint should annotate a sealed [trait|abstract class] with companion")

    val clait: ClassDef      = inputs.collect({ case c: ClassDef  => c }).head
    val companion: ModuleDef = inputs.collect({ case c: ModuleDef => c }).head

    val A: TypeName                = c.freshName(TypeName("A"))
    val claitTypeParams            = clait.tparams
    val claitTypeParamNames        = claitTypeParams.map(_.name)
    val claitTypeParamNamesAsTrees = claitTypeParams.map(x => Ident(x.name))
    val claitTypeParamNamesWithA   = claitTypeParamNames :+ A
    val claitTypeParamNamesWithAAsTrees =
      claitTypeParamNamesWithA.map(x => Ident(x))

    val NonRecursiveAdtName: TypeName = TypeName(s"${clait.name}F")
    val NonRecursiveAdtFullName: AppliedTypeTree =
      AppliedTypeTree(
        Ident(NonRecursiveAdtName),
        claitTypeParamNamesWithAAsTrees)

    def isSealed(classOrTrait: ClassDef): Boolean =
      classOrTrait.mods.hasFlag(TRAIT) ||
        classOrTrait.mods.hasFlag(ABSTRACT) &&
          classOrTrait.mods.hasFlag(SEALED)

    /**
      * returns wether c extends d
      */
    def xtends(c: Tree, d: ClassDef): Boolean =
      (c collect {
        case c: ClassDef =>
          c.impl.parents
            .collect({ case i: Ident => i })
            .exists(_.name == d.name)
        case c: ModuleDef =>
          c.impl.parents
            .collect({ case i: Ident => i })
            .exists(_.name == d.name)
      }).contains(true)

    val isCase: PartialFunction[Tree, Tree] = {
      case c: ClassDef if c.mods.hasFlag(CASE) && xtends(c, clait)  => c
      case c: ModuleDef if c.mods.hasFlag(CASE) && xtends(c, clait) => c
    }

    val AdtCases = companion.impl.body.collect(isCase)

    def substType(rec: Tree => Boolean): Tree => Tree = {
      case x: Ident if rec(x) => Ident(A)
      case x: AppliedTypeTree =>
        AppliedTypeTree(x.tpt, x.args.map(substType(rec)))
      case x => x
    }

    val convertCaseClassParam: ValDef => ValDef = { valDef =>
      val recursive: Tree => Boolean = _.toString == clait.name.toString

      ValDef(
        valDef.mods,
        valDef.name,
        substType(recursive)(valDef.tpt),
        valDef.rhs)
    }

    def convertCaseObject(module: ModuleDef): ClassDef = {
      val Name: TypeName = TypeName(s"${module.name}F")

      q"case class $Name[..${clait.tparams}, $A]() extends $NonRecursiveAdtFullName"
    }

    def getCaseClassParams(caseClass: ClassDef): List[ValDef] =
      caseClass.impl.body
        .collect({
          case v: ValDef
              if v.mods.hasFlag(PARAMACCESSOR) && v.mods.hasFlag(
                CASEACCESSOR) =>
            v
        })
        .map(convertCaseClassParam)

    def convertCaseClass(caseClass: ClassDef): ClassDef = {
      val Name: TypeName = TypeName(s"${caseClass.name}F")

      val params: List[ValDef] = getCaseClassParams(caseClass)

      q"case class $Name[..${clait.tparams}, $A](..$params) extends $NonRecursiveAdtFullName"
    }

    val NonRecursiveAdtCases: List[ClassDef] =
      AdtCases.map {
        case c: ModuleDef => convertCaseObject(c)
        case c: ClassDef  => convertCaseClass(c)
        case _            => sys.error("Nope")
      }

    val nonRecursiveAdt: ClassDef =
      q"""sealed trait $NonRecursiveAdtName[..${clait.tparams}, $A] extends Product with Serializable"""

    val λ: TypeDef =
      q"type λ[α] = $NonRecursiveAdtName[..${clait.tparams.map(_.name)}, α]"

    val traverseInstance: DefDef = {

      val G  = c.freshName(TypeName("G"))
      val AA = c.freshName(TypeName("AA"))
      val B  = c.freshName(TypeName("B"))

      val cases = NonRecursiveAdtCases map { origin =>
        val name       = TermName(origin.name.toString)
        val params     = getCaseClassParams(origin)
        val arity      = params.length
        val freshTerms = List.fill(arity)(TermName(c.freshName()))
        val binds      = freshTerms.map(x => Bind(x, Ident(termNames.WILDCARD)))
        val args = params.zip(freshTerms).map {
          case (valDef, t) =>
            if (valDef.tpt.toString == A.toString) {
              q"fn($t)"
            } else if (valDef.tpt.toString.contains(A.toString)) {
              val T = valDef.tpt.asInstanceOf[AppliedTypeTree].tpt
              q"_root_.cats.Traverse[$T].traverse($t)(fn)"
            } else {
              q"_root_.cats.Applicative[$G].pure($t)"
            }
        }
        val body = if (arity > 1) {
          val mapN: TermName = TermName(s"map${arity.toString}")
          q"_root_.cats.Applicative[$G].$mapN(..$args)($name.apply[$B])"
        } else if (arity == 1) {
          q"_root_.cats.Applicative[$G].map($args.head)($name.apply[$B])"
        } else {
          q"_root_.cats.Applicative[$G].pure($name[$B]())"
        }

        cq"$name(..$binds) => $body"
      }

      val mtch = Match(Ident(TermName("fa")), cases)

      q"""
      implicit def traverseInstance[..${clait.tparams}]: _root_.cats.Traverse[λ] = new _root_.higherkindness.droste.util.DefaultTraverse[λ] {
        def traverse[$G[_]: _root_.cats.Applicative, $AA, $B](fa: λ[$AA])(fn: $AA => $G[$B]): $G[λ[$B]] = $mtch
      }
      """
    }

    val toRecursive: DefDef = {
      @nowarn("msg=match may not be exhaustive")
      val embedAlgebraCases: List[CaseDef] =
        (NonRecursiveAdtCases zip AdtCases) map {
          case (origin, target: ClassDef) =>
            val originName = TermName(origin.name.toString)
            val targetName = TermName(target.name.toString)
            val freshTerms = List.fill(getCaseClassParams(target).length)(
              TermName(c.freshName()))
            val binds = freshTerms.map(x => Bind(x, Ident(termNames.WILDCARD)))
            val args  = freshTerms.map(x => Ident(x))
            cq"$originName(..$binds) => $targetName[..$claitTypeParamNamesAsTrees](..$args)"
          case (origin, target: ModuleDef) =>
            val originName = TermName(origin.name.toString)
            val targetName = TermName(target.name.toString)
            cq"$originName => $targetName"
        }

      val mtch = Match(EmptyTree, embedAlgebraCases)

      val algebra =
        q"""
        new _root_.higherkindness.droste.GAlgebra[λ, ${clait.name}[..$claitTypeParamNames], ${clait.name}[..$claitTypeParamNames]]($mtch)
        """

      q"def embedAlgebra[..${clait.tparams}]: _root_.higherkindness.droste.Algebra[λ, ${clait.name}[..$claitTypeParamNames]] = $algebra"

    }

    val toFixedPoint: DefDef = {
      @nowarn("msg=match may not be exhaustive")
      val embedAlgebraCases: List[CaseDef] =
        (AdtCases zip NonRecursiveAdtCases) map {
          case (origin: ClassDef, target) =>
            val originName = TermName(origin.name.toString)
            val targetName = TermName(target.name.toString)
            val freshTerms = List.fill(getCaseClassParams(target).length)(
              TermName(c.freshName()))
            val binds = freshTerms.map(x => Bind(x, Ident(termNames.WILDCARD)))
            val args  = freshTerms.map(x => Ident(x))

            cq"$originName(..$binds) => $targetName[..$claitTypeParamNamesAsTrees, ${clait.name}[..$claitTypeParamNames]](..$args)"
          case (origin: ModuleDef, target) =>
            val targetName = TermName(target.name.toString)
            val originName = TermName(origin.name.toString)
            cq"$originName => $targetName[..$claitTypeParamNamesAsTrees, ${clait.name}[..$claitTypeParamNames]]()"
        }

      val mtch = c.untypecheck(Match(EmptyTree, embedAlgebraCases))

      val algebra =
        q"""
        new _root_.higherkindness.droste.GCoalgebra[λ, ${clait.name}[..$claitTypeParamNames], ${clait.name}[..$claitTypeParamNames]]($mtch)
        """

      q"def projectCoalgebra[..${clait.tparams}]: _root_.higherkindness.droste.Coalgebra[λ, ${clait.name}[..$claitTypeParamNames]] = $algebra"
    }

    val basisInstance: DefDef = {
      q"""
      implicit def basisInstance[..${clait.tparams}]: _root_.higherkindness.droste.Basis[λ, ${clait.name}[..$claitTypeParamNames]] =
        _root_.higherkindness.droste.Basis.Default(embedAlgebra, projectCoalgebra)
      """
    }

    val deriveFixedPointModule: ModuleDef = q"""
      object fixedpoint {
        $nonRecursiveAdt
        ..$NonRecursiveAdtCases
        $λ
        $traverseInstance
        $toFixedPoint
        $toRecursive
        $basisInstance
      }
      """

    val outputs =
      clait match {
        case classOrTrait: ClassDef if isSealed(classOrTrait) =>
          List(
            clait,
            ModuleDef(
              companion.mods,
              companion.name,
              Template(
                companion.impl.parents,
                companion.impl.self,
                companion.impl.body :+ deriveFixedPointModule
              )
            )
          )

        case _ =>
          sys.error(
            "@deriveFixedPoint should only annotate sealed traits or sealed abstract classes")
      }

    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}

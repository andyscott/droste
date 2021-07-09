package higherkindness.droste
package examples.histo

import org.scalacheck.Properties
import org.scalacheck.Prop._

import cats.Applicative
import cats.Traverse
import cats.syntax.all._

import higherkindness.droste.data.Attr
import higherkindness.droste.data.Fix
import higherkindness.droste.util.DefaultTraverse
import higherkindness.droste.data.prelude._

/** Making change with histomorphisms.
  *
  * Ported from the blog post "Recursion Schemes, Part IV: Time is of
  * the Essence" by Patrick Thomson.
  *
  * https://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/
  */
final class MakeChange extends Properties("MakeChange") {
  import MakeChange._

  property("toNat") = {
    val ZeroFixed = Fix(Zero: Nat[Fix[Nat]])

    val a = toNat(0) ?= ZeroFixed
    val b = toNat(1) ?= Fix(Next(ZeroFixed))
    val c = toNat(2) ?= Fix(Next(Fix(Next(ZeroFixed))))
    val d = toNat(3) ?= Fix(Next(Fix(Next(Fix(Next(ZeroFixed))))))

    a && b && c && d
  }

  val solve = scheme.zoo.histo(makeChangeAlgebra)

  // dyna is the same as an ana followed by a histo
  val solveFused = scheme.zoo.dyna(makeChangeAlgebra, toNatCoalgebra)

  property("1 cent solutions") = solve(toNat(1)) ?= Set(Penny :: Nil)

  property("2 cent solutions") = solve(toNat(2)) ?= Set(Penny :: Penny :: Nil)

  property("3 cent solutions") =
    solve(toNat(3)) ?= Set(Penny :: Penny :: Penny :: Nil)

  property("4 cent solutions") =
    solve(toNat(4)) ?= Set(Penny :: Penny :: Penny :: Penny :: Nil)

  property("5 cent solutions") = solve(toNat(5)) ?= Set(
    Penny :: Penny :: Penny :: Penny :: Penny :: Nil,
    Nickle :: Nil
  )

  property("6 cent solutions") = solve(toNat(6)) ?= Set(
    Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Nil,
    Penny :: Nickle :: Nil
  )

  property("7 cent solutions") = solve(toNat(7)) ?= Set(
    Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Nil,
    Penny :: Penny :: Nickle :: Nil
  )

  property("8 cent solutions") = solve(toNat(8)) ?= Set(
    Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Nil,
    Penny :: Penny :: Penny :: Nickle :: Nil
  )

  property("9 cent solutions") = solve(toNat(9)) ?= Set(
    Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Nil,
    Penny :: Penny :: Penny :: Penny :: Nickle :: Nil
  )

  property("10 cent solutions") = solve(toNat(10)) ?= Set(
    Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Nil,
    Penny :: Penny :: Penny :: Penny :: Penny :: Nickle :: Nil,
    Nickle :: Nickle :: Nil,
    Dime :: Nil
  )

  property("11 cent solutions") = solve(toNat(11)) ?= Set(
    Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Nil,
    Penny :: Penny :: Penny :: Penny :: Penny :: Penny :: Nickle :: Nil,
    Penny :: Nickle :: Nickle :: Nil,
    Penny :: Dime :: Nil
  )

  property("num 10 cent solutions") = solveFused(10).size ?= 4

  property("num 25 cent solutions") = solveFused(25).size ?= 13

  property("num 50 cent solutions") = solveFused(50).size ?= 50

  property("num 100 cent solutions") = solveFused(100).size ?= 293

  property("num 200 cent solutions") = solveFused(200).size ?= 2728
}

object MakeChange {

  sealed abstract class Coin(val value: Int)
  case object Penny      extends Coin(1)
  case object Nickle     extends Coin(5)
  case object Dime       extends Coin(10)
  case object Quarter    extends Coin(25)
  case object HalfDollar extends Coin(50)
  case object Dollar     extends Coin(100)

  object Coin {
    implicit val coinOrdering: Ordering[Coin] = Ordering.by(_.value)
  }

  val allCoins: List[Coin] =
    List(Penny, Nickle, Dime, Quarter, HalfDollar, Dollar)

  sealed trait Nat[+A]
  object Nat {
    implicit val traverseForNat: Traverse[Nat] =
      new DefaultTraverse[Nat] {
        def traverse[G[_]: Applicative, A, B](
            fa: Nat[A]
        )(f: A => G[B]): G[Nat[B]] =
          fa match {
            case Zero    => (Zero: Nat[B]).pure[G]
            case Next(a) => f(a).map(Next(_))
          }
      }
  }
  case object Zero               extends Nat[Nothing]
  final case class Next[A](a: A) extends Nat[A]

  val toNatCoalgebra: Coalgebra[Nat, Int] =
    Coalgebra(n => if (n > 0) Next(n - 1) else Zero)

  val toNat: Int => Fix[Nat] =
    scheme.ana(toNatCoalgebra)

  val fromNat: Fix[Nat] => Int =
    scheme[Fix].cata[Nat, Int](Algebra {
      case Next(n) => n + 1
      case Zero    => 0
    })

  def lookup(cache: Attr[Nat, Set[List[Coin]]], n: Int): Set[List[Coin]] =
    if (n == 0) cache.head
    else
      cache.tail match {
        case Next(inner) => lookup(inner, n - 1)
        case Zero        => Set.empty
      }

  val makeChangeAlgebra: CVAlgebra[Nat, Set[List[Coin]]] = CVAlgebra {
    case Next(attr) =>
      val _given = fromNat(attr.forget) + 1
      val validCoins = allCoins
        .takeWhile(_.value <= _given)
        .map(coin => coin -> (_given - coin.value))
      val (zeros, toProcess) = validCoins.span(_._2 == 0)
      val zeroSolutions      = zeros.map(_._1 :: Nil).toSet
      val chainSolutions = toProcess
        .map(tp => lookup(attr, tp._1.value - 1).map(ps => tp._1 :: ps))
        .flatten
        .map(_.sorted)
      zeroSolutions ++ chainSolutions
    case Zero => Set(List.empty)
  }

}

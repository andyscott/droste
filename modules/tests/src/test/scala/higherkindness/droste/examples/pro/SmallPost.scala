package higherkindness.droste.examples.pro

import org.scalacheck.Properties
import org.scalacheck.Prop._

import cats.~>
import cats.Functor
import cats.Eval

import higherkindness.droste.Algebra
import higherkindness.droste.Coalgebra
import higherkindness.droste.Embed
import higherkindness.droste.scheme
import scala.collection.compat.immutable.LazyList

final class SmallPost extends Properties("SmallPost") {

  // pattern functor for a lazy list
  sealed trait StreamF[+A, +B]
  case class PrependF[A, B](head: A, tail: Eval[B]) extends StreamF[A, B]
  case object EmptyF                                extends StreamF[Nothing, Nothing]

  implicit def streamFunctor[A]: Functor[StreamF[A, *]] =
    new Functor[StreamF[A, *]] {
      override def map[B, C](fa: StreamF[A, B])(f: B => C): StreamF[A, C] =
        fa match {
          case EmptyF         => EmptyF
          case PrependF(h, t) => PrependF(h, t.map(f))
        }
    }

  implicit def streamFEmbed[A]: Embed[StreamF[A, *], LazyList[A]] = new Embed[StreamF[A, *], LazyList[A]] {
    override def algebra = Algebra[StreamF[A, *], LazyList[A]] {
      case PrependF(head, tail) => head #:: tail.value
      case EmptyF               => LazyList.empty[A]
    }
  }

  def filterNT(lim: Int): StreamF[Int, *] ~> StreamF[Int, *] =
    new (StreamF[Int, *] ~> StreamF[Int, *]) {
      def apply[A](s: StreamF[Int, A]): StreamF[Int, A] = s match {
        case EmptyF                         => EmptyF
        case t @ PrependF(h, _) if h <= lim => t
        case PrependF(_, _)                 => EmptyF
      }
    }

  val infiniteCoalg = Coalgebra[StreamF[Int, *], Int] { n =>
    PrependF(n, Eval.later(n + 1))
  }

  val smallStream =
    scheme.zoo
      .postpro[StreamF[Int, *], Int, LazyList[Int]](infiniteCoalg, filterNT(10))
      .andThen(_.toList)

  property("under limit") = smallStream(7) ?= List(7, 8, 9, 10)

  property("over limit") = smallStream(11) ?= Nil

}

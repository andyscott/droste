package higherkindness.droste.derivation

import cats.{Foldable, Functor, Traverse}

class FoldableTests {

  case class Box[A](value: A) derives Foldable

  sealed trait Maybe[+A] derives Foldable
  case object Nufin extends Maybe[Nothing]
  case class Just[A](value: A) extends Maybe[A]

  sealed trait CList[A] derives Foldable
  case object CNil extends CList[Nothing]
  case class CCons[A](head: A, tail: CList[A]) extends CList[A]
}


class FunctorTests {

  case class Box[A](value: A) derives Functor

  sealed trait Maybe[+A] derives Functor
  case object Nufin extends Maybe[Nothing]
  case class Just[A](value: A) extends Maybe[A]

  sealed trait CList[A] derives Functor
  case object CNil extends CList[Nothing]
  case class CCons[A](head: A, tail: CList[A]) extends CList[A]

}


class TraverseTests {

  case class Box[A](value: A) derives Traverse

  sealed trait Maybe[+A] derives Traverse
  case object Nufin extends Maybe[Nothing]
  case class Just[A](value: A) extends Maybe[A]

  sealed trait CList[A] derives Traverse
  case object CNil extends CList[Nothing]
  case class CCons[A](head: A, tail: CList[A]) extends CList[A]
}
package higherkindness.droste
package reftree

import _root_.reftree.core.RefTree
import _root_.reftree.core.ToRefTree

import cats.Traverse

import higherkindness.droste.data.Attr
import higherkindness.droste.data.AttrF
import higherkindness.droste.data.Coattr
import higherkindness.droste.data.CoattrF
import higherkindness.droste.data.Fix
import higherkindness.droste.data.prelude._

object prelude {

  implicit def fixedToRefTree[F[_] <: AnyRef: Traverse](
      implicit ev: ToRefTree[F[RefTree]]
  ): ToRefTree[Fix[F]] =
    ToRefTree(
      input =>
        scheme
          .hyloM[Zedd.M, F, Fix[F], RefTree](
            Zedd.up(fixedToRefTreeAlgebra),
            Zedd.down
          )
          .apply(input)
          .runA(Zedd.empty)
          .value)

  implicit def cofreeToRefTree[F[_] <: AnyRef: Traverse, A](
      implicit evF: ToRefTree[F[RefTree]],
      evA: ToRefTree[A]
  ): ToRefTree[Attr[F, A]] =
    ToRefTree(
      input =>
        scheme
          .hyloM[Zedd.M, AttrF[F, A, *], Attr[F, A], RefTree](
            Zedd.up(cofreeToRefTreeAlgebra[F, A]),
            Zedd.down
          )
          .apply(input)
          .runA(Zedd.empty)
          .value)

  implicit def freeToRefTree[F[_] <: AnyRef: Traverse, A](
      implicit evF: ToRefTree[F[RefTree]],
      evA: ToRefTree[A]
  ): ToRefTree[Coattr[F, A]] =
    ToRefTree(
      input =>
        scheme
          .hyloM[Zedd.M, CoattrF[F, A, *], Coattr[F, A], RefTree](
            Zedd.up(freeToRefTreeAlgebra[F, A]),
            Zedd.down
          )
          .apply(input)
          .runA(Zedd.empty)
          .value)

  private def fixedToRefTreeAlgebra[F[_]](
      implicit evF: ToRefTree[F[RefTree]]
  ): Algebra[F, RefTree] =
    Algebra((fa: F[RefTree]) => evF.refTree(fa))

  private def cofreeToRefTreeAlgebra[F[_] <: AnyRef, A](
      implicit evF: ToRefTree[F[RefTree]],
      evA: ToRefTree[A]
  ): Algebra[AttrF[F, A, *], RefTree] =
    Algebra { (fa: AttrF[F, A, RefTree]) =>
      val children = evF.refTree(fa.lower) match {
        case ref: RefTree.Ref => ref.children.toList
        case other            => List(other.toField.withName("value"))
      }
      RefTree.Ref(
        fa.lower,
        evA.refTree(fa.ask).toField.withName("attr") :: children)
    }

  private def freeToRefTreeAlgebra[F[_] <: AnyRef, A](
      implicit evF: ToRefTree[F[RefTree]],
      evA: ToRefTree[A]
  ): Algebra[CoattrF[F, A, *], RefTree] =
    Algebra(CoattrF.un(_).fold(evA.refTree, evF.refTree))
}

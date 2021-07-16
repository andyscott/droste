package higherkindness.droste

import cats.~>
import cats.Functor
import cats.Traverse
import cats.Monad
import cats.free.Yoneda
import cats.syntax.functor._

import higherkindness.droste.data.Attr
import higherkindness.droste.data.Coattr

import higherkindness.droste.data.prelude._

private[droste] trait Zoo {

  /** A variation of an anamorphism that lets you terminate any point of
    * the recursion using a value of the original input type.
    *
    * One use case is to return cached/precomputed results during an
    * unfold.
    *
    * @group unfolds
    */
  def apo[F[_]: Functor, A, R](
      coalgebra: RCoalgebra[R, F, A]
  )(implicit embed: Embed[F, R]): A => R =
    kernel.hyloC(
      embed.algebra.run.compose((frr: F[(R Either R)]) => frr.map(_.merge)),
      coalgebra.run
    )

  /** A monadic version of an apomorphism.
    *
    * @group unfolds
    */
  def apoM[M[_]: Monad, F[_]: Traverse, A, R](
      coalgebraM: RCoalgebraM[R, M, F, A]
  )(implicit embed: Embed[F, R]): A => M[R] =
    kernel.hyloMC(
      embed.algebra
        .lift[M]
        .run
        .compose((frr: F[(R Either R)]) => frr.map(_.merge)),
      coalgebraM.run
    )

  /** A variation of a catamorphism that gives you access to the input value at
    * every point in the computation.
    *
    * A paramorphism "eats its argument and keeps it too."
    *
    * This means each step has access to both the computed result
    * value as well as the original value.
    *
    * @group folds
    */
  def para[F[_]: Functor, R, B](
      algebra: RAlgebra[R, F, B]
  )(implicit project: Project[F, R]): R => B =
    kernel.hyloC(algebra.run, project.coalgebra.run.andThen(_.map(r => (r, r))))

  /** A monadic version of a paramorphism.
    *
    * @group folds
    */
  def paraM[M[_]: Monad, F[_]: Traverse, R, B](
      algebraM: RAlgebraM[R, M, F, B]
  )(implicit project: Project[F, R]): R => M[B] =
    kernel.hyloMC(
      algebraM.run,
      project.coalgebra.lift[M].run.andThen(_.map(_.map(r => (r, r))))
    )

  /** Histomorphism
    *
    * @group folds
    */
  def histo[F[_]: Functor, R, B](
      algebra: CVAlgebra[F, B]
  )(implicit project: Project[F, R]): R => B =
    kernel.hylo[F, R, Attr[F, B]](
      fb => Attr(algebra(fb), fb),
      project.coalgebra.run
    ) andThen (_.head)

  /** Futumorphism
    *
    * @group unfolds
    */
  def futu[F[_]: Functor, A, R](
      coalgebra: CVCoalgebra[F, A]
  )(implicit embed: Embed[F, R]): A => R =
    kernel.hylo[F, Coattr[F, A], R](
      embed.algebra.run,
      _.fold(coalgebra.run, identity)
    ) compose (Coattr.pure(_))

  /** A fusion refold of a futumorphism followed by a histomorphism
    *
    * @group refolds
    */
  def chrono[F[_]: Functor, A, B](
      algebra: CVAlgebra[F, B],
      coalgebra: CVCoalgebra[F, A]
  ): A => B =
    kernel.hylo[F, Coattr[F, A], Attr[F, B]](
      fb => Attr(algebra(fb), fb),
      _.fold(coalgebra.run, identity)
    ) andThen (_.head) compose (Coattr.pure(_))

  /** A fusion refold of an anamorphism followed by a histomorphism
    *
    * @group refolds
    */
  def dyna[F[_]: Functor, A, B](
      algebra: CVAlgebra[F, B],
      coalgebra: Coalgebra[F, A]
  ): A => B =
    kernel.hylo[F, A, Attr[F, B]](
      fb => Attr(algebra(fb), fb),
      coalgebra.run
    ) andThen (_.head)

  /** A variation of a catamorphism that applies a natural transformation before its algebra.
    *
    * This allows one to preprocess the input structure.
    *
    * @group folds
    */
  def prepro[F[_]: Functor, R, B](
      natTrans: F ~> F,
      algebra: Algebra[F, B]
  )(implicit project: Project[F, R]): R => B =
    kernel.hylo[Yoneda[F, *], R, B](
      yfb => algebra.run(yfb.mapK(natTrans).run),
      project.coalgebra.run.andThen(Yoneda.apply[F, R])
    )

  /** A variation of an anamorphism that applies a natural transformation after its coalgebra.
    *
    * This allows one to postprocess the output structure.
    *
    * @group unfolds
    */
  def postpro[F[_]: Functor, A, R](
      coalgebra: Coalgebra[F, A],
      natTrans: F ~> F
  )(implicit embed: Embed[F, R]): A => R =
    kernel.hylo[Yoneda[F, *], A, R](
      yfb => embed.algebra.run(yfb.run),
      coalgebra.run.andThen(fa => Yoneda.apply[F, A](fa).mapK(natTrans))
    )

  /** A catamorphism built from two semi-mutually recursive functions.
    *
    * This allows the second algebra to depend on the result of the first one.
    *
    * @group folds
    */
  def zygo[F[_]: Functor, R, A, B](
      algebra: Algebra[F, A],
      ralgebra: RAlgebra[A, F, B]
  )(implicit project: Project[F, R]): R => B =
    kernel.hylo[F, R, (A, B)](
      fab => (algebra.run(fab.map(_._1)), ralgebra.run(fab)),
      project.coalgebra.run
    ) andThen (_._2)
}

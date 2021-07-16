package higherkindness.droste

import cats.Functor
import cats.syntax.functor._

import higherkindness.droste.data.Attr

object Gather {

  def cata[F[_], A]: Gather[F, A, A] =
    (a, _) => a

  def zygo[F[_]: Functor, A, B](algebra: Algebra[F, B]): Gather[F, (B, A), A] =
    (a, fa) => (algebra(fa.map(_._1)), a)

  def para[F[_]: Functor, A, B](implicit
      embed: Embed[F, B]
  ): Gather[F, (B, A), A] =
    zygo(embed.algebra)

  def histo[F[_], A]: Gather[F, Attr[F, A], A] =
    Attr(_, _)

  def zip[F[_]: Functor, Ax, Ay, Sx, Sy](
      x: Gather[F, Sx, Ax],
      y: Gather[F, Sy, Ay]
  ): Gather[F, (Sx, Sy), (Ax, Ay)] =
    (a, fs) => (x(a._1, fs.map(_._1)), y(a._2, fs.map(_._2)))

}

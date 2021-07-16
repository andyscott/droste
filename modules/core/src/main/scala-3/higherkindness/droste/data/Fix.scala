package higherkindness.droste
package data

object Fix {
  def apply[F[_]](f: F[Fix[F]]): Fix[F] = f.asInstanceOf
  def un[F[_]](f: Fix[F]): F[Fix[F]]    = f.asInstanceOf

  def unapply[F[_]](f: Fix[F]): Some[F[Fix[F]]] = Some(un(f))

  def algebra[F[_]]: Algebra[F, Fix[F]]     = Algebra(apply(_))
  def coalgebra[F[_]]: Coalgebra[F, Fix[F]] = Coalgebra(un(_))
}

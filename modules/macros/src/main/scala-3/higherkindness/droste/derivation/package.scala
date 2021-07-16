package higherkindness.droste.derivation

import cats.*

/*
Credit to the `kittens` project. This is a direct copy/paste from there

https://github.com/typelevel/kittens/blob/5b3b7ca168b9636f340b23bc95a4fd1506a26707/core/src/main/scala-3/cats/derived/package.scala
*/

extension (F: Foldable.type)
  inline def derived[F[_]]: Foldable[F] = DerivedFoldable[F]

extension (F: Functor.type)
  inline def derived[F[_]]: Functor[F] = DerivedFunctor[F]

extension (F: Traverse.type)
  inline def derived[F[_]]: Traverse[F] = DerivedTraverse[F]

object semiauto:

  inline def foldable[F[_]]: Foldable[F] = DerivedFoldable[F]

  inline def functor[F[_]]: Functor[F] = DerivedFunctor[F]

  inline def traverse[F[_]]: Traverse[F] = DerivedTraverse[F]


object auto:

  inline given [F[_]]: Foldable[F] = DerivedFoldable[F]

  inline given [F[_]]: Functor[F] = DerivedFunctor[F]

  inline given [F[_]]: Traverse[F] = DerivedTraverse[F]
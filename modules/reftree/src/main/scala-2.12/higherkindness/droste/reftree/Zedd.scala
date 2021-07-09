package higherkindness.droste
package reftree

import _root_.reftree.core.RefTree

import cats.data.State

/** Tracker for tree position when reassigning reftree id
  * values.
  *
  * A counter is kept for assigning ids to nodes on the
  * same level of the structure.
  */
private[reftree] final case class Zedd(
    level: Int,
    counters: Map[Int, Int]
) {

  def down: Zedd = copy(level = level + 1)
  def up: Zedd   = copy(level = level - 1)

  def next: (Zedd, Int) = {
    val nn = counters.get(level).fold(0)(_ + 1)
    copy(counters = counters + (level -> nn)) -> nn
  }

}

private[reftree] object Zedd {
  type M[A] = State[Zedd, A]

  def empty: Zedd = Zedd(0, Map.empty)

  def down[F[_], R](implicit project: Project[F, R]): CoalgebraM[M, F, R] =
    CoalgebraM(a => State(s => (s.down, project.coalgebra(a))))

  def up[F[_]](algebra: Algebra[F, RefTree]): AlgebraM[M, F, RefTree] =
    AlgebraM(fa =>
      State { s =>
        val (ss, i) = s.next
        ss.up -> (algebra(fa) match {
          case ref: RefTree.Ref => ref.copy(id = s"${ref.name}-${s.level}-$i")
          case other            => other
        })
    })
}

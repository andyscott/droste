package qq.athema

import atto._
import atto.syntax.all._

import cats.data.StateT
import cats.implicits._

import qq.droste.data._

object ExprParser {

  def parse(input: String): Either[String, Expr.Fixed[BigDecimal]] =
    for {
      tokens <- tokenizer.parseOnly(input).either
      result <- tokens.tailRecM(yardStep).runA(SS(Nil, Nil))
    } yield result

  private sealed trait Token
  private sealed trait TParen extends Token
  private case object TParenL extends TParen
  private case object TParenR extends TParen
  private case class TConst(value: BigDecimal) extends Token
  private case class TVar(name: String) extends Token
  private case object TAdd extends Token
  private case object TSub extends Token
  private case object TProd extends Token
  private case object TDiv extends Token

  // aliases for F, S, and A for StateT
  private type FF[a] = Either[String, a]
  private type Output = List[Expr.Fixed[BigDecimal]]
  private final case class SS(ops: List[(Int, Token)], output: Output)
  private type AA = Either[List[Token], Expr.Fixed[BigDecimal]]

  private val yardStep: List[Token] => StateT[FF, SS, AA] = input => {
    input match {
      case op :: tail =>
        (op match {
          case c: TConst => enqueue(c)
          case v: TVar   => enqueue(v)
          case TParenL   => parenL()
          case TParenR   => parenR().flatMap(_.traverse(enqueue))
          case TAdd      => shunt(op, 1).flatMap(_.traverse(enqueue))
          case TSub      => shunt(op, 1).flatMap(_.traverse(enqueue))
          case TProd     => shunt(op, 2).flatMap(_.traverse(enqueue))
          case TDiv      => shunt(op, 2).flatMap(_.traverse(enqueue))
        }).as(tail.asLeft)
      case Nil =>
        for {
          s0 <- StateT.get[FF, SS]
          _  <- s0.ops.map(_._2).traverse(enqueue)
          s1 <- StateT.get[FF, SS]
        }
        yield s1.output.head.asRight
    }
  }

  private def parenL(): StateT[FF, SS, Unit] =
    StateT.modify(s => SS((0, TParenL) :: s.ops, s.output))

  private def parenR(): StateT[FF, SS, List[Token]] =
    StateT { s =>
      val (oops, ops) = s.ops.span(_._2 != TParenL)
      (SS(ops.tail, s.output), oops.map(_._2)).asRight
    }

  private def shunt(op: Token, p: Int): StateT[FF, SS, List[Token]] =
    StateT { s =>
      val (oops, ops) = s.ops.span(_._1 >= p)
        (SS((p, op) :: ops, s.output), oops.map(_._2)).asRight
    }


  private def enqueue(op: Token): StateT[FF, SS, Unit] = {
    val focused = op match {
      case TConst(c) => nullary(Fix(Const(c)))
      case TVar(n)   => nullary(Fix(Var(n)))
      case TAdd      => binary(Add(_, _))
      case TSub      => binary(Sub(_, _))
      case TProd     => binary(Prod(_, _))
      case TDiv      => binary(Div(_, _))
      case TParenL |
          TParenR    => StateT.liftF[FF, Output, Unit](
                         "unexpected token during enqueue".asLeft)
    }
    focused.transformS(_.output, (s, o) => s.copy(output = o))
  }

  private def binary(
    f: (Expr.Fixed[BigDecimal], Expr.Fixed[BigDecimal]) => Expr.Fixed[BigDecimal]
  ): StateT[FF, Output, Unit] =
    StateT.modifyF { output0 =>
      val (xy, output) = output0.splitAt(2)
      xy match {
        case y :: x :: Nil => (f(x, y) :: output).asRight
        case x :: Nil      => "expected two operands for operator but only got one".asLeft
        case Nil           => "expected two operands for operator but got none".asLeft
      }
    }

  private def nullary(expr: Expr.Fixed[BigDecimal]): StateT[FF, Output, Unit] =
    StateT.modify(expr :: _)

  private val token: Parser[Token] =
    Atto.token(
      Atto.char('+').as(TAdd: Token)        |
      Atto.char('-').as(TSub: Token)        |
      Atto.char('*').as(TProd: Token)       |
      Atto.char('/').as(TDiv: Token)        |
      Atto.char('(').as(TParenL: Token)     |
      Atto.char(')').as(TParenR: Token)     |
      Atto.bigDecimal.map(TConst(_): Token) |
      identifier.map(TVar(_): Token)
    )

  private val identifier: Parser[String] =
    (Atto.letter, Atto.many(Atto.letterOrDigit))
      .mapN((h, t) => (h :: t).mkString)

  private val tokenizer: Parser[List[Token]] =
    Atto.many(token)
}

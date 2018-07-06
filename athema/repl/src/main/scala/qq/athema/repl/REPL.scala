package qq.athema
package repl

import cats.effect._
import cats.syntax.all._

import qq.droste.data._

object REPL extends IOApp {

  def loop: IO[ExitCode] =
    for {
      input <- StdIO.readln("𝝰: ")
      _     <- StdIO.println(input)
      _     <- input match {
                 case ":quit" => IO.pure(ExitCode.Success)
                 case       _ => handle(input).followedBy(loop)
               }
    } yield ExitCode.Success

  def handle(input: String): IO[Unit] =
    IO(ExprParser.parse(input)).flatMap(_.fold(
      error => StdIO.println(s"Oh no!: $error"),
      expr => StdIO.println(s"$expr")))

  def run(args: List[String]): IO[ExitCode] =
    loop

}

object StdIO {
  def readln(prompt: String): IO[String] = IO(scala.io.StdIn.readLine(prompt))
  def println(line: String): IO[Unit] = IO(Console.println(line))
}

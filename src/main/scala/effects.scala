package lambdaconf.effects

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object effects {

  def putStrLn(line: String): Unit = ???
  def getStrLn(): String = ???

  sealed trait Console[A]
  case class PutStrLn(line: String) extends Console[Unit]
  case class GetStrLn() extends Console[String]

  case class Sequence[A0, A](first: Console[A0], f: A => Console[A]) extends Console[A0] // bind
  case class Pure[A](value: A) extends Console[A] // point
  case class Map[A0, A](program: Console[A0], f: A0 => A) extends Console[A] //map
  
/*. No flatMap on these yet
  val console: Console[Unit] = 
    for {
      _ <- PutStrLn("What is your name")
      n <- GetStrLn()
      _ <- PutStrLn("Hello, " + n)
    } yield ()
*/
}


object exercise1 {

  // final case class IO[A](unsafePerformIO: () => A)

  // def putStrLn(line: String): IO[Unit] = 
  //   IO(unsafePerformIO = () => println(line))
  
  // def getStrLb(): IO[String] = 
  //   IO(unsafePerformIO = () => )

  //implicit val IOMonad: Monad[IO] = ???
}

object exercise2 {
  import exercise1._

  sealed trait ConsoleF[A]

  val program: Free[ConsoleF, Unit] = ???
}

object exercise3 {
  final case class State[S, A](/* ??? */) {
    def evalState(s: S): A = ???
  }

  implicit def StateMonad[S]: Monad[State[S, ?]] = ???
}

object exercise4 {
  import exercise3._

  def sort[A: Order](list: List[A]): List[A] =
    (??? : State[List[A], List[A]]).evalState(list)
}

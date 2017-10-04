package lambdaconf.patterns

import matryoshka._
import monocle._
import scalaz._, Scalaz._, concurrent.Task



object maybe {
  sealed trait Maybe[+A] { self =>       
    def map[B](f: A => B) = self match {
      case Just(a) => Just(f(a))
      case Empty => Empty
    }

    def flatMap[B](f: A => Maybe[B]) = self match {
      case Just(a) => f(a)
      case Empty => Empty
    }
  }

  case class Just[+A](value: A) extends Maybe[A]
  case object Empty extends Maybe[Nothing]

  object Maybe {
    def just[A](a: A) = Just(a)
  }


  def getPort: Maybe[Int] = ???

  def getUrl: Maybe[String] = ???

  getUrl.flatMap( u => getPort.map( p => s">>> contacting http://$u:$p") )

}


object disjunction {
  
  sealed trait Either[A, B] { self => 
    def map[C](f: B => C): Either[A, C] = self match {
      case Left(a) => Left(a)
      case Right(b) => Right(f(b))
    }

    def flatMap[C](f: B => Either[A, C]): Either[A, C] = self match {
      case Left(a) => Left(a)
      case Right(b) => f(b)
    }

  }


  case class Left[A, B](a: A) extends Either[A, B]
  case class Right[A, B](b: B) extends Either[A, B]

  def Either {
    def success[A, B](b: B) = Right[A, B](b)
  }



  object Config {
    def getString(key: String): Either[String, String] = if (key == "url") Right("yahoo.com") else Left("Key not found")
    def getInt(key: String): Either[String, Int] = Right(8080)
  }

  val c = for {
    u <- Config.getString("urool")
    p <- Config.getInt("port")
  } yield u + ":" + p


}


object validation {
  sealed trait Validation[A, B] { self => 
    final def map[C](f: B => C): Validation[A, C] = self match {
      case Failure(es) => Failure(es)
      case Success(b) => Success(f(b))
    }

    final def zip[C](that: Validation[A, C]): Validation[A, (B, C)] = (self, that) match {
      case (Failure((a1, a1s)), Failure((a2, a2s))) => 
        Failure((a1, a1s ::: (a2 :: a2s)))
      case (Success(b), Failure((a2, a2s))) =>
        Failure((a2, a2s))
      case (Failure((a1, a1s)), Success(b)) => 
        Failure((a1, a1s))
      case (Success(b), Success(c)) => 
        Success((b, c))
    }
  }

  case class Failure[A, B](errors: (A, List[A])) extends Validation[A, B]
  case class Success[A, B](success: B) extends Validation[A, B]


  object Config {
    def getString(key: String): Validation[String, String] = if (key == "url") Success("yahoo.com") else Failure((s"Key $key not found", Nil))
    def getInt(key: String): Validation[String, Int] = if (key == "port") Success(8080) else Failure((s"Key $key not found", Nil))
  }

  val c = Config.getString("urkl").zip(Config.getInt("pokrt"))
  
}



object exercise1 {

  /** we have some potential errors*/
  sealed trait Error
  case object NotInTheMoodToTalkToHumans extends Error
  case object MiningBitcoinNoTimeToTalk extends Error

  /** this will give you String (but potentially an error as well) */
  val iWillGiveYouString: Double => Error \/ String =
    (d: Double) => if(d > 10) d.toString.right else NotInTheMoodToTalkToHumans.left

  /** this will give you Int (but potentially an error as well) */
  val iWillGiveYouInt: Long => Error \/ Int =
    (l: Long) => if(l < 0) l.toInt.right else MiningBitcoinNoTimeToTalk.left

    def calculation(d: Double, l: Long): Error \/ (String, Int) = for {
    s <- iWillGiveYouString(d)
    i <- iWillGiveYouInt(l)
  } yield (s, i)

  /** If I'm not interested with an error - I want to ignore it */
  val result: Option[(String, Int)] = ??? // calculate(20.0, -3)

  /** What is a potential issue with the returned type Error \/ (String, Int) */
  /** Run all those function calls and reason about the result, what can we do about it? */
  calculation(20.0, -2)
  calculation(20.0, 6)
  calculation(5.0, -2)
  calculation(5.0, 6)

}

object exercise2 {

  case class User(login: String)

  sealed trait Item
  case object Apple extends Item
  case object Orange extends Item
  /** Basket contains items */
  case class Basket(items: List[Item])

  /** I have to baskets */
  val basket1 = Basket(items =  List(Apple, Orange, Apple, Apple))
  val basket2 = Basket(items =  List(Apple, Apple, Orange))

  /** Could I merge two baskets together */
  val finalBasket: Basket = ??? // basket1 +  basket2

  val user1 = User("john")
  val user2 = User("pawel")
  val user3 = User("jeff")

  /** Could I merge two sessions together */
  val session1: Map[User, Basket] = Map(user1 -> basket1, user2 -> basket2)
  val session2: Map[User, Basket] = Map(user1 -> finalBasket, user2 -> basket1, user3 -> basket2)

  val session: Map[User, Basket] = ??? // session1 + session2

  /** Implement toBasket method, what needs to be added for this to work? */
  def toBasket(maybeItem: Option[Item]): Basket = ???
}

object exercise3 {

  /** What is the space of possible solutions of this method */
  def sum(i1: Int, i2: Int): Int = ???

  /** Write abstract version of method sum. What is the space of possible solutions of its body? */
  // def sum(???): ??? = ???
}

object exercise4 {

  /** What is the space of possible solutions of this method */
  def sum(list: List[Int]): Int =
    list.fold(0)(_ + _)

  /** Write the most abstract version of method sum. What is the space of possible solutions of its body? */
  // def sum(???): ??? = ???
}

object exercise5 {

  /** problem 1 */
  val problem1: Option[Option[String]] = Some(Some("hello"))
  val solution1: Option[String] = ???

  /** problem 2 */
  val problem2: List[Task[Int]] = List(
    Task.now(1), Task.now(3)
  )
  val solution2: Task[List[Int]] = ???

  /** problem 3 */
  case class User(id: Int)
  val ids = List(1, 2, 3)
  val fetch: Int => Task[User] = (id: Int) => Task.delay {
    User(id)
  }

  val solution: Task[List[User]] = ???

}

object exercise6 {

  val plus: (Int, Int) => Int = _ + _

  def plusOperation[F[_] : Apply](
    f1: F[Int], f2: F[Int]
  ): F[Int] = (f1 |@| f2)(plus)

  /** second argument not closed over F, what now? */
  object problem1 {
    def plusOperation[F[_]](
      f1: F[Int], i: Int
    ): F[Int] = ???
  }

  /** we get new parameter seed F[String], last paramter is now String => F[Int]... can you make it work? */
  object problem2 {
    def plusOperation[F[_]](
      seed: F[String], f1: F[Int], f2: String => F[Int]
    ): F[Int] = ???
  }
}


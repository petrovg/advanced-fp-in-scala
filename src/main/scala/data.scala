package lambdaconf.data

import matryoshka._
import monocle._
import scalaz.{Lens => _, _}

import Scalaz._

object exercise1 {
  // 1. All prime numbers
  // 2. A JSON document
}


object game {

  type Optic[S, T, A, B]
  // Before:  Substructure A inside a superstructure S
  // After:  Substructure B inside a superstructure T
  // Monomorphic cases: S = T, A = B
  case class Lens[S, A](
    set: A => S => S,
    get: S => A
  ) { self => 
    def >>> [B](that: Lens[A, B]): Lens[S, B] = 
      Lens(
        set = (b: B) => (s: S) => self.set(that.set(b)(self.get(s)))(s): S,
        get = (s: S) => (that.get(self.get(s)) : B)
      )
    def modify(f: A => A): S => S = 
      (s: S) => set(f(get(s)))(s)
  }

  case class Prism[S, A](
    set: A => S,
    get: S => Option[A]
  ) { self => 
    def >>> [B](that: Prism[A, B]): Prism[S, B] = 
      Prism(
        set = (b: B) => (self.set(that.set(b)) : S),
        //get = (s: S) => (that.get(self.get(s)) : Option[B])
        get = (s: S) => (self.get(s).flatMap(that.get) : Option[B])
      )
  }
  

  case class Profile(name: String, description: String)

  case class Location(x: Int, y: Int)
  object Locaton {
    def apply(x: Int, y: Int): Option[Location] = ???
  }

  sealed trait Alignment
  case object Positive extends Alignment
  case object Negative extends Alignment
  case object Neutral extends Alignment


  case class Stats(hitPoints: Int, strength: Int, stamina: Int) 

  object Stats {
    val hitPoints: Lens[Stats, Int] = Lens[Stats, Int](set = i => s => s.copy(hitPoints = i), get = _.hitPoints)
  }

  case class Character(stats: Stats, location: Location)

  object Character {
    val stats: Lens[Character, Stats] = 
      Lens[Character, Stats](set = s => c => c.copy(stats = s), get = _.stats)
  }

  sealed trait Item
  case class Chest(size: Int) extends Item

  case class Cell(description: String, items: List[Item], characters: List[Character])

  case class Player(name: String, character: Character)

  object Player {
    val character: Lens[Player, Character] = 
      Lens[Player, Character](set = c => p => p.copy(character = c), get = _.character)
  }

  case class GameMap(rooms: Array[Array[Cell]], player: Player) {
    def location(character: Character): Option[Location] = ???
  }

  object GameMap {
    val player: Lens[GameMap, Player] = 
      Lens[GameMap, Player](set = v => p => p.copy(player = v), get = _.player)
  }

  sealed trait Action

  sealed trait GameState
  case class InProgress(map: GameMap) extends GameState
  case class Complete() extends GameState
  case class Paused() extends GameState

  def update(action: Action, old: GameState): GameState = ???

  def damagePlayer(damage: Int)(state: GameState): GameState = state match {
    case s @ InProgress(map) => 
      val lens: Lens[GameMap, Int] = (GameMap.player >>> Player.character >>> Character.stats >>> Stats.hitPoints)
      InProgress( lens.modify(_ - damage)(map) )

    case Complete() => Complete()
  }

}


// trait Algorithm[Stack[_]] {   // Stack : * => *
//   def map(f: Int => Int): List[Int]
//   def foo[A[_], B[_, _, _, _]]() = ??? // A: * => *, B: [* , *, *, *] => *
// }

trait Algorithm[Stack[_]] {
  def run(params: Int, start: Stack[_]): Int = ???
}
// Algorithm : (* => *) => *

object algae {
  val myAlg: Algorithm[List] = new Algorithm[List]{}
}


trait Module[Algorithm[_[_]]] {}
// Module : ((* => *) => *) => *

// [_[_]]
// List[A]
// * => *

// [_[_]]                   ====  * => *
// Algotithm[_[_]]          ====  (* => *) => *
// Module[Algorithm[_[_]]]  ====  ( (* => *) => * ) => *


// scala> :kind lambdaconf.data.Module
// lambdaconf.data.Module's kind is Y[X[F[A]]]

// scala> :kind lambdaconf.data.ModuleB
// lambdaconf.data.ModuleB's kind is Y[X[F[A]]]

trait ModuleB[A[B[C]]] {}


trait Foo[T[_[_], _]] 
trait Goo[T[A[B], C]]

// T takes two type parameters. The first is a type constructor (* => *), the second is of kind *
// Foo : [ * => *, * ] => *





object exercise2 {
  sealed trait Boolean {
    // ???
  }
  object Boolean {
    val True = new Boolean {
      // ???
    }
    val False = new Boolean {
      // ???
    }
  }

  sealed trait Either[A, B] {
    // ???
  }
  object Either {
    def left[A, B](v: A): Either[A, B] = new Either[A, B] {
      // ???
    }
    def right[A, B](v: B): Either[A, B] = new Either[A, B] {
      // ???
    }
  }

  // Cheat: type Option[A] = Either[Unit, A]
  sealed trait Option[A] {
    // ???
  }
  object Option {
    def none[A]: Option[A] = new Option[A] {
      // ???
    }
    def some[A](v: A): Option[A] = new Option[A] {
      // ???
    }
  }
}

object exercise3 {
  trait Natural { self =>
    def fold[Z](zero: => Z, succ: Z => Z): Z

    def succ: Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z = succ(self.fold(zero, succ))
    }
    def + (that: Natural): Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z = that.fold[Natural](self, _.succ).fold[Z](zero, succ)
    }
    def * (that: Natural): Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z =
        that.fold[Natural](Natural.zero, _ + self).fold[Z](zero, succ)
    }
    def isZero: Boolean = fold[Boolean](true, _ => false)
    def toInt: Int = fold[Int](0, _ + 1)
    override def toString = toInt.toString
  }
  object Natural {
    val zero = new Natural { def fold[Z](zero: => Z, succ: Z => Z): Z = zero }
    def of(v: Int): Natural = if (v == 0) zero else of(v - 1).succ
  }

  trait Integer { self =>
    // ???
  }
}

object exercise4 {
  sealed trait JSON
  final case object Null extends JSON
  final case class Array(value: List[JSON]) extends JSON
  final case class Object(value: List[(String, JSON)]) extends JSON
  final case class Number(value: String) extends JSON
  final case class Boolean(value: Boolean) extends JSON

  val _Null     : Prism[JSON, Unit] = ???
  val _Array    : Prism[JSON, List[JSON]] = ???
  val _Object   : Prism[JSON, List[(String, JSON)]] = ???
  val _Number   : Prism[JSON, String] = ???
  val _Boolean  : Prism[JSON, Boolean] = ???
}

object exercise5 {
  import exercise4._

  sealed trait ContactType
  case object Business extends ContactType
  case object Personal extends ContactType

  case class Person(name: String, age: Int, contactType: ContactType)

  val _name : Lens[Person, String] = ???
  val _age : Lens[Person, Int] = ???
  val _contactType : Lens[Person, ContactType] = ???

  val _Business : Prism[ContactType, Unit] = ???
  val _Personal : Prism[ContactType, Unit] = ???

  def encodePerson(v: Person): JSON = ???

  def decodePerson(v: JSON): Option[Person] = ???
}

object exercise6 {
  sealed trait JSON[A]
  // ???

  val TraverseJson: Traverse[JSON] = ???
}

object exercise7 {
  import exercise6._

  type RecursiveJSON = Fix[JSON]

  val ExampleJSON : RecursiveJSON = ???

  def detectStringNumbers(v: RecursiveJSON): RecursiveJSON = {
    val functorT: FunctorT[Fix] = implicitly[FunctorT[Fix]]

    import functorT._


    ???
  }
}

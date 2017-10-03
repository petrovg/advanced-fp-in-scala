package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

/** 
  * Create ADT representing geometric figures using traits and case classes
  * A figure can be a rectangle or trapezoid or cicrle
  * Rectangle has side a and side b
  * Trapezoid has base a, base b and height h
  * Circle has radious r
  * 
  * Can you think of other representation using different machinery?
  */
object exercise1 {
  sealed trait Figure
}

/**
  * Call me maybe
  */
object exercise2 {
  /** Maybe is of kind *. Change it to be * -> * so that it can hold values of any type */
  sealed trait Maybe
  case class Just(value: String)
  case object Empty

  val gotIt = Just("hello")
  val nah = Empty
  // val gotNum = Just(10)
  // case class User(name: String)
  // val maybeUser = Just(User("Swift"))
}

/**
  * Call each function with any argument so that the code compiles.
  * Please provide the types explicitly in [ ]
  */
object exercise3 {
  def func1[A](a: A): A = a
  def func2[F[_], A](f: F[A]): F[A] = f
  def func3[E[_, _], A, B](e: E[A, B]): E[A, B] = e
  def func4[T[_[_]], F[_]](t: T[F]): T[F] = t

  
  func1[String]("Boo")

  func2[List, String](List("Boo"))

  //func3[Map, String, Int](Map("Pork", "Chop"))
  func3[Map, String, Int](Map( "Pork" -> 7 ))

  import lambdaconf.data.Algorithm
  func4[Algorithm, List](new Algorithm[List](){})

}


// W : [(* => *) => *, ([*, *] => *) => *] => *

trait Lamb[ A[B[_]], C[D[_, _]] ]


object Existential {

  def foo(l: List[_]): Int = {   
    // We don't know the type of the list - hence, must handle all types
    // Which means we can't do much, really. 
    // The type of the list is existentially hidden
    // We must code without knowing it.
    ???
    // eg. l.length
  }

// TODO - look at implementing this.
/*
  class ActorType[S, A, B](send: (S, A) => (S, B)) {
    def makeActor(state: S): Actor[A, B]
  }

  trait Actor[A, B] {
    type State

    val state: State

    def ! (a: A): (B, Actor[A, B]) = {
      val (newState, b) = send(state, a)

      (b, new Actor {
        type State = self.State
        val state = newState
      })
    }

    private def send: (S, A) => (S, B)
  }
*/

}


trait CollectionLike[F[_]] {
  def empty[A]: F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def size[A](fa: F[A]): Int

  def concat[A](l: F[A], r: F[A]): F[A]

}

object cols {

  val ListModule = new CollectionLike[List] {
    def empty[A]: List[A] = Nil
    def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
    def size[A](fa: List[A]) = fa.size
    def concat[A](l: List[A], r: List[A]) = l ++ r
  }

  //val MapModule = new CollectionLike[Map] {   // Map does not fit here - it is * => * => *
  //}

  type MapString[A] = Map[String, A]

  val MapModuleString = new CollectionLike[MapString] {
    def empty[A]: Map[String, A] = Map.empty
    def map[A, B](fa: Map[String, A])(f: A => B) = fa.mapValues(f)
    def size[A](fa: Map[String, A]) = fa.size
    def concat[A](l: Map[String, A], r: Map[String, A]) = l ++ r
  }

  // Now let's squash the type
  //def MapModule[K] = new CollectionLike[Map[K, _]] {
  //def MapModule[K] = new CollectionLike[({type Apply[A] = Map[K, A]})#Apply] { // <-- Ugly!!!
  def MapModule[K] = new CollectionLike[Map[K, ?]] { // <-- kind-projector enables this
    def empty[A]: Map[K, A] = Map.empty
    def map[A, B](fa: Map[K, A])(f: A => B) = fa.mapValues(f)
    def size[A](fa: Map[K, A]) = fa.size
    def concat[A](l: Map[K, A], r: Map[K, A]) = l ++ r
  }

  def myCode[F[_]](coll: CollectionLike[F]) = {
    import coll._

    empty
  }
}




object exercise4 {
  sealed trait Example[F[_]] {
    def value: F[String]
  }

  val listExample = new Example[List] {
    def value: List[String] = List("foo")
  }

//  new Example[({type Apply[A] = Either[A, Int]})#Apply] { // <-- ???
//    def value: Either[String, Int] = Right(2)
//  }

 new Example[Either[?, Int]] { // <-- ???
   def value: Either[String, Int] = Right(2)
 }
  
 type EitherInt[A] = Either[A, Int]
 new Example[EitherInt] { // <-- ???
   def value: Either[String, Int] = Right(2)
 }


}

/** Explore the mysteries of magic box */
object exercise5 {

  /** This is a magic box. */
  trait MagicBox[A] {
    type B
    def create[C](c: C): MagicBox[C]
    def get: A

    def map(f: A => B): MagicBox[B] = create[B](f(get))
  }

  /** create a class IntMagicBox[A] that is a MagicBox where B is an Int */
  // class IntMagicBox[A] ???

  /** 
    Method transformAndExtract should take a MagicBox and function f, apply to map method and then
    retrive the value using get method.
    
    Implement transformAndExtract
    */
  // def transformAndExtract[A](mb: MagicBox[A])(f: A => ???): ??? = ???

  // val strIntMagicBox = new IntMagicBox[String]("hello existential")
  val length: String => Int = _.length
  /** call transformAndExtract with instance of intMagicBox and function length to test that it works*/
  // transformAndExtract[String](strIntMagicBox)(length)

}

package lambdaconf.typeclasses

import matryoshka._
import monocle._
//import scalaz._

//import Scalaz._

/**
  * Implement Eq type class
  */
object exercise1 {

  def sort[A: Ord](a: List[A]): List[A] = a match {
    case pivot :: tail => 
      val (before, after) = tail.partition(_ <= pivot)
      sort(before) ::: (pivot :: sort(after))
    case Nil => Nil
  }

  sealed trait Ordering 
  case object Equal extends Ordering
  case object LT extends Ordering
  case object GT extends Ordering

  trait Ord[A] { self => // This says it's just a capability
    def compare(a: A, b: A): Ordering
  }

  object Ord {

    def apply[A](implicit o: Ord[A]): Ord[A] = o

    implicit val OrdInt = new Ord[Int] { self => 
      def compare(a: Int, b: Int) = 
        if (a == a) Equal
        else if (a < b) LT
        else GT
    }

    implicit def OrdList[A](implicit ord: Ord[A]) = new Ord[List[A]] {
      def compare(l: List[A], r: List[A]): Ordering = ???
    }
  }


  // Important!!! Putting the implicit on the method and not the class is better,
  // because a) perfoemance, and b) much better error message
  implicit class OrdSyntax[A](val l: A) extends AnyVal {
    def < (r: A)(implicit o: Ord[A]): Boolean = o.compare(l, r) == LT
    def <= (r: A)(implicit o: Ord[A]): Boolean = o.compare(l, r) != GT
    def > (r: A)(implicit o: Ord[A]): Boolean = o.compare(l, r) == GT
    def >= (r: A)(implicit o: Ord[A]): Boolean = o.compare(l, r) != LT
    def == (r: A)(implicit o: Ord[A]): Boolean = o.compare(l, r) == Equal
  }

  sort[List[Int]](List(List(1, 2, 3)))

  case class Person(name: String, age: Int)
  object Person { 
    implicit val ordPerson: Ord[Person] = new Ord[Person] {
      def compare(a: Person, b: Person) = if (a.name == b.name) Equal else if (a.name < b.name) LT else GT
    }

  }

  case class ByAge(p: Person) extends AnyVal
  object ByAge {
    implicit val ordByAge = new Ord[ByAge] {
      def compare(a: ByAge, b: ByAge): Ordering = 
        if (a.p.age == b.p.age) Equal else if (a.p.age < b.p.age) LT else GT
    }
  }

  val ann = Person("Anne", 36)
  val bob = Person("Bob", 31)

  sort(List(ann, bob))
  sort(List(bob, ann))


  /** Before we start, try to reason what is wrong with the code below? */
  // def check(i: Int): String =
  //   if(i == "1") "It is one!" else "It is something else then 1"
  // check(1)
  // check(2)


  sealed trait Eq[A] {
    def equalz(a: A, b: A): Boolean
  }

  object Eq {
    implicit val IntEq = new Eq[Int] {
      def equalz(a: Int, b: Int): Boolean = a == b
    }
  }


  implicit class EqSyntax[A](val l: A) extends AnyVal{
    def === (r: A)(implicit o: Eq[A]): Boolean = o.equalz(l, r)
  }


  def check(i: Int): String = if (i === 1) "FOUND" else "NOT FOUND"



  /** 
    * 1. Create Eq type class with method equalz checking for equality of two object of type A
    * 2. Create extension method === 
    * 3. Create instance for Int
    * 4. Reimplelemnt check method using Eq
    */

}

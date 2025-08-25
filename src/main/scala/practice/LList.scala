package practice

import scala.annotation.tailrec

trait Predicate[T]:
  def test(elem: T): Boolean

trait Transformer[A, B]:
  def transform(elem: A): B

//-----------------------------------------------------------------------------
trait LList[A] {
  def head: A
  def tail: LList[A]

  def isEmpty: Boolean
  def add(element: A): LList[A] = new Cons(element, this)

  // concatenate. Needed for flatMap. Also useful on its own
  infix def ++(anotherList: LList[A]): LList[A]

  def map[B](transformer: Transformer[A, B]): LList[B]
  def map_v2[B](transformer: Transformer[A, B]): LList[B]

  def filter(predicate: Predicate[A]): LList[A]
  def filter_v2(predicate: Predicate[A]): LList[A]

  def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B]
}

//-----------------------------------------------------------------------------
/*
  Empty is a case class with a generic type A and empty argument list.
  All Empty's of the same type are now equal. We get this feature for free
  from case classes.
*/
case class Empty[A]() extends LList[A] {
  override def head: A          = throw new NoSuchElementException()
  override def tail: LList[A]   = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  override def map[B](transformer: Transformer[A, B]): LList[B]    = Empty[B]()
  override def map_v2[B](transformer: Transformer[A, B]): LList[B] = Empty[B]()

  override def filter(predicate: Predicate[A]): LList[A]    = this // instead of Empty[A]()
  override def filter_v2(predicate: Predicate[A]): LList[A] = this

  override def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B] = Empty[B]()

  override infix def ++(anotherList: LList[A]): LList[A] = anotherList
}

//-----------------------------------------------------------------------------
// Override the head and tail METHODS of the LList parent with FIELDS;
// this is permitted only in Scala
case class Cons[A](override val head: A, override val tail: LList[A]) extends LList[A] {
  override def isEmpty: Boolean  = false
  override def toString: String  = {
    @tailrec
    def concatElements(remainder: LList[A], acc: String): String = {
      if (remainder.isEmpty) acc
      else concatElements(remainder.tail, s"$acc, ${remainder.head}")
    }
    s"[${concatElements(this.tail, s"$head")}]"
    // s"[${concatElements(this, "")}]"  // incorrect code, causes list to be printed as [, 1, 2, 3]
  }

  // Instructor's implementation (stack recursive)
  override def map[B](transformer: Transformer[A, B]): LList[B] =
    new Cons(transformer.transform(head), tail.map(transformer))

  // My implementation (tail recursive). Result is in reverse order
  override def map_v2[B](transformer: Transformer[A, B]): LList[B] = {
    @tailrec
    def concatElems(remainder: LList[A], acc: LList[B]): LList[B] = {
      if (remainder.isEmpty) acc // should return reverse acc
      else {
        val transformedHead = transformer.transform(remainder.head)
        concatElems(remainder.tail, new Cons(transformedHead, acc))
      }
    }
    concatElems(this, Empty[B]())
  }

  // Instructor's solution is stack recursive
  override def filter(predicate: Predicate[A]): LList[A] =
    if predicate.test(head) then
      new Cons(head, tail.filter(predicate))
    else
      tail.filter(predicate)

  // My implementation: a tail-recursive version, but result is in reverse order :-(
  override def filter_v2(predicate: Predicate[A]): LList[A] = {
    @tailrec
    def concatElems(remainder: LList[A], acc: LList[A]): LList[A] = {
      if (remainder.isEmpty)
        acc // should return reverse acc
      else if (predicate.test(remainder.head))  // head passes the filter condition
        concatElems(remainder.tail, new Cons(remainder.head, acc))
      else
        concatElems(remainder.tail, acc)
    }
    concatElems(this, Empty())
  }

  override def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B] =
    transformer.transform(head) ++ tail.flatMap(transformer)

  /* example
    [1,2,3] ++ [4,5,6]
    = new Cons(1, [2,3] ++ [4,5,6])
    = new Cons(1, new Cons(2, [3] ++ [4,5,6]))
    = new Cons(1, new Cons(2, new Cons(3, [] ++ [4,5,6])))
    = new Cons(1, new Cons(2, new Cons(3, [4,5,6])))
    = [1,2,3,4,5,6]
   */
  override def ++(anotherList: LList[A]): LList[A] =
    new Cons(head, tail ++ anotherList)
}

//-----------------------------------------------------------------------------
/**
 * Exercise: LList extension
 *
 * 1. Generic trait Predicate[T] with a little method test(T) => Boolean
 * 2. Generic trait Transformer[A, B] with a method transform(A) => B
 * 3. LList:
 *    - map(transformer) => LList
 *    - filter(predicate) => LList
 *    - flatMap(transformer from A to LList[B]) => LList[B]
 *
 *    class EvenPredicate extends Predicate[Int]
 *    class StringToIntTransformer extends Transformer[String, Int]
 *
 *    [1,2,3].map(n * 2) = [2,4,6]
 *    [1,2,3,4].map(n % 2) = [2, 4]
 *    [1,2,3].flatMap(n => [n, n+1]) = [1,2, 2,3, 3,4]
 */

//-----------------------------------------------------------------------------
object LListTest {
  def main(args: Array[String]): Unit = {
    val empty = Empty[Int]()
    println(empty.isEmpty)
    println(empty)

    val first3Numbers: LList[Int] = Cons(1, Cons(2, Cons(3, empty)))
    println(first3Numbers) // [1, 2, 3]

    // another way of creating the same list
    val first3Numbers_v2 = empty.add(3).add(2).add(1)
    println(first3Numbers_v2) // [1, 2, 3]

    // first 6 numbers
    val first6Num = empty.add(6).add(5).add(4).add(3).add(2).add(1)
    println(first6Num) // [1, 2, 3, 4, 5, 6]

    // using anonymous classes
    val evenPredicate = new Predicate[Int] {
      override def test(elem: Int): Boolean = elem % 2 == 0
    }

    val doubler = new Transformer[Int, Int] {
      override def transform(elem: Int): Int = elem * 2
    }

    val stringToIntTransformer = new Transformer[String, Int] {
      override def transform(elem: String): Int = elem.toInt
    }

    // 2 -> [2, 3]
    val doublerList = new Transformer[Int, LList[Int]] {
      override def transform(elem: Int): LList[Int] = Cons(elem, Cons(elem + 1, Empty[Int]()))
    }

    // test the map function
    // Run the doubler on the first 6 numbers
    val doubled = first6Num.map(doubler)
    println(doubled) // [2, 4, 6, 8, 10, 12]

    val doubled_v2 = first6Num.map_v2(doubler)
    println(doubled_v2) // [12, 10, 8, 6, 4, 2]

    val numbersNested = first6Num.map(doublerList)
    println(numbersNested) // [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 7]]

    val numbersNested_v2 = first6Num.map_v2(doublerList)
    println(numbersNested_v2) // [[6, 7], [5, 6], [4, 5], [3, 4], [2, 3], [1, 2]]

    // test the filter functions
    val evenNums = first6Num.filter(evenPredicate)
    println(evenNums) // [2, 4, 6]

    val evenNums_v2 = first6Num.filter_v2(evenPredicate)
    println(evenNums_v2) // [6, 4, 2]

    // test concatenation
    val concatNums = first3Numbers ++ first3Numbers_v2
    println(concatNums) // [1, 2, 3, 1, 2, 3]

    // test flatMap
    val flattenedList = first6Num.flatMap(doublerList)
    println(flattenedList) // [1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7]

  }
}

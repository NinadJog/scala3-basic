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

  def withFilter(predicate: Predicate[A]): LList[A]

  def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B]

  // find test
  def find(predicate: Predicate[A]): A

  // HOFs exercises
  def foreach(f: A => Unit): Unit
  def zipWith[B, T](other: LList[T], zip: (A, T) => B): LList[B]

  def foldLeft[B](start: B)(combine: (B, A) => B): B
  def sort(compare: (A, A) => Int): LList[A]
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

  override   def withFilter(predicate: Predicate[A]): LList[A] = this

  override def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B] = Empty[B]()

  override infix def ++(anotherList: LList[A]): LList[A] = anotherList

  override def find(predicate: Predicate[A]): A = throw new NoSuchElementException("List is empty")

  // HOFs exercises
  override def foreach(f: A => Unit): Unit = () // Return the Unit value

  override def zipWith[B, T](other: LList[T], zip: (A, T) => B): LList[B] =
    if (!other.isEmpty) throw new IllegalArgumentException("Trying to zip empty list with non-empty list")
    else Empty()

  override def foldLeft[B](start: B)(combine: (B, A) => B): B = start
  override def sort(compare: (A, A) => Int): LList[A] = this
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
      Cons(head, tail.filter(predicate))
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

  // Added as part of exercises so it can support for comprehensions
  override def withFilter(predicate: Predicate[A]): LList[A] =
    filter(predicate)
      
  override def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B] =
    transformer.transform(head) ++ tail.flatMap(transformer)

  /* example
    [1,2,3] ++ [4,5,6]
    = Cons(1, [2,3] ++ [4,5,6])
    = Cons(1, Cons(2, [3] ++ [4,5,6]))
    = Cons(1, Cons(2, Cons(3, [] ++ [4,5,6])))
    = Cons(1, Cons(2, Cons(3, [4,5,6])))
    = [1,2,3,4,5,6]
   */
  override def ++(anotherList: LList[A]): LList[A] =
    Cons(head, tail ++ anotherList)

  override def find(predicate: Predicate[A]): A =
    if predicate.test(head) then head
    else tail.find(predicate)

  // HOFs exercises
  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def zipWith[B, T](other: LList[T], zip: (A, T) => B): LList[B] =
    if (other.isEmpty)
      throw new IllegalArgumentException("Trying to zip empty list with non-empty list")
    else
      Cons(zip(head, other.head), tail.zipWith(other.tail, zip))

  override def foldLeft[B](start: B)(combine: (B, A) => B): B = {
    val newStart = combine(start, head)
    tail.foldLeft[B](newStart)(combine)
  }

  // insertion sort, O(n^2), stack recursive
  override def sort(compare: (A, A) => Int): LList[A] = {

    // insert value into appropriate position in sorted list. examples:
    // 1. insert(2, [1,3,5]) returns [1,2,3,5]
    // 2. insert(1, [6,7,8]) returns [1,6,7,8]
    // 3. insert(8, [1,2,3]) returns [1,2,3,8]
    /*
      insert 2 in [0, 1, 3, 5] == insertSorted(2, [0,1,3,5]
      2 >= 0 so Cons(0, insertSorted(2, [1,3,5])
      2 >= 1 so (Cons(0, Cons(1, insertSorted(2, [3,5]))
      2 < 3  so (Cons(0, Cons(1, Cons(2, [3,5])))
      == [0,1,2,3,5]
     */
    def insertSorted(elem: A, sortedList: LList[A]): LList[A] =
      if sortedList.isEmpty then
        Cons(elem, Empty())
      else if compare(elem, sortedList.head) <= 0 then // insert value
        Cons(elem, sortedList)
      else
        Cons(sortedList.head, insertSorted(elem, sortedList.tail))

    // insert head into the sorted tail
    insertSorted(head, tail.sort(compare))
  }
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

/**
 * LList exercises from HOFs & Currying
 *  - foreach(A => Unit): Unit
 *    [1, 2, 3].foreach(x => println(x))
 *
 *  - sort((A, A) => Int): LList[A]
 *    comparison function should return a negative value if the first argument is
 *    "less" than the other, a positive value if it's greater than, and 0 if they are equal.
 *    [3,2,4,1].sort((x, y) => x - y) == [1,2,3,4]
 *    function need not be optimal or tail recursive. hint: use insertion sort.
 *
 *  - zipWith[B](LList[A], (A, A) => B): LList[B]
 *    [1,2,3].zipWith([4,5,6], x * y) == [4,10,18]
 *    If the two lists are not of the same length then throw an exception.
 *
 *  - foldLeft[B](start: B)((A, B) => B): B
 *    [1,2,3,4].foldLeft[Int](0)(x + y): Int == 10
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

    // test find
    println(first6Num.find(evenPredicate)) // 2
    // println(first6Num.find((elem: Int) => elem > 10)) // throws a NoSuchElement exception

    // Above line is same as follows. Compiler suggested the shorter version
    /*
    println(first6Num.find(new Predicate[Int] {
      override def test(elem: Int): Boolean = elem > 10
    }))
     */

    // test foreach
    // first6Num.foreach(println)

    // test zipWith
    println(first3Numbers.zipWith(first3Numbers, _ + _)) // [2,4,6]

    val animalList: LList[String] = Cons("dog", Cons("cat", Cons("crocodile", Empty())))
    val zippedList: LList[String] = first3Numbers.zipWith(animalList, (num, animal) => s"$num-$animal")
    println(zippedList)

    // throws exception because the lists are not of the same length
    // println(first3Numbers.zipWith(first6Num, _ + _)) // NoSuchElementException("Lists are not of the same length")

    // test foldLeft
    println(first6Num.foldLeft(0)(_ + _)) // 21

    // test sort
    val list1: LList[Int] = Cons(5, Cons(0, Cons(4, Cons(1, Empty()))))
    val sortedList1 = list1.sort((x, y) => x - y)
    println(sortedList1) // [0,1,4,5]

    // test for comprehensions
    val chars: LList[Char] = Cons('a', Cons('b', Cons('c', Empty())))
    val combo: LList[String] = for {
      num <- first3Numbers
      ch <- chars
    } yield s"$num$ch"
    
    println(combo) // [1a, 1b, 1c, 2a, 2b, 2c, 3a, 3b, 3c]

    // test for comprehensions with guards. even numbers only
    val comboEven: LList[String] = for {
      num <- first3Numbers if num % 2 == 0
      ch <- chars
    } yield s"$num$ch"

    println(comboEven) // [2a, 2b, 2c]
  }
}

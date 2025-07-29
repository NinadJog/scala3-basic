package practice

import scala.annotation.tailrec

trait LList[A] {
  def head: A
  def tail: LList[A]

  def isEmpty: Boolean
  def add(element: A): LList[A] = new Cons(element, this)
}

class Empty[A] extends LList[A] {
  override def head: A          = throw new NoSuchElementException()
  override def tail: LList[A]   = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def toString: String = "[]"
}

// Override the head and tail METHODS of the LList parent with FIELDS;
// this is permitted only in Scala
class Cons[A](override val head: A, override val tail: LList[A]) extends LList[A] {
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
}

object LListTest {
  def main(args: Array[String]): Unit = {
    val empty = new Empty[Int]
    println(empty.isEmpty)
    println(empty)

    val first3Numbers: LList[Int] = new Cons(1, new Cons(2, new Cons(3, empty)))
    println(first3Numbers)

    // another way of creating the same list
    val first3Numbers_v2 = empty.add(3).add(2).add(1)
    println(first3Numbers_v2)
  }
}

package part3fp

import scala.annotation.tailrec

object HOFsCurrying {

  // higher-order functions (HOFs)
  val aHof: (Int, Int => Int) => Int = (x, func) => func(x + 1)
  /* Comparable Haskell code would be
  aHof :: (Int, Int -> Int) -> Int
  aHof x func = func (x + 1)
   */

  // function that returns a function
  // the function it returns is the lambda
  val anotherHof: Int => Int => Int = x => (y => y + 2 * x)
  /* Equivalent Haskell code
  anotherHof :: Int -> Int -> Int
  anotherHof x = \y -> y + 2 * x
   */

  // Exercise: What are this function's arguments and return type?
  // Give an example implementation.
  // Answer: takes 2 arguments and returns an Int => Int function
  // The two arguments are Int and (String, Int => Boolean) => Int
  // The second argument is a function that takes a tuple of String and
  // an Int => Boolean function and returns an Int.
  val superfunction: (Int, (String, Int => Boolean) => Int) => Int => Int =
    (x, func) => x => func("Hi there!", y => y % 2 == 0) + 27

  // more examples

  // function that applies f to x n times
  // my implementation isn't tail recursive:
  def nTimes_v0(f: Int => Int, n: Int, x: Int): Int = {
    if n <= 0 then x
    else f(nTimes_v0(f, n - 1, x))
  }

  @tailrec
  def nTimes(f: Int => Int, n: Int, x: Int): Int =
    if n <= 0 then x
    else nTimes(f, n - 1, f(x))

  // usage example
  val plusOne: Int => Int = x => x + 1
  val six = nTimes(plusOne, 5, 1) // 6

  /**
   * Function that takes a function f and integer n and returns a function
   * that applies f 'n' times. This version is stack-recursive, not
   * tail-recursive. It's NOT possible to create a tail-recursive version
   * of this function.
   *
   * Moral: While returning functions from other functions, make sure that
   * they don't become too nested, otherwise it risks stack overflow.
  */
  def nTimes_v2(f: Int => Int, n: Int): Int => Int =
    if n <= 0 then  identity
    else            x => nTimes_v2(f, n - 1)(f(x))

  val nine = nTimes_v2(plusOne, 8)(1) // plusOne(plusOne(plusOne...))) 8 times, so it's at risk of stack overflow

  // currying = HOFs returning function instances
  val superAdder: Int => Int => Int = x => y => x + y
  val addThree: Int => Int = superAdder(3)    // y => 3 + y
  val invokeSuperAdder = superAdder(3)(100)   // 103

  // curried methods = methods with multiple argument lists
  def curriedFormatter(fmt: String)(x: Double): String = fmt.format(x)

  // example of use
  val standardFormat: Double => String = curriedFormatter("%4.2f")
  // same as (x: Double) => "%4.2f".format(x)

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(six)
    println(nine)
    println(standardFormat(Math.PI))
  }
}

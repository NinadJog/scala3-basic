package part3fp

object WhatsAFunction {

  /**
   * Functions in Scala are first-class citizens, meaning functions can be
   * used as values. They can be passed as parameters to other functions and
   * returned from other functions.
   *
   * But Scala works on top of the JVM, which was designed for Java, where
   * the first-class elements are objects (instances of classes).
   *
   * So Scala uses a trick to make functions first-class: it represents
   * functions by instances of traits that take type parameters. So ALL
   * Scala functions are objects.
   */
  trait MyFunction[A, B] {
    def apply(arg: A): B
  }

  // Create a function by instantiating an anonymous instance of the trait.
  // Since doubler is an instance of MyFunction, we can pass it around
  // in a first-class manner, as if it's a first-class function.
  val doubler = new MyFunction[Int, Int] {
    override def apply(arg: Int): Int = arg * 2
  }

  val meaningOfLife = 42
  val doubledMeaning = doubler(meaningOfLife) // same as doubler.apply(meaningOfLife)

  // Using Scala's built-in function types
  val doublerStandard = new Function[Int, Int] {
    override def apply(arg: Int): Int = arg * 2
  }

  // all functions are instances of FunctionX with apply methods

  // 2-argument function
  // Function2[Int, Int, Int] === (Int, Int) => Int
  val adder = new Function2[Int, Int, Int]{
    override def apply(a: Int, b: Int): Int = a + b
  }

  //---------------------------------------------------------------------------
  /**
   * Exercises
   * 1. A function that takes two strings and concatenates them
   * 2. Replace Predicate/Transformer with the appropriate function types if necessary
   * 3. Define a function that takes an Int as an argument and returns another function as a result.
   */

  // 1.
  val concat: (String, String) => String = new Function2[String, String, String]{
    override def apply(a: String, b: String): String = a + b
  }

  // 2.
  // yes. Predicate[T] is equivalent to Function1[T, Boolean] === T => Boolean
  // Transformer[A, B] is equivalent to Function1[A, B]

  // 3. Currying
  val superAdder: Int => Int => Int = new Function1[Int, Function1[Int, Int]]{
    override def apply(x: Int) = new Function1[Int, Int] {
      override def apply(y: Int): Int = x + y
    }
  }

  val adder2    = superAdder(2)     // type is Int => Int
  val total     = adder2(3)         // 5
  val total_v2  = superAdder(2)(3)  // 5

  // function values != methods
  // function values are instances of the Function1, Function2, etc. traits

  def main(args: Array[String]): Unit = {

  }
}

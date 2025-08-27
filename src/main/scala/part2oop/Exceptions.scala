package part2oop

object Exceptions {

  val aString: String = null
  // aString.length crashes with a null pointer exception

  //-----------------------------------------------------------------------------
  // 1. throw exceptions
  // val aWeirdValue: Int = throw new NullPointerException // returns Nothing

  // type Throwable
  //   Error: e.g. stack overflow, out of memory (OOM)
  //   Exception: null pointer, no such element, ...

  def getInt(withExceptions: Boolean): Int = {
    if (withExceptions) throw new RuntimeException("No int for you!")
    else 42
  }

  //-----------------------------------------------------------------------------
  // 2. catching exceptions
  // The compiler is smart enough to  unify the type of the code that might fail and the types
  // of each of the case clauses in the catch block. Here there's only one clause in the catch block and it
  // returns a String. So the compiler will return the lowest common ancestor of Int and String, which
  // in this case is Any. But in Scala 3.x, we can use the type annotation Int | String.
  val potentialFail: Int | String | Boolean = try {
    // code that might fail
    getInt(true)  // an Int
  }
  catch {
    // It's a good practice to place the most specific exceptions first, otherwise they won't get caught
    // Since NullPointerException is a subtype of RuntimeException, place it first.
    case e: NullPointerException  => true     // Boolean
    case e: RuntimeException      => "Hello"  // String
    // can add as many cases as we want
  }
  finally {
    // this section is always executed no matter what
    // Used to close resources
    // this section is optional
    // this section has no impact on the return type. this section's return type is Unit
  }

  //-----------------------------------------------------------------------------
  // 3. Custom exceptions
  class MyException extends RuntimeException {
    override def getMessage: String = "My exception!"
  }

  val myException = new MyException

  //-----------------------------------------------------------------------------

  /**
   * Exercises
   * 1. Crash with stack overflow error
   * 2. Crash with an out of memory error
   * 3. Find an element matching a predicate in LList
   */

  // 1
  def stackOverflowCrash(): Unit = {
    def infinite: Int = 1 + infinite
    infinite
  }

  // 2
  // Cause an out of memory crash by creating a big string
  def outOfMemoryCrash(): Unit = {
    def bigString(n: Int, acc: String): String =
      if (n == 0) acc
      else bigString(n - 1, acc + acc)

    bigString(50123456, "Scala")
  }
  //-----------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(potentialFail)
    // val throwingException: Nothing = throw myException
    // stackOverflowCrash()
    outOfMemoryCrash()
  }
}

package part3fp

object AnonymousFunctions {

  // instances of FunctionN
  val doubler = new Function1[Int, Int]{
    override def apply(x: Int): Int = x * 2
  }

  // lambdas = anonymous function instances
  // with syntax sugar (shorthand syntax). Compiler rewrites RHS to that of the above using Function1
  val doubler_v2: Int => Int = (x: Int) => x * 2
  val doubler_v3: Int => Int = x => x * 2   // with type inference

  // zero-arg functions
  val doSomething: () => Int = () => 45
  val anInvocation = doSomething() // 45

  // desugared representation using Function0
  val doSometing_v2 = new Function0[Int] {
    override def apply(): Int = 45
  }

  //------------------------
  // Alternative syntax with curly braces. Common in production code.
  // This syntax is especially useful for passing function instances
  // as arguments to methods
  val stringToInt = { (str: String) =>
    // implementation: code block. do other stuff here if needed
    str.toInt
  }

  // The following syntax would have been used if the above curly brace
  // syntax were not available. The above one is preferred.
  val stringToIntBoring = (str: String) => {
    // code block
    str.toInt
  }

  // shortest lambdas
  val doubler_v4: Int => Int = _ * 2 // same as x => x * 2. The _ means first arg of function
  val adder_v2: (Int, Int) => Int = _ + _ // each argument denotes a separate argument; we can't reuse them.

  /** Exercises
   * 2. Rewrite the following special adder using just lambdas.
   */
  val superAdder: Int => Int => Int = new Function1[Int, Function1[Int, Int]]{
    override def apply(x: Int) = new Function1[Int, Int] {
      override def apply(y: Int): Int = x + y
    }
  }

  // curried lambdas
  val superAdder_v2: Int => Int => Int = x => y => x + y
  val superAdder_v3 = (x: Int) => (y: Int) => x + y

  val adding2 = superAdder_v2(2)  // y => 2 + y

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(doSomething)    // part3fp.AnonymousFunctions$$$Lambda$16/0x0000000800090040@1500955a
    // Using doSomething with () invokes the function i.e. calls the apply method.
    println(doSomething())  // 45
  }
}

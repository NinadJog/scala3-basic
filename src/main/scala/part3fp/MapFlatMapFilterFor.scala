package part3fp

object MapFlatMapFilterFor {

  // Exercise
  
  val numbers = List(1,2,3,4)
  val chars = List('a', 'b', 'c', 'd')
  val colors = List("black", "white", "red")
  
  // Generate all possible combinations of the form "1a - black"
  val comboList = for {
    num   <- numbers
    ch    <- chars
    color <- colors
  } yield s"$num$ch - $color"
  
  val comboList_v2 =
    numbers.flatMap (num    => 
    chars.flatMap   (ch     => 
    colors.map      (color  => s"$num$ch - $color")))
    
  // Same but only for even numbers
  // Add an if guard to the numbers generator
  val comboEven = for {
    num   <- numbers if num % 2 == 0
    ch    <- chars
    color <- colors
  } yield s"$num$ch - $color"

  // withFilter is similar to filter, but uses lazy evaluation
  val comboEven_v2 =
    numbers
      .withFilter(_ %2 == 0)
      .flatMap    (num    => 
    chars.flatMap (ch     =>
    colors.map    (color  => s"$num$ch - $color")))
    
  //---------------------------------------------------------------------------
  // for comprehensions with side-effects (Units). This for does not have a yield
  for {
    num <- numbers if num % 2 == 0
  } println(num)

  /**
   * Exercises
   * 1. Does LList support for comprehensions? If not, add what's missing.
   *    (Solutions are in LList.scala)
   * 2. Implement a small collection of at most one element: Maybe[A]
   *    - map
   *    - flatMap
   *    - filter
   *    (Solution is in Maybe.scala)
   */

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(comboList)
    // println(comboList_v2)
    println(comboEven)
    println(comboEven_v2)
  }
}

package practice

// instructor used an abstract class; I used a trait
trait Maybe[A] {
  // def value: A // I defined it but instructor didn't, so I'm commenting it out
  def isEmpty: Boolean

  def map[B](f: A => B): Maybe[B]
  def flatMap[B](f: A => Maybe[B]): Maybe[B]

  def filter(predicate: A => Boolean): Maybe[A]
  def withFilter(predicate: A => Boolean): Maybe[A]
}

//---------------------------------------------------------------------------
case class MaybeNot[A]() extends Maybe[A] {

  // override def value: A = throw new NoSuchElementException("Maybe is empty")
  override def isEmpty: Boolean = true

  override def map[B](f: A => B): Maybe[B] = MaybeNot()
  override def flatMap[B](f: A => Maybe[B]): Maybe[B] = MaybeNot()

  override def filter(predicate: A => Boolean): Maybe[A] = this
  override def withFilter(predicate: A => Boolean): Maybe[A] = this
}

//---------------------------------------------------------------------------
case class Just[A](value: A) extends Maybe[A] {
  override def isEmpty: Boolean = false

  override def map[B](f: A => B): Maybe[B] = Just(f(value))
  override def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(value)

  override def filter(predicate: A => Boolean): Maybe[A] =
    if (predicate(value)) this
    else MaybeNot()

  override def withFilter(predicate: A => Boolean): Maybe[A] =
    if (predicate(value)) this
    else MaybeNot()
}

//---------------------------------------------------------------------------
object MaybeTest {

  val missingNum: Maybe[Int] = MaybeNot()
  val validNum: Maybe[Int] = Just(5)

  // test map
  val incrementNothing = missingNum.map(_ + 1) // MaybeNot()
  val incrementedNum = validNum.map(_ + 1) // Just(6)

  // test flatMap
  // suppress odd numbers; retain even ones.
  val oddKiller: Int => Maybe[Int] = x => if x % 2 == 0 then Just(x) else MaybeNot()
  val evensOnly = validNum flatMap oddKiller // MaybeNot()

  // test filter
  // retain even number, else kill it
  val y = incrementedNum filter (_ % 2 == 0) // Just(6)
  val z = validNum filter (_ % 2 == 0) // MaybeNot()

  // test map, flatMap, filter on an empty Maybe
  val mapNothing      = missingNum map      (_ + 50)      // MaybeNot()
  val flatMapNothing  = missingNum flatMap  oddKiller     // MaybeNot()
  val filterNothing   = missingNum filter   (_ % 2 == 0)  // MaybeNot()

  // Just(27)
  val sum: Maybe[Int] = for {
    six <- Just(6)
    nine <- Just(9)
    twelve <- Just(12)
  } yield six + nine + twelve

  // Empty()
  val opAborted: Maybe[String] = for {
    six <- Just(6)
    empty <- MaybeNot()
    twelve <- Just(12)
  } yield s"$six - $empty - $twelve"

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    println(incrementNothing) // MaybeNot()
    println(incrementedNum) // Just(6)
    println(evensOnly) // MaybeNot()

    println(y) // Just(6)
    println(z) // MaybeNot()

    // Test map, flatMap, filter over MaybeNot()
    println(mapNothing) // MaybeNot()
    println(flatMapNothing) // MaybeNot()
    println(filterNothing) // MaybeNot()

    // test for comprehensions on Maybe
    println(sum) // 27
    println(opAborted) // MaybeNot()
  }
}

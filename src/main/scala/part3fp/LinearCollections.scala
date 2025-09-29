package part3fp

import scala.util.Random

object LinearCollections {

  // Seq = well-defined ordering + indexing. Seq is a trait
  def testSeq(): Unit = {
    val aSequence: Seq[Int] = Seq(4, 2, 3, 1)

    // main API is to index an element
    val thirdElem     = aSequence.apply(2)
    val thirdElem_v2  = aSequence(2)

    // other methods
    val reversed      = aSequence.reverse
    val concatenation = aSequence ++ Seq(5,6,7)
    val sortedSeq     = aSequence.sorted
    val sum           = aSequence.foldLeft(0)(_ + _)
    val stringRep     = aSequence.mkString("[", ",", "]")

    println(aSequence)
    println(reversed)
    println(concatenation)
    println(sortedSeq)
  }

  //---------------------------------------------------------------------------
  def testLists(): Unit = {
    val aList = List(1,2,3)

    // prepending and appending
    // rule of thumb for the +: and :+ operators: the + stays on the side of the element being added
    val aBiggerList = 0 +: aList :+ 4

    // The :: operator is the apply method of the case class that we called Cons in our LList implementation
    // In the Scala standard library, this case class is called ::
    val prepending = 0 :: aList   // :: is the most used

    // utility methods
    val scalax5 = List.fill(5)("Scala")

    println(aBiggerList)
    println(scalax5)
  }

  //---------------------------------------------------------------------------
  def testRanges(): Unit = {

    // Range does not contain all the numbers: for example '1 to 1000000000' will not contain a billion numbers;
    // they are not created unless needed (lazy)
    val aRange: Seq[Int] = 1 to 10      // Actual type is Range.Inclusive
    val aNonInclusiveRange = 1 until 10 // This is 1..9. The number 10 is not included

    // Ranges are useful when we want to do something a specific number of times.
    (1 to 5).foreach(_ => println("Scala"))
  }

  //---------------------------------------------------------------------------
  // arrays are mutable; all the others are immutable
  def testArray(): Unit = {
    val anArray = Array(1,2,3,4,5,6)  // int[] on the JVM; not boxed or modified in any way

    // Array has access to most Seq APIs even though arrays are NOT Sequences.
    // Converting an array to a Seq
    val aSequence: Seq[Int] = anArray.toIndexedSeq // actual type is IndexedSeq[Int]

    // arrays are mutable
    anArray.update(2, 30) // [1,2,30,4,5,6]. no new array is allocated
  }

  //---------------------------------------------------------------------------
  // vectors are very fast sequences for a large amount of data
  def testVectors(): Unit = {
    val aVector: Vector[Int] = Vector(1, 2, 3, 4, 5, 6)
    // the exact same Seq API
  }

  //---------------------------------------------------------------------------
  // Compare the write times (modification times) for Vector vs List
  // Vector will be much faster than List
  def smallBenchmark(): Unit = {
    val maxRuns = 1000
    val maxCapacity = 1000000 // 1 million

    def getWriteTime(collection: Seq[Int]): Double = {
      val random = new Random()

      val times = for { // Collection containing 1,000 times.
        i <- 1 to maxRuns
      } yield {
        val index = random.nextInt(maxCapacity)
        val newElement = random.nextInt()

        // Update the collection at this index and see how long it takes
        val currentTime = System.nanoTime()
        val updatedCollection = collection.updated(index, newElement)
        val finalTime = System.nanoTime()
        finalTime - currentTime
      }
      // Compute the average of these 1,000 times
      times.sum * 1.0 / maxRuns   // Alternative code for sum: times.foldLeft(0L)(_ + _)
    }

    val numbersList   = (1 to maxCapacity).toList
    val numbersVector = (1 to maxCapacity).toVector

    val listTime = getWriteTime(numbersList)
    val vectorTime = getWriteTime(numbersVector)
    val faster = listTime / vectorTime

    println(s"List time   = $listTime")   // 7245042.459 = 7.2 ms
    println(s"Vector time = $vectorTime") // 6849.135    = 6.8 microseconds
    println(s"Vector updates were $faster times faster than list")  // 888 i.e. 3 orders of magnitude difference
  }

  //---------------------------------------------------------------------------
  // sets - order is not guaranteed
  // TreeSets guarantee order
  def testSets(): Unit = {
    val aSet: Set[Int] = Set(1,2,3,4,5,6,1,2,3,1)
    println(aSet) // HashSet(5, 1, 6, 2, 3, 4) Does not guarantee any particular ordering

    // Sets are usually implemented in terms of equals + hashCode because a popular
    // implementation of Set is a HashSet.
    // main API: test whether an element is in the Set
    val contains3 = aSet.contains(3)  // true
    val contains3_v2 = aSet(3)        // true. Same as aSet.apply(3)

    // adding/removing
    val aBiggerSet = aSet + 4 // Set remains same but order can change
    val aSmallerSet = aSet - 4  // [1,2,3,5,6]

    // set operations: union
    val anotherSet = Set(4,5,6,7,8)
    val muchBiggerSet = aSet union anotherSet   // [1,2,3,4,5,6,7,8]
    val muchBiggerSet_v2 = aSet ++ anotherSet   // same
    val muchBiggerSet_v3 = aSet | anotherSet    // same

    // set difference
    val aDiffSet = aSet diff anotherSet   // [1,2,3]
    val aDiffSet_v2 = aSet -- anotherSet  // same
    println(s"Set difference: $aDiffSet")

    // set intersection
    val anIntersection = aSet intersect anotherSet  // [4,5,6]
    val anIntersection_v2 = aSet & anotherSet       // same
    println(s"Set intersection: $anIntersection")
  }

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    // testSeq()
    // testLists()
    // smallBenchmark()
    testSets()
  }
}

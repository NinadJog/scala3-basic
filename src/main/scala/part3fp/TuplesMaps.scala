package part3fp

object TuplesMaps {

  // tuples = finite, ordered "lists", similar to Python lists which can be heterogeneous
  // or we can think of them as a group of values under the same "big" value.

  val aTuple = (2, "rock the jvm")  // Tuple2[Int, String] == (Int, String)
  // (Int, String) is syntactic sugar for Tuple2[Int, String]

  val firstField = aTuple._1
  val aCopiedTuple = aTuple.copy(_1 = 54)

  // We can think of 2-element tuples as associations.
  val aTuple_v2 = 2 -> "rock the jvm" // identical to (2, "rock the jvm")

  //---------------------------------------------------------------------------
  // maps: keys -> values
  val aMap = Map()
  
  val phonebook: Map[String, Int] = Map(
    "Jim"     -> 555,
    "Daniel"  -> 789,
    "Jane"    -> 123
  ).withDefaultValue(-1)  // returns -1 when a key is missing

  // map core APIs
  val hasJim = phonebook.contains("Jim")
  val marysPhoneNumber = phonebook("Mary")  // crashes with an exception if no default value is set

  // add a pair
  val newPair = "Mary" -> 678
  val newPhonebook = phonebook + newPair

  // remove a key
  val truncatedPhonebook = phonebook - "Daniel"

  // list -> map
  val linearPhonebook = List(
    "Jim"     -> 555,
    "Daniel"  -> 789,
    "Jane"    -> 123
  )
  val phonebook_v2 = linearPhonebook.toMap

  // map -> linear collection
  val linearPhonebook_v2: Seq[(String, Int)] = phonebook_v2.toList // toSeq, toArray, toVector, toSet

  // map, flatMap, filter are rarely used on Maps. We can think of a map as a linear collection of associations,
  // where the element type is a tuple.
  /*
    Dangers of using map on a Map. Suppose we have the following map
    Map("Jim" -> 123, "jiM" -> 999)
    Converting the key to uppercase would create a map with a single JIM key, but there's no way of knowing
    whether the value will be 123 or 999, as Map is typically implemented as a HashMap. The resulting map is
    Map("JIM" -> ????)
    So it's almost never a good idea to change the keys in a Map by calling map. Ditto for flatMap and filter.
   */
  val aProcessedPhonebook     = phonebook map ((key, value) => (key.toUpperCase, value))
  val aProcessedPhonebook_v2  = phonebook map (pair => (pair._1.toUpperCase, pair._2))

  // filtering keys: return a map without a few keys
  // filter API is weird starting in Scala 2.13 and continuing in Scala 3
  // We have to call toMap because otherwise it's an intermediate representation called MapView
  val noJs = phonebook.view.filterKeys(!_.startsWith("J")).toMap

  // mapping values - this is a more common use-case
  val prefixNumbers = phonebook.view.mapValues(num => s"+91-$num").toMap

  //---------------------------------------------------------------------------
  // other collections can also create maps
  val names = List("Bob", "James", "Angela", "Bill", "Daniel", "Jim")

  // We can group these values by some function and the result of that function will be the key in the resulting Map
  // In the following example, group the elements by the first letter
  val nameGroupings = names groupBy (name => name.charAt(0))  // Map[Char, List[String]]

  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(phonebook)
    println(marysPhoneNumber) // -1
    println(nameGroupings)
  }
}

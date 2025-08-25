package part2oop

object CaseClasses {

  // lightweight data structures
  case class Person(name: String, age: Int) {
    // do some other stuff
  }

  // 1. class args are promoted to fields
  val daniel = Person("Daniel", 99)
  val danielsAge = daniel.age

  // 2. toString, equals, hashCode
  val danielToString = daniel.toString  // Person("Daniel", 99)
  val danielDupe = Person("Daniel", 99)
  val isSameDaniel = daniel == danielDupe // true

  // 3. utility methods
  val danielYounger = daniel.copy(age = 78) // Person("Daniel", 78)

  // 4. case classes have companion objects
  val thePersonSingleton = Person
  val daniel_v2 = Person("Daniel", 99)  // Constructor

  // 5. case classes are serializable
  // use case: Akka

  // 6. case classes have extractor patterns for pattern matching

  // can't create case classes with no args
  /*
    case class CCWithNoArgs {
      // some code
    }

    val ccna = CCWithNoArgs
    val ccna_v2 = CCWithNoArgs // all instances would be equal!
  */

  case object UnitedKingdom {
    // fields and methods
    def name: String = "The United Kingdom of Great Britain and Northern Ireland"
  }

  case class CCWithArgListNoArgs[A]() // allowed; mainly used in the context of generics


  def main(args: Array[String]): Unit = {
    println(danielToString)
    println(isSameDaniel)
    println(danielYounger)
  }
}

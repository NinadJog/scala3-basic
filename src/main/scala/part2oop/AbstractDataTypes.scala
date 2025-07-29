package part2oop

/**
 * Main takeaway
 *
 * "Accessor" METHODS in an abstract class can be overridden by a FIELD in
 * a child class, as the preferredMeal method from the abstract class
 * Animal below demonstrates.
 *
 * Accessor methods are methods that don't take any arguments and don't have
 * parentheses. No other method types are eligible for such overrides. This
 * feature is specific only to Scala.
 */
object AbstractDataTypes {

  abstract class Animal {
    val creatureType: String  // abstract
    def eat(): Unit           // abstract

    def preferredMeal: String = "anything"  // Accessor method
  }

  class Dog extends Animal {
    override val creatureType: String = "dog"
    override def eat(): Unit = println("crunching this bone")

    // overriding accessor method with a FIELD
    override val preferredMeal: String = "bones"
  }

  //---------------------------------------------------------------------------
  // traits

  /**
   * A trait's members can be either implemented or not. In fact both extremes are
   * also possible: all members implemented or none implemented.
   *
   * Differences between abstract classes and traits.
   * 1. Practical: Facilitates multiple inheritance. A Scala class can inherit
   *    from just one class (single inheritance) but it can mix in multiple traits.
   *
   * 2. Philosophical: A class defines a THING whereas a trait defines a BEHAVIOR
   */
  trait Carnivore { // traits can have constructor args starting with Scala 3
    def eat(animal: Animal): Unit
  }

  trait ColdBlooded

  // multiple inheritance
  class Crocodile extends Animal with Carnivore with ColdBlooded {
    override val creatureType: String = "croc"
    override def eat(): Unit                = println("crunch")
    override def eat(animal: Animal): Unit  = println("eating given animal")
  }

  //---------------------------------------------------------------------------
  // Scala type hierarchy
  /*
    Any
      AnyRef
        All classes & traits that we write automatically extend AnyRef; the compiler adds that
          scala.Null (the Scala null reference EXTENDS every single class!)
      AnyVal
        Int, Bool, Char,... value classes (an advanced topic)

            scala.Nothing (under AnyRef & AnyVal; it has no instances!)


    Just as scala.Null is a proper replacement for reference types, scala.Nothing is a
    proper replacement for ANY type, be it an AnyRef or AnyVal.
   */

  class MyThing // extends AnyRef

  val aNonExistentAnimal: Animal = null
  val anInt: Int = throw new NullPointerException   // returns scala.Nothing

}

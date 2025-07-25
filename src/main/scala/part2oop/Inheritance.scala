package part2oop

object Inheritance {

  // constructor is defined in the class declaration
  // The Person class definition is also the definition of its primary constructor
  class Person(val name: String, age: Int) {
    def this(name: String) = this(name, 0)  // auxiliary constructor
    // The only implementation of an auxiliary constructor is in terms of other constructors
    // Secondary (auxiliary) constructors aren't all that useful because for inheritance we
    // have to specify the primary constructor of the superclass with the appropriate
    // arguments we have to use.
  }

  // How to inherit a class when we have a constructor defined in the class declaration
  // Must specify super-constructor in the class declaration.
  // The following version passes on arguments to the Person's PRIMARY constructor in the extends clause
  class Adult(name: String, age: Int, idCard: String) extends Person(name, age)

  // In this example, arguments are passed on the Person's secondary constructor defined above
  class Adult_v2(name: String, age: Int, idCard: String) extends Person(name)

  //---------------------------------------
  // overriding fields and methods

  class Animal:
    val creatureType = "wild"
    def eat(): Unit = println("nomnomnom")

  class Dog extends Animal {
    override val creatureType = "domestic"

    override def eat(): Unit = {
      super.eat()
      println("I like this bone")
    }
    override def toString: String = "a dog"
  }

  // subtype polymorphism
  // declare a supertype (Animal) on the left but instantiate a subtype (Dog) on the right
  val dogAsAnimal: Animal = new Dog
  dogAsAnimal.eat() // the most specific method will be called when we have subtype polymorphism

  // examples of overloading
  class Crocodile extends Animal {
    override def eat(): Unit = println("I'm a croc, I eat everything")
    
    def eat(animal: Animal): Unit = println("I'm eating this poor animal")
    def eat(dog: Dog): Unit       = println("I'm eating this dog")
    def eat(dog: Dog, person: Person): Unit = println(s"I'm eating a dog and a person named ${person.name}")
    def eat(person: Person, dog: Dog): Unit = println(s"I'm eating a dog and a person named ${person.name}")
    // def eat(): Int = 45 // not a valid overload
  }
  
  val croc = new Crocodile
  croc.eat(dogAsAnimal) // "I'm eating this poor animal"

  val dogAsDog: Dog = new Dog
  croc.eat(dogAsDog)    // "I'm eating this dog"
  
  //---------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    println(dogAsAnimal)  // println(dog.toString)
  }
}

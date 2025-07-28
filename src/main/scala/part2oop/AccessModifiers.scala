package part2oop

object AccessModifiers {

  class Person(val name: String) {
    protected def sayHi(): String = s"Hi, my name is $name"
  }

  // complication
  class KidWithParents(override val name: String,
                       age: Int,
                       momName: String,
                       dadName: String) extends Person(name) {
    val mom = new Person(momName)
    val dad = new Person(dadName)

    // The following code does not work, because mom.sayHi() and dad.sayHi() do not compile, as we are attempting a
    // public access of the sayHi() method when it's protected. But this.sayHi() works because KidWithParents
    // is a subclass of Person.
    // def everyoneSayHi(): String =
    //   this.sayHi() + " My parents are" + mom.sayHi() + dad.sayHi()
  }
}

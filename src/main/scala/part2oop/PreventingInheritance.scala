package part2oop

object PreventingInheritance {

  // sealing a type hierarchy = inheritance permitted only inside this file
  sealed class Guitar(nStrings: Int)
  class ElectricGuitar(nStrings: Int) extends Guitar(nStrings)
  class AcousticGuitar extends Guitar(6)

  // no modifier = "not encouraging" inheritance
  // 'open' is not mandatory, but is good practice
  open class ExtensibleGuitar(nStrings: Int) // open = specifically marked for extension
}

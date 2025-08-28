package part2oop

// Fields and methods can be defined at the top level in Scala 3. They act as global variables.
// Scala 3 handles it by creating an object with the same name as the package (part2oop in this case)
// So whenever we import the part2oop package, it will also automatically import these fields and methods.
// Such top-level definitions are not recommended, as they are hard to read, hard to find, and are an anti-pattern.
val meaningOfLife = 42
def computeMyLife: String = "Scala"

object ImportsAndExports {

  //import under an alias
  import java.util.List as JList
  // val aList: JList[Int] = ???

  // import multiple symbols
  import PhysicsConstants.{EARTH_GRAVITY, SPEED_OF_LIGHT}

  // import everything except something
  object PlayingPhysics {
    import PhysicsConstants.{PLANCK as _, *}
    // val plank = PLANCK // does not compile
    val c = SPEED_OF_LIGHT // this does compile
  }

  // default imports - imported automatically by the compiler
  // scala.*, scala.Predef.*, java.lang.*, plus others

  // exports
  // Introduced in Scala 3. Alias for fields or methods from an enclosing scope
  class PhysicsCalculator {
    import PhysicsConstants.*
    
    // API
    def computePhotonEnergy(wavelength: Double): Double =
      wavelength / PLANCK
  }
  
  object ScienceApp {
    val physicsCalculator = new PhysicsCalculator
    
    // Brings the computePhotonEnergy API to the current scope so we don't need to use its qualified name.
    // In that sense export is more like an import
    export physicsCalculator.computePhotonEnergy
    
    def computeEquivalentMass(wavelength: Double): Double =
      computePhotonEnergy(wavelength) / (SPEED_OF_LIGHT * SPEED_OF_LIGHT)
  }
  
  def main(args: Array[String]): Unit = {
  }
}

object PhysicsConstants {
  val SPEED_OF_LIGHT = 299792458
  val PLANCK = 6.62e-34
  val EARTH_GRAVITY = 9.8
}

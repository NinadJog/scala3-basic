package practice

import scala.annotation.tailrec

object TuplesMapsExercises {

  /**
   * Social network = Map[String, Set[String]]
   * String is person's name; Set contains his/her list of friends.
   * Friendships are MUTUAL
   *
   * - add a person to the network
   * - remove a person from the network
   * - add a friend relationship
   * - unfriend
   *
   * - number of friends of a person
   * - who has the most friends
   * - how many people have no friends
   * + whether there's a social connection between two people (direct or not)
   *    Dainel <-> Mary <-> Jane <-> Tom
   */
  def addPerson(network: Map[String, Set[String]], newPerson: String): Map[String, Set[String]] =
    network + (newPerson -> Set())  // new person does not have any friends

  //---------------------------------------------------------------------------
  /**
   * Remove person from network and from the friend lists of all the people they are friends with.
   * A more efficient solution would iterate only over the person's friends to remove
   * the person from each of those friends' friend lists.
   */
  def removePerson(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    (network - person) map {
      (pers, friends) => pers -> (friends - person)
    }
      // same as: (network - person).map(pair => pair._1 -> (pair._2 - person))

  //---------------------------------------------------------------------------
  /**
   * My attempt at a more efficient solution. Update only those friends lists that
   * need to be updated instead of iterating over every person's friends lists.
   */
  def removePerson_v2(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {

    val friends: Set[String] = network(person)  // Person = Jim, friends = [Bob, Mary, Daniel]
    val updatedFriends: Map[String, Set[String]] = // Remove Jim from the friends lists of Bob, Mary, Daniel
      friends
        .map{ friend => (friend, network(friend) - person) }  // remove Jim
        .toMap
    // update network with the new friends lists. updatedFriends will overwrite
    // the old entries for Bob, Mary, Daniel. This happens because when keys conflict
    // in the ++ operator, the entries from the right-hand map overwrite the entries
    // from the left-hand (network) map.
    network ++ updatedFriends
  }

  //---------------------------------------------------------------------------
  /**
   * Most efficient solution, as it updates only the two relevant entries,
   * unlike map, which iterates over all entries. The intent is also more clear here.
   */
  def addFriend(
       network: Map[String, Set[String]],
       a: String,
       b: String): Map[String, Set[String]] = {

    if !network.contains(a) then
      throw new NoSuchElementException(s"Person $a is missing from the network")
    else if !network.contains(b) then
      throw new NoSuchElementException(s"Person $b is missing from the network")
    else {
      val newFriendsOfA = network(a) + b
      val newFriendsOfB = network(b) + a
      network + (a -> newFriendsOfA) + (b -> newFriendsOfB)
    }
  }

  //---------------------------------------------------------------------------
  /**
   * Alternative solution using map. Not recommended for production code, as it
   * is slower, since it iterates over ALL entries to update just two.
   * Processes unchanged entries. The intent is also less clear.
   */
  def addFriend_v2(
        network: Map[String, Set[String]],
        a: String,
        b: String): Map[String, Set[String]] = {
    // TBD: Add error check if a and b are missing from the network
    network map { (pers, friends) =>
      if pers == a then (pers, friends + b)
      else if pers == b then (pers, friends + a)
      else (pers, friends)
    }
  }

  //---------------------------------------------------------------------------
  /**
   * With pattern matching. Again not recommended for production code, as it
   * iterates over all entries when the goal is to update just two of them.
   */
  def addFriend_v3(
                    network: Map[String, Set[String]],
                    a: String,
                    b: String): Map[String, Set[String]] = {
    // TBD: Add error check if a and b are missing from the network
    network map {
      case (pers, friends) if pers == a => (pers, friends + b)
      case (pers, friends) if pers == b => (pers, friends + a)
      case (pers, friends) => (pers, friends)
    }
  }

  //---------------------------------------------------------------------------
  def unfriend(
       network: Map[String, Set[String]],
       a: String,
       b: String): Map[String, Set[String]] = {
    // If either person is missing from the network, return the same network
    if !network.contains(a) || !network.contains(b) then network
    else {
      // update the friends sets of each person
      val newFriendsOfA = network(a) - b
      val newFriendsOfB = network(b) - a
      // update the network with the new friends sets
      network + (a -> newFriendsOfA) + (b -> newFriendsOfB)
    }
  }

  //---------------------------------------------------------------------------
  // Returns the number of friends. Returns -1 if the person isn't present in the network
  def numFriends(network: Map[String, Set[String]], person: String): Int =
    if !network.contains(person) then -1
    else network(person).size

  //---------------------------------------------------------------------------
  /**
    Given a social network, returns a Map with the friend count as the key and
    a list of people as the value. This is a helper method; not part of the original exercise.

    Example input:
      Map(
       "Jim"    -> Set("Bob", "Mary", "Daniel"),  // most number of friends
       "Tim"    -> Set(),                         // no friends
       "Bob"    -> Set("Jim", "Mary"),
       "Mary"   -> Set("Jim", "Bob"),
       "Daniel" -> Set("Jim")
      )

    Intermediate friendCounts:
      HashMap(
        Jim -> 3,
        Bob -> 2,
        Tim -> 0,
        Daniel -> 1,
        Mary -> 2
      )

    Example output:
    Map(
      0 -> List(Tim),       // Tim has no friends
      1 -> List(Daniel),
      2 -> List(Bob, Mary), // Bob and Mary have two friends each
      3 -> List(Jim)        // Jim has the most number of friends
   )
   */
  def personsByFriendCount(network: Map[String, Set[String]]): Map[Int, Iterable[String]] = {
    val friendCounts: Map[String, Int] = // Map of person's name to their number of friends
      network.view
        .mapValues(friends => friends.size)
        .toMap
    // Group by values, which are the number of friends; extract the keys, which are person names
    friendCounts.groupMap(_._2)(_._1)
  }

  // ------------------------------------------------------------------------------------------------------------------
  // BETTER:
  // Less verbose but cryptic because of _._2 and _._1
  def personsByFriendCount_v2(network: Map[String, Set[String]]): Map[Int, Iterable[String]] =
    network.groupMap(_._2.size)(_._1)

  // ------------------------------------------------------------------------------------------------------------------
  // BEST:
  // This version is functionally equivalent to the above one but has better readability, as it
  // eliminates the _._1 and _._2
  def personsByFriendCount_v3(network: Map[String, Set[String]]): Map[Int, Iterable[String]] = {
    // Group by values, which are the number of friends; extract the keys, which are person names
    network.groupMap {
      case (person, friends) => friends.size // group by the number of friends
    }{
      case (person, friendCount) => person
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // who has the most friends
  def mostFriends(network: Map[String, Set[String]]): Set[String] = {
    val friendCounts:   Map[Int, Iterable[String]]  = personsByFriendCount_v3(network)
    val maxFriendCount: Int                         = friendCounts.keys.toList.max
    friendCounts(maxFriendCount).toSet
  }

  // ------------------------------------------------------------------------------------------------------------------
  // helper method for if-then-else
  extension [A](condition: => Boolean)
    def when(thenPart: => A)(elsePart: => A): A =
      if condition then thenPart else elsePart

  // ------------------------------------------------------------------------------------------------------------------
  // Instructor's version using foldLeft. Returns only one person with the most friends rather than all of them
  def mostFriends_v2(network: Map[String, Set[String]]): String = {
    if network.isEmpty then
      throw new RuntimeException("Network is empty, so there's no one with most friends")
    else {
      val best = network.foldLeft("", -1) { (bestSoFar, curKeyVal) =>
        val bestName  = bestSoFar._1  // String
        val bestCount = bestSoFar._2  // Int
        val curName   = curKeyVal._1  // String
        val curCount  = curKeyVal._2.size // Int
        when (curCount > bestCount) ((curName, curCount)) (bestSoFar)
      }
      best._1 // Return the person's name
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Same as instructor's version, but uses pattern matching. Also uses the 'when' method as a .when on the condition
  def mostFriends_v3(network: Map[String, Set[String]]): String = {
    if network.isEmpty then
      throw new RuntimeException("Network is empty, so there's no one with most friends")
    else {
      val best = network.foldLeft("", -1) {
        case (bestSoFar @ (bestName, bestCount), (curName, curFriends)) =>
          val curCount  = curFriends.size // Int
          (curCount > bestCount).when ((curName, curCount)) (bestSoFar)
      }
      best._1 // Return the person's name
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // Returns the names of all the people who have no friends
  def noFriends(network: Map[String, Set[String]]): Set[String] = {
    val friendCounts: Map[Int, Iterable[String]] = personsByFriendCount_v3(network)
    friendCounts(0).toSet
    // friendsCounts(0).size if you want to return the number of people with no friends
  }

  def numFriendlessPeople(network: Map[String, Set[String]]): Int =
    network
      .filter(pair => pair._2.isEmpty)
      .size

  // Better version that replaces filter and size with count. (IDE's suggestion)
  def numFriendlessPeople_v2(network: Map[String, Set[String]]): Int =
    network.count(_._2.isEmpty)

  // ------------------------------------------------------------------------------------------------------------------
  // Returns true if a and b are connected to each other by being friends or friends' friends, etc.
  // breadth-first search
  def socialConnection(network: Map[String, Set[String]], a: String, b: String): Boolean = {
    /**
     * discoveredPeople = yet to be discovered; yet to be traversed
     * consideredPeople = people that have been explored so far; already traversed.
     *
     * Examples
     * network = Map (Bob -> Set(Mary), Mary -> Set(Bob, Jim), Jim -> Set(Mary, Daniel), Daniel -> Set(Jim))
     *
     * 1. Is Bob connected to Jim?
     * socialConnection(network, Bob, Jim)
     * search([Mary], [Bob]) // Mary is Bob's friend
     * true since Mary's friends contains Jim
     *
     * 2. Is Bob connected to Daniel?
     * socialConnection(network, Bob, Daniel)
     * search([Mary], [Bob])
     * search([Bob, Jim] - Bob, [Mary, Bob]) == search([Jim], [Mary, Bob])
     * true since Jim's friend list contains Daniel
     */
    @tailrec
    def search(toBeDiscoveredPeople: Set[String], consideredPeople: Set[String]): Boolean =
      if toBeDiscoveredPeople.isEmpty then
        false
      else {
        // pick a person from the people yet to be discovered and look at their set of friends
        val person = toBeDiscoveredPeople.head
        val personsFriends = network(person)

        if personsFriends.contains(b) then true
        else {
          val newToBeDiscovered = toBeDiscoveredPeople - person ++ personsFriends -- consideredPeople
          search(newToBeDiscovered, consideredPeople + person)
        }
      }

    if !network.contains(a) || !network.contains(b) then false
    else search(network(a), Set(a))  // Start searching from a's friends
  }

  // ------------------------------------------------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    val network = Map(
      "Jim" -> Set("Bob", "Mary", "Daniel"),  // most number of friends
      "Tim" -> Set(), // no friends
      "Bob" -> Set("Jim", "Mary"),
      "Mary" -> Set("Jim", "Bob"),
      "Daniel" -> Set("Jim")
    )
    println(network)

    // Create the same network by adding persons and their friends
    val empty: Map[String, Set[String]] = Map()
    val network1 = addPerson(addPerson(addPerson(addPerson(addPerson(empty, "Jim"), "Tim"), "Bob"), "Mary"), "Daniel")
    val network2 = addFriend(
          addFriend(addFriend(addFriend(network1, "Daniel", "Jim"), "Mary", "Jim"), "Mary", "Bob"), "Bob", "Jim")

    println(network2)

    println(personsByFriendCount(network))
    /* Output is
      Map(
        0 -> List(Tim),
        1 -> List(Daniel),
        2 -> List(Bob, Mary),
        3 -> List(Jim)
      )
     */

    println(mostFriends(network))           // Set(Jim)
    println(mostFriends_v3(network))        // Jim
    println(noFriends(network))             // Set(Tim)
    println(numFriendlessPeople(network))   // 1

    println(socialConnection(network, "Mary", "Daniel")) // true
    println(socialConnection(network, "Tim", "Daniel"))  // false
    println(socialConnection(network, "Bob", "Tim"))     // false
  }
}

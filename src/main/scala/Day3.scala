object Day3 {

/*

Part 1

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

Data from square 1 is carried 0 steps, since it's at the access port.
Data from square 12 is carried 3 steps, such as: down, left, left.
Data from square 23 is carried only 2 steps: up twice.
Data from square 1024 must be carried 31 steps.

We can determine which 'ring' a number is in by the sequence of numbers

n^2, (n+2)^2, (n+2+2)^2, ...

i  (1 + (i*2)) ^ 2
0  1
1  9
2  25
3  49

By calculating where a number fits in this sequence we can determine the ring.

Unfortunately knowing the ring doesn't give us the Manhattan distance, for that we
need the x and y delta from zero ...

A simple way to solve this puzzle is to build the spiral manually as a look up table
of memory address to coord. A map is an obvious choice to store this.

Then we can simply find the coord of a memory address and return the manhattan distance of
that coord

 */


  // When generating a spiral we want to know how long each side of each ring is
  // This can be calculated easily ...
  // 1 2 3 4 5
  // 1 3 5 7 9
  def getRingSide(ring: Int) : Int = 1 + ((ring - 1) * 2)

  // A simple Coord class that lets us get adjacent coords
  // and the manhattan distance
  case class Coord(x: Int, y: Int) {

    def right: Coord = this.copy(x = x + 1)
    def left: Coord = this.copy(x = x - 1)
    def up: Coord = this.copy(y = y + 1)
    def down: Coord = this.copy(y = y - 1)

    def manhattan : Int = Math.abs(x) + Math.abs(y)

  }

  // Our look up table is a map of memory address to Coord
  type RingMap = Map[Int, Coord]

/*
  How to build a spiral


           543
           612  (0,0)
           789

  Using a fold we will iteratively build the spiral by moving up, left, down and right
  at the appropriate number of steps for the current ring size

 */

  // On entry we start with startNum and coord, add that the map and unless done recursively call this
  // moving around in a spiral following the rules above
  def buildRingNFromPoint(startNum : Int, startPos: Coord, ringMap : RingMap, ringNum : Int) : (Int, RingMap, Coord) = {

    // we need to know the size of the sides of the spiral which can be calculated by ring num
    val sides = getRingSide(ringNum)

    val coordsToGenerate = sides * 2 + (sides - 2) * 2

    val result = (1 to coordsToGenerate).foldLeft((startNum, startPos, ringMap)) {
      case ((curNum,curPos,curMap), num) =>

        num / (sides-1) match {
          case 0 => (curNum + 1, curPos.up, curMap updated (curNum, curPos))
          case 1 => (curNum + 1, curPos.left, curMap updated (curNum, curPos))
          case 2 => (curNum + 1, curPos.down, curMap updated (curNum, curPos))
          case 3 => (curNum + 1, curPos.right, curMap updated (curNum, curPos))
          case 4 => (curNum + 1, curPos, curMap updated (curNum, curPos))
        }
    }

    (result._1, result._3, result._2)
  }


  /**
    * Build a spiral memory incrementing the address location
    * @param numRings How many rings to generate
    * @return Map of addresses and their coordinates
    */
  def buildSpiral(numRings: Int) : RingMap = {

    // Note it would be nice to generate location 1 as part of the same loop
    val ring1 = Map(1 -> Coord(0,0))

    (2 to numRings).foldLeft((2, ring1, Coord(0,0))) {
      case ((startNum, ringMap, startPos), ringNum) =>

        buildRingNFromPoint(startNum, startPos.right, ringMap, ringNum)

    }._2
  }

  // We need to know how many rings of the spiral to generate
  // which can be calculated iteratively as follows ...

  def getRingNumberForLocation(location: Int, ring : Int = 0) : Int = {

    val s = getRingSide(ring)
    val maxForRing = s * s

    if(location <= maxForRing) ring
    else getRingNumberForLocation(location, ring + 1)

  }

  // Part two

  type SpiralMemory = Map[Int, Int]
  type ReverseRingMap = Map[Coord, Int]

  def reverseRingMapFromRingMap(r: RingMap) : ReverseRingMap = {

    r.foldLeft(Map.empty[Coord,Int]) {
      case (rrm, (k,v)) => rrm updated (v,k)
    }
  }

  def storeNeighbourSum(location: Int, memory: SpiralMemory, ringMap: RingMap, reverseRingMap: ReverseRingMap) : SpiralMemory = {

    // Square one is a special case

    if(location == 1) memory updated (1, 1)
    else {
      // Get the coord of this location and all the neighbours
      val thisCoord: Coord = ringMap(location)

      val right = thisCoord.right
      val left = thisCoord.left
      val up = thisCoord.up
      val down = thisCoord.down

      val rightdown = thisCoord.right.down
      val leftdown = thisCoord.left.down
      val rightup = thisCoord.right.up
      val leftup = thisCoord.left.up

      // Get the value at each point

      val neighbourLocations = List(
        reverseRingMap.get(right),
        reverseRingMap.get(left),
        reverseRingMap.get(up),
        reverseRingMap.get(down),
        reverseRingMap.get(rightdown),
        reverseRingMap.get(leftdown),
        reverseRingMap.get(rightup),
        reverseRingMap.get(leftup)

      ).flatten

      val sum = neighbourLocations.foldLeft(0) {

        case (acc, loc) =>
          acc + memory.getOrElse(loc, 0)
      }

      // Store it

      memory updated (location, sum)
    }

  }

  def main(args : Array[String]): Unit = {

    val testInput = "347991".toInt

    val maxRing = getRingNumberForLocation(testInput + 1)

    /*
      Data from square 1 is carried 0 steps, since it's at the access port.
      Data from square 12 is carried 3 steps, such as: down, left, left.
      Data from square 23 is carried only 2 steps: up twice.
      Data from square 1024 must be carried 31 steps.
     */

    // generate a spiral of the required number of rings (3)

    val spiralMemoryRingMap = buildSpiral(maxRing)

    assert(spiralMemoryRingMap(1).manhattan == 0)
    assert(spiralMemoryRingMap(12).manhattan == 3)
    assert(spiralMemoryRingMap(23).manhattan == 2)
    assert(spiralMemoryRingMap(1024).manhattan == 31)

    print(s"Part one answer: ${spiralMemoryRingMap(testInput).manhattan}")

    /*
     --- Part Two ---

      As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.

      So, the first few squares' values are chosen as follows:

      Square 1 starts with the value 1.
      Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
      Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
      Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
      Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
      Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

      147  142  133  122   59
      304    5    4    2   57
      330   10    1    1   54
      351   11   23   25   26
      362  747  806--->   ...
      What is the first value written that is larger than your puzzle input?

      Your puzzle input is still 347991.
     */

    // So to perform this calculation we need a way to store values in the memory locations
    // and a way to get the neighbouring values...
    // To that end we can make a new data structure that stores values by memory address
    // (1 -> 1, 2 -> 1, 3 -> 2 ...)
    // Then we can use the code and data from part one to get the coords of memory addresses
    // we intend to write and calculate the sum of the neighbours...
    // We also need to the find the memory location of a coord so let's make a reverse map for that

    val reverseRingMap = reverseRingMapFromRingMap(spiralMemoryRingMap)

    val memory = Map.empty[Int,Int]

    val test = (1 to 5).foldLeft(memory) {
      case (mem, loc) =>
        storeNeighbourSum(loc, mem, spiralMemoryRingMap, reverseRingMap)
    }

    print(test)

    // Ok that works, now we need to recursively find the first bigger value than our test input

    def firstBigger(currentLocation: Int, memory: SpiralMemory, ringMap: RingMap, reverseRingMap: ReverseRingMap, target: Int) : Int = {

      val updatedMemory = storeNeighbourSum(currentLocation, memory, spiralMemoryRingMap, reverseRingMap)

      // what value did we write?

      val value = updatedMemory(currentLocation)

      // is bigger than target?

      if(value > target) value
      else firstBigger(currentLocation + 1, updatedMemory, ringMap, reverseRingMap, target)

    }

    val partTwo = firstBigger(1, memory, spiralMemoryRingMap, reverseRingMap, testInput)

    println(s"Part two: $partTwo")

  }

}

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

    val genSpiral = buildSpiral(maxRing)

    assert(genSpiral(1).manhattan == 0)
    assert(genSpiral(12).manhattan == 3)
    assert(genSpiral(23).manhattan == 2)
    assert(genSpiral(1024).manhattan == 31)

    print(s"Step one answer: ${genSpiral(testInput).manhattan}")

  }

}

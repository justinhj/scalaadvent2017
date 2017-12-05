

object Day3 {


/*

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

Unfortunately knowing the ring doesn't give us the Manhatten distance, for that we
need the x and y delta from zero ...

We would need to calculate this x and y height using knowledge of the spiral
structure ...

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23  24  25

Notes
- the first number in the spiral is immediately to the right of 1
- its x distance will be the ring number -1
- eg 2 has x distance (2-1) and 11 has ring distance


 */

  def getRingSize(i : Int) : Int = {
    val x = 1 + (i*2)
    x * x
  }

  def getRingWidth(ring: Int) : Int = 1 + (ring * 2)

  case class Coord(x: Int, y: Int)

  type RingMap = Map[Int, Coord]

  /**
    * Given a ring number and a start pos, walk around and build the rings coords
    * @param startNum
    * @param ringNum
    * @param startPos
    * @param ringMap
    * @return
    */
  def buildRingNFromPoint(startNum : Int, ringNum : Int, startPos : Coord, ringMap: RingMap) : (RingMap, Coord, Int) = {
    ???
  }

  /**
    * Build a spiral of numRings rings by literally walking around in a spiral
    * @param numRings
    * @return
    */
  def buildSpiral(numRings: Int) : RingMap = {

    val ring1 = Map(1 -> Coord(0,0))

    (2 to numRings).foldLeft((2, ring1, Coord(0,0))) {
      case ((startNum, ringMap, startPos), ringNum) =>

        buildRingNFromPoint(startNum, ringNum, startPos, ringMap)

    }._1
  }

  def main(args : Array[String]): Unit = {

    val testInput = "347991"

    println(s"hello ${getRingWidth(3)}")


  }

}

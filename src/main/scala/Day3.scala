

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

-2  -1   0  1    2

17  16  15  14  13    2
18   5   4   3  12    1
19   6   1   2  11    0
20   7   8   9  10   -1
21  22  23  24  25   -2

Notes
- the first number in the spiral is immediately to the right of 1
- its x distance will be the ring number -1
- eg 2 has x distance (2-1) and 11 has ring distance

ring 2 size is 3, ring 3 size is 5 and so on
starting at 2 we go up 1, left 2, down 2, right 2
ring 3 is size 5
starting at previous end position (to the right of it) =
(1,-1) => (2,-1)
up 3 right 3 down 3 right 3
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
    * @param startNum start number of numbers in the spiral, eg ring 2 would be 2
    * @param ringNum ring number from 1 outwards
    * @param startPos startpos is the x y coord of where the spiral should be begin from
    * @param ringMap where to store the map
    * @return returns the number we got to, with ring 2 it would be 9. coord is the last coord and ring map is the map
    *         so far
    */
  def buildRingNFromPoint(startNum : Int, ringNum : Int, startPos : Coord, ringMap: RingMap) : (Int, Coord, RingMap) = {

    val ringSize = getRingWidth(ringNum)

    val rightSide: (RingMap, Coord, Int) = (startNum until (startNum + ringSize - 1)).foldLeft(ringMap, startPos, startNum) {
      case ((rm, pos, num1), num) =>

        val nextPos = pos.copy(y = pos.y + 1)

        (rm updated (num, pos), nextPos, num)
    }

    val topSide: (RingMap, Coord, Int) = (rightSide._3 to (rightSide._3 + ringSize - 1)).foldLeft(rightSide._1, rightSide._2, rightSide._3) {
      case ((rm, pos, num1), num) =>

        val nextPos = pos.copy(x = pos.x - 1)

        (rm updated (num, pos), nextPos, num)
    }


    ???
  }

  /**
    * Build a spiral of numRings rings by literally walking around in a spiral
    * @param numRings
    * @return
    */
  def buildSpiral(numRings: Int) : RingMap = {

    val ring1 = Map(1 -> Coord(0,0))

    (1 to numRings).foldLeft((2, Coord(1,0), ring1)) {
      case ((startNum, startPos, ringMap), ringNum) =>

        buildRingNFromPoint(startNum, ringNum, startPos, ringMap)

    }._3
  }

  def main(args : Array[String]): Unit = {

    val testInput = "347991"

    println(s"hello ${getRingWidth(3)}")

    val s = buildSpiral(3)

    s

  }

}

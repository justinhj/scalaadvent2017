

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

/*
  How to build a spiral


           543
           612  (0,0)
           789

  move right
  2 (1,0)
  if something to the left move up
  else if nothing to the left and something above move right
  else if nothing beneath and something to the right move down
  else move left
  else done

 */

  // on entry we start with startNum and coord, add that the map and unless done recursively call this
  // moving around in a spiral following the rules above
  def buildRingNFromPoint(startNum : Int, startPos: Coord, ringMap : RingMap) : (Int, RingMap, Coord) = {

    // see who's around us (this indicates we should probably use a graph)

    val alreadySet = ringMap.values.toSet

    val above = alreadySet.contains(startPos.copy(y = startPos.y + 1))
    val below = alreadySet.contains(startPos.copy(y = startPos.y - 1))
    val right = alreadySet.contains(startPos.copy(x = startPos.x + 1))
    val left = alreadySet.contains(startPos.copy(x = startPos.x - 1))

    if(left == true)  buildRingNFromPoint(startNum + 1, startPos.copy(y = startPos.y + 1), ringMap + (startNum -> startPos))
    else if(below == true) buildRingNFromPoint(startNum + 1, startPos.copy(x = startPos.x - 1), ringMap + (startNum -> startPos))
    else if(right == true)  buildRingNFromPoint(startNum + 1, startPos.copy(y = startPos.y - 1), ringMap + (startNum -> startPos))
    else if(above == true)  buildRingNFromPoint(startNum + 1, startPos.copy(x = startPos.x + 1), ringMap + (startNum -> startPos))
    else (startNum, ringMap, startPos)

  }


  /**
    * Build a spiral of numRings rings by literally walking around in a spiral
    * @param maxNum The number of spiral points to generate starting from 1
    * @return
    */
  def buildSpiral(maxNum: Int) : RingMap = {

    val ring1 = Map(1 -> Coord(0,0))

    (2 until maxNum).foldLeft((2, ring1, Coord(1,0))) {
      case ((startNum, ringMap, startPos), num) =>

        buildRingNFromPoint(num, startPos, ringMap)

    }._2
  }

  def main(args : Array[String]): Unit = {

    val testInput = "347991"

    println(s"hello ${getRingWidth(3)}")

    val s = buildSpiral(3)

    s


  }

}

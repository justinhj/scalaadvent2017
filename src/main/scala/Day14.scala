object Day14 {

  /**
    * --- Day 14: Disk Defragmentation ---

Suddenly, a scheduled job activates the system's disk defragmenter. Were the situation different, you might sit and watch
  it for a while, but today, you just don't have that kind of time. It's soaking up valuable system resources that are
  needed elsewhere, and so the only option is to help it finish its task as soon as possible.

The disk in question consists of a 128x128 grid; each square of the grid is either free or used. On this disk, the state
  of the grid is tracked by the bits in a sequence of knot hashes.

A total of 128 knot hashes are calculated, each corresponding to a single row in the grid; each hash contains 128 bits
  which correspond to individual grid squares. Each bit of a hash indicates whether that square is free (0) or used (1).

The hash inputs are a key string (your puzzle input), a dash, and a number from 0 to 127 corresponding to the row.
  For example, if your key string were flqrgnkx, then the first row would be given by the bits of the knot hash of
  flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and so on until the last row, flqrgnkx-127.

The output of a knot hash is traditionally represented by 32 hexadecimal digits; each of these digits correspond to 4 bits,
  for a total of 4 * 32 = 128 bits. To convert to bits, turn each hexadecimal digit to its equivalent binary value,
  high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111, and so on; a hash that begins with
  a0c2017... in hexadecimal would begin with 10100000110000100000000101110000... in binary.

Continuing this process, the first 8 rows and columns for key flqrgnkx appear as follows, using # to denote used
  squares, and . to denote free ones:

##.#.#..-->
.#.#.#.#
....#.#.
#.#.##.#
.##.#...
##..#..#
.#...#..
##.#.##.-->
|      |
V      V
In this example, 8108 squares are used across the entire 128x128 grid.

Given your actual key string, how many squares are used?

    Puzzle input is hfdlxzhv

    Now, all the defragmenter needs to know is the number of regions. A region is a group of used squares that are all adjacent, not including diagonals. Every used square is in exactly one region: lone used squares form their own isolated regions, while several adjacent squares all count as a single region.

In the example above, the following nine regions are visible, each marked with a distinct digit:

11.2.3..-->
.1.2.3.4
....5.6.
7.8.55.9
.88.5...
88..5..8
.8...8..
88.8.88.-->
|      |
V      V
Of particular interest is the region marked 8; while it does not appear contiguous in this small view, all of the squares marked 8 are connected when considering the whole 128x128 grid. In total, in this example, 1242 regions are present.

How many regions are present given your key string?

    */

  import Day10.calcHash

  def countHexDigitBits(hex: String) : Int = {
    val value = Integer.parseInt(hex, 16)
    val bs = value.toBinaryString
    bs.count(_ == '1')
  }

  def countBitsInHexString(hex: String) : Int = {
    hex.map(c => countHexDigitBits(c.toString)).sum
  }

  def getHashes(key: String) = {
    val keys = (0 to 127).map {
      n => s"$key-$n"
    }

    val hashes = keys.map {
      key => calcHash(key)
    }

    hashes
  }

  def countUsed(key: String) : Int = {

    val hashes = getHashes(key)

    val c = hashes.map{h => countBitsInHexString(h)}

    c.sum
  }

  // Step 2... we need to generate the memory cells in a grid so we can iterate through it, look at neighbours
  // and assign all the cells to a map of groups

  // convert the expanded hash string to a vector of boolean
  def hashStringToBooleanVector(hashString: String) : Vector[Boolean] = {

    val toHexStrings = hashString.grouped(4)

    val toBinaryVectors = toHexStrings.flatMap{
      row =>
        val value = Integer.parseInt(row, 16)
        val asBinary = value.toBinaryString

        // pad left

        val padded : String = if(asBinary.size < 16) {
                        val padLen = 16 - asBinary.size
                        val pad = List.fill(padLen)("0").mkString
                        pad ++ asBinary
                      } else {
                        asBinary
                      }

        padded.toString.map{
            case '1' => true
            case '0' => false
          }
    }

    toBinaryVectors.toVector
  }

  type MemoryMap = Vector[Vector[Boolean]]
  type ComponentMap = Map[(Int,Int), Int]

  def inputToVectorOfVector(input: String) : MemoryMap = {

    val hashes = getHashes(input).toVector
    hashes.map{hashStringToBooleanVector}

  }

  // test the memory value to the left of the given coord

  def leftIsSet(row : Int, col : Int, mm: MemoryMap) : Boolean = {
    if(col <= 0) false
    else mm(row)(col - 1)
  }

  def aboveIsSet(row : Int, col : Int, mm: MemoryMap) : Boolean = {
    if(row <= 0) false
    else mm(row - 1)(col)
  }

  def isSet(row : Int, col : Int, mm: MemoryMap) : Boolean = mm(row)(col)

  def rowColToIndex(row: Int, col: Int, width: Int) = col + (row * width)

  def tryToConnect(row: Int, col: Int, uf: UF, mm : MemoryMap, components: ComponentMap) : Unit = {

    if(isSet(row, col, mm)) {

      if(leftIsSet(row, col, mm)) {

        val p1 = components.get(row,col).get
        val p2 = components.get(row,col - 1).get

        uf.union(p1,p2)
      }

      if(aboveIsSet(row, col, mm)){

        val p1 = components.get(row - 1, col).get
        val p2 = components.get(row,col).get

        uf.union(p1,p2)
      }
    }

  }

  /*
    * step 1 enumerate the set memory with node ids Map[(col,row), Int]
    * step 2 create uf with the map size components
    * step 3 iterate over the memory and create links to left and above nodes
    * step 4 uf count should be correct
  */

  def countGroups(mm: MemoryMap) : Int = {

    val size = mm.size

    val allCoords = for (
      row <- 0 until size;
      col <- 0 until size
    ) yield (row, col)


    val (components, nextId) = allCoords.foldLeft(Map.empty[(Int,Int), Int], 0) {
          case ((acc, nextId), (row, col)) =>

            if(isSet(row, col, mm))
              (acc updated((row, col), nextId), nextId + 1)
            else
              (acc, nextId)
        }

    // Handle no set memory
    if(nextId == 0)  {
      0
    }
    else {

      val count = nextId

      val uf = new UF(count)

      for (
        row <- (0 until size);
        col <- (0 until size);
        _ = tryToConnect(row, col, uf, mm, components)

      ) yield ()


      uf.count()
    }

  }

  def main(args : Array[String]) : Unit = {

    assert(countBitsInHexString("f") == 4)

    assert(countBitsInHexString("d") == 3)

    assert(countBitsInHexString("a") == 2)

    assert(countBitsInHexString("1") == 1)

    assert(countBitsInHexString("1a") == 3)

    val testCount = countUsed("flqrgnkx")

    println(s"Used count (test): $testCount")

    val count = countUsed("hfdlxzhv")

    println(s"Used count (step1): $count")

    val testVector = Vector(
      false,false,false,false,
      false,false,false,false,
      false,false,false,true,
      false,false,false,true)

    val t2 = hashStringToBooleanVector("11")
    assert(t2 == testVector)

    val testVector2 = Vector(
      Vector(true,false,false,false),
      Vector(false,true,true,false),
      Vector(true,false,false,true),
      Vector(true,true,false,true))

    val testSmallGroupCount = countGroups(testVector2)

    val testGroupMM = inputToVectorOfVector("flqrgnkx")

    val testGroupCount = countGroups(testGroupMM)

    println(s"Test group answer is $testGroupCount")

    val step2GroupMM = inputToVectorOfVector("hfdlxzhv")

    val step2GroupCount = countGroups(step2GroupMM)

    println(s"Step 2 group answer is $step2GroupCount")

  }

}

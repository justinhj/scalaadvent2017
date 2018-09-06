object Day21 {

  import better.files.Resource
  import scalaz._, Scalaz._
  import java.nio.charset.Charset

  // A grid consists of a vector of rows. Each row is a vector of pixels

  type Pixel = Char

  type VectorVector[A] = Vector[Vector[A]]

  type Grid = VectorVector[Pixel]

  // When splitting the grid we return a vector of vector of grids

  type GridOfGrids = VectorVector[Grid]

  object Grid {

    // Count the pixels that are 'on'
    def gridCountPixels(g: Grid) : Int =
      g.map (_.count(_ == '#')).sum

    // For debugging render a grid of grids as a string
    def gridOfGridsToString(g: GridOfGrids) : String = {

      g.map {
        row =>
          row.map {
            col =>
              gridToString(col) + "\n"
          }.mkString("\n")
      }.mkString("\n\n")
    }

    // For debugging render a grid as a string
    def gridToString(g: Grid) : String = {

      val s = s"Size ${g.size}x${g.head.size}\nGrid\n"

      val s2 = g.map {

        row =>

          row.mkString

      }.mkString("\n")

      val both = s + s2
      both
    }

    /*
      Parse the input into a grid
      example input ../.#
      gives a grid 2x2 as so...
           ..
           .#
     */

    def gridFromString(in: String): Grid = {

      val rows: Array[String] = in.split("/")

      def rowToPixels(r: String): Vector[Pixel] = {
        r.toVector
      }

      rows.map(rowToPixels).toVector
    }

    // flip the input grid (along the vertical axis)
    def flipVertical(in: Grid): Grid = {

      in.map(_.reverse)
    }

    def flipHorizontal(in: Grid): Grid = {

      in.reverse
    }

    def getNthColumn(n: Int, in: Grid): Vector[Pixel] = {
      in.map(r => r(n))
    }

    // rotate grid clockwise
    // method here is that each row of the new grid becomes each column from the original grid

    def rotateGrid(in: Grid): Grid = {
      val rows = in.indices.map(getNthColumn(_, in).reverse)

      rows.toVector
    }

    // Given a list of rules and a grid, transform the grid

    def enhanceGrid(rules: List[TransformRule], input: Grid): Grid = {

      rules match {

        case rule :: rest =>

          rule.transform(input) match {

            case Some(g) =>
              g

            case None =>
              enhanceGrid(rest, input)
          }

        case Nil =>
          println("failed to match any rules")
          input

      }

    }

    def splitGrid(grid: Grid) : GridOfGrids = {

      // This is a simple but unsatisfying way to split the grid into smallers ones
      // Loops within loops simply build the smaller grids from the larger one. The code is not easy to
      // read, I may come back and refactor this

      if(grid.size % 2 == 0) {
        // split into 2's

        val newSize = grid.size / 2

        (0 until newSize).map {

          row =>

            (0 until newSize).map {

              col =>

                (0 until 2).map {

                  r =>

                    (0 until 2).map {

                      c =>

                        grid(row * 2 + r)(col * 2 + c)

                    }.toVector

                }.toVector

            }.toVector


        }.toVector
      }
      else {
        // split into 3's

        val newSize = grid.size / 3

        (0 until newSize).map {

          row =>

            (0 until newSize).map {

              col =>

                (0 until 3).map {

                  r =>

                    (0 until 3).map {

                      c =>

                        grid(row * 3 + r)(col * 3 + c)

                    }.toVector

                }.toVector

            }.toVector

        }.toVector

      }
    }

    // Combine a grid of grids to a grid
    def combine(g : GridOfGrids) : Grid = {

      g.flatMap{ r =>

        val size = r.head.size

        (0 until size).map {
          index =>

            r.foldLeft(Vector.empty[Vector[Char]]) {
              case (acc, s) =>

                acc ++ s.zipWithIndex.filter {
                  case (_, i) =>
                    index == i
                }.map(_._1)
            }.flatten
        }
      }

    }

    // Iterate the grid one step

    def iterate1(rules: List[TransformRule], input: Grid): Grid = {

      val split: Vector[Vector[Grid]] = splitGrid(input)

      val transformed: Vector[Vector[Grid]] = split.map {
        row =>
          row.map {
            col: Grid =>
              val w = enhanceGrid(rules, col)
              w
          }
      }

      combine(transformed)
    }

    // Iterate N times to solve the puzzle; a simple recursive method using iterate1

    def iterateN(rules: List[TransformRule], input: Grid, n: Int) : Grid = {
      if(n == 0)
        input
      else {

        val newGrid = iterate1(rules, input)

        //println(s"$n\n${gridToString(newGrid)}\n")
        println(s"$n")

        iterateN(rules, newGrid, n - 1)
      }
    }
  }

  import Grid._

  // Given the string encoding of a grid create a transform including the flipped and mirrored grids
  // which can then be used to transform the input

  case class TransformRule(transformEncoding : String) {

    private val inputOutput = transformEncoding.split(" => ")
    private val (inputStr, outputStr) = (inputOutput(0), inputOutput(1))

    val inputGrid : Grid = gridFromString(inputStr)
    val outputGrid : Grid = gridFromString(outputStr)

    // We want a list of the original input, the original input rotated 3 times
    // the flipped input, and that input rotated 3 times ...

    private val r1 = rotateGrid(inputGrid)
    private val r2 = rotateGrid(r1)
    private val r3 = rotateGrid(r2)

    private val flippedGridVertical = flipVertical(inputGrid)

    private val fr1 = flipVertical(r1)
    private val fr2 = flipVertical(r2)
    private val fr3 = flipVertical(r3)

    private val all = List(inputGrid, r1, r2, r3, flippedGridVertical, fr1, fr2, fr3)

    // Apply the first matching rule to the input
    // If no rules match we return None
    def transform(input: Grid): Option[Grid] = {

      if(input.size != all.head.size)
        None
      else {
        all.iterator.find {
          r =>
            //println(s"compare\n${gridToString(r)}\nwith\n${gridToString(input)}")
            r == input
        }.map{
          m =>
            //println(s"matched rule:\n${gridToString(m)}")

            outputGrid}
      }

    }
  }

  def main(args: Array[String]): Unit = {

    val dudical = TransformRule("../.# => ##./#../...")

    val test1Grid = gridFromString("../.#")

    val dudicalTransformed = dudical.transform(test1Grid)

    assert(dudicalTransformed.contains(gridFromString("##./#../...")))

    // test flip
    val t1 = gridFromString("###/#..")
    val t1flipped = gridFromString("###/..#")

    assert(flipVertical(t1) == t1flipped)

      /*
      #..   ###
      #.#   ..#
      ##.   .#.
       */

    // test rotate
    val r1 = gridFromString("#../#.#/##.")
    val r1Rotated = gridFromString("###/#../.#.")

    val actual = rotateGrid(r1)

    assert(actual === r1Rotated)

    val sampleRule1 = "../.# => ##./#../..."
    val sampleRule2 = ".#./..#/### => #..#/..../..../#..#"

    val sampleRules = List(TransformRule(sampleRule1), TransformRule(sampleRule2))

//    val sampleIterate1 = enhanceGrid(sampleRules, gridFromString(".#./..#/###"))
//
//    println(sampleIterate1)

    val rulesStrings = Resource.getAsString("input21.txt")(Charset.forName("US-ASCII")).split("\n")

    val rules = rulesStrings.map(TransformRule).toList

    val sampleStart = gridFromString(".#./..#/###")

    val splitSample = gridFromString("AABB/AABB/CCDD/CCDD")
    val split: GridOfGrids = splitGrid(splitSample)
    println(s"grid of grids\n${gridOfGridsToString(split)}")
    val combineSample = combine(split)
    println(s"combine\n${gridToString(combineSample)}")

    val splitSample2 = gridFromString("AAA/BBB/CCC")
    val split2: GridOfGrids = splitGrid(splitSample2)
    println(s"grid of grids 2\n${gridOfGridsToString(split2)}")

    println(s"$split2")

    val combineSample2 = combine(split2)

    println(s"combine2\n${gridToString(combineSample2)}")

    val step1a = iterateN(sampleRules, sampleStart, 2)
    val lightsOna = gridCountPixels(step1a)
    println(s"lightsOn2 is $lightsOna")

    val step1 = iterateN(rules, sampleStart, 5)

    println("step1:\n" + gridToString(step1))

    val lightsOn = gridCountPixels(step1)

    println(s"lightsOn is $lightsOn")

    val step2 = iterateN(rules, sampleStart, 18)

    println("step2:\n" + gridToString(step2))

    val lightsOn2 = gridCountPixels(step2)

    println(s"lightsOn2 is $lightsOn2")

  }

}

/*
--- Day 21: Fractal Art ---
You find a program trying to generate some art. It uses a strange process that involves repeatedly enhancing the detail
of an image through a set of rules.

The image consists of a two-dimensional square grid of pixels that are either on (#) or off (.). The program always
begins with this pattern:

.#.
..#
###`
Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have a size of 3.

Then, the program repeats the following process:

If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and convert each 2x2 square into a 3x3
square by following the corresponding enhancement rule.
Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3 squares, and convert each 3x3 square into a
4x4 square by following the corresponding enhancement rule.
Because each square of pixels is replaced by a larger one, the image gains pixels and so its size increases.

The artist's book of enhancement rules is nearby (your puzzle input); however, it seems to be missing rules. The artist
explains that sometimes, one must rotate or flip the input pattern to find a match. (Never rotate or flip the output
pattern, though.) Each pattern is written concisely: rows are listed as single units, ordered top-down, and separated
by slashes. For example, the following rules correspond to the adjacent patterns:

../.#  =  ..
          .#

                .#.
.#./..#/###  =  ..#
                ###

                        #..#
#..#/..../#..#/.##.  =  ....
                        #..#
                        .##.
When searching for a rule to use, rotate and flip the pattern as necessary. For example, all of the following patterns
match the same rule:

.#.   .#.   #..   ###
..#   #..   #.#   ..#
###   ###   ##.   .#.
Suppose the book contained the following two rules:

../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#
As before, the program begins with this pattern:

.#.
..#
###
The size of the grid (3) is not divisible by 2, but it is divisible by 3. It divides evenly into a single square; the
square matches the second rule, which produces:

#..#
....
....
#..#
The size of this enhanced grid (4) is evenly divisible by 2, so that rule is used. It divides evenly into four squares:

#.|.#
..|..
--+--
..|..
#.|.#
Each of these squares matches the same rule (../.# => ##./#../...), three of which require some flipping and rotation
to line up with the rule. The output for the rule is the same in all four cases:

##.|##.
#..|#..
...|...
---+---
##.|##.
#..|#..
...|...
Finally, the squares are joined into a new grid:

##.##.
#..#..
......
##.##.
#..#..
......
Thus, after 2 iterations, the grid contains 12 pixels that are on.

How many pixels stay on after 5 iterations?
 */
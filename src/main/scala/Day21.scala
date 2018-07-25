import java.nio.charset.Charset

object Day21 {

  import better.files.Resource
  import scalaz._, Scalaz._

  // A grid consists of a vector of rows. Each row is a vector of pixels

  type Pixel = Char

  type VectorVector[A] = Vector[Vector[A]]

  type Grid = VectorVector[Pixel]

  // When splitting the grid we return a vector of vector of grids

  type GridOfGrids = VectorVector[Grid]

  object Grid {

    /*

      example input ../.#
      gives a grid 2x2

     */

    def gridFromString(in: String): Grid = {

      val rows: Array[String] = in.split("/")

      def rowToPixels(r: String): Vector[Pixel] = {
        r.toVector
      }

      rows.map(rowToPixels(_)).toVector
    }

    // flip the input grid (along the vertical axis)
    def flipGrid(in: Grid): Grid = {

      in.map(_.reverse)
    }

    def getNthColumn(n: Int, in: Grid): Vector[Pixel] = {
      in.map(r => r(n))
    }

    // rotate grid clockwise
    // method here is that each row of the new grid becomes each column from the original grid

    def rotateGrid(in: Grid): Grid =
      in.indices.map(getNthColumn(_, in)).toVector

    // Given a list of rules and a grid, transform the grid

    def enhanceGrid(rules: List[MultiTransform], input: Grid): Grid = {

      rules match {

        case rule :: rest =>

          rule.transform(input) match {

            case Some(g) =>
              println(s"matched rule ${rule.inputGrid}")
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

      if(grid.size % 3 == 0) {
        // split into 3's

        val newSize = grid.size / 3

        val dafuck: Vector[Vector[Vector[Vector[Pixel]]]] = (0 until newSize).map {

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


        dafuck

      }
      else {
        // split into 2's

        val splitRows: Vector[Vector[Vector[Pixel]]] = grid.map(_.grouped(2).toVector)

        val groupedRows: Vector[Vector[Vector[Vector[Pixel]]]] = splitRows.grouped(2).toVector

        groupedRows

      }


    }

    // Combine a grid of grids to a grid

    def combine(g : GridOfGrids) : Grid = {

      val what: Vector[Vector[Pixel]] = g.map(_.flatten).flatten
      what

    }

    // Iterate the grid one step

    def iterate(rules: List[MultiTransform], input: Grid): Grid = {

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
  }

  import Grid._

  // Given the string encoding of a grid create a transform including the flipped and mirrored grids

  case class MultiTransform(transformEncoding : String) {

    private val inputOutput = transformEncoding.split(" => ")
    private val (inputStr, outputStr) = (inputOutput(0), inputOutput(1))

    val inputGrid : Grid = gridFromString(inputStr)
    val outputGrid : Grid = gridFromString(outputStr)

    // We want a list of the original input, the original input rotated 3 times
    // the flipped input, and that input rotated 3 times ...

    private val r1 = rotateGrid(inputGrid)
    private val r2 = rotateGrid(r1)
    private val r3 = rotateGrid(r2)

    private val flippedGrid = flipGrid(inputGrid)

    private val fr1 = rotateGrid(flippedGrid)
    private val fr2 = rotateGrid(fr1)
    private val fr3 = rotateGrid(fr2)

    private val all = List(inputGrid, r1, r2, r3, flippedGrid, fr1, fr2, fr3)

    def transform(input: Grid): Option[Grid] = {

      if(input.size != all.head.size)
        None
      else {
        all.iterator.find {
          r =>
            //println(s"compare $r with $input")
            r === input
        }.map{
          _ =>
            outputGrid}
      }

    }
  }

  def main(args: Array[String]): Unit = {

    val dudical = MultiTransform("../.# => ##./#../...")

    val test1Grid = gridFromString("../.#")

    val dudicalTransformed = dudical.transform(test1Grid)

    assert(dudicalTransformed.contains(gridFromString("##./#../...")))

    // test flip
    val t1 = gridFromString("###/#..")
    val t1flipped = gridFromString("###/..#")

    assert(flipGrid(t1) == t1flipped)

      /*
      #..   ###
      #.#   ..#
      ##.   .#.
       */

    // test rotate
    val r1 = gridFromString("#../#.#/##.")
    val r1Rotated = gridFromString("###/..#/.#.")

    val actual = rotateGrid(r1)

    assert(actual == r1Rotated)

    val sampleRule1 = "../.# => ##./#../..."
    val sampleRule2 = ".#./..#/### => #..#/..../..../#..#"

    val sampleRules = List(MultiTransform(sampleRule1), MultiTransform(sampleRule2))

//    val sampleIterate1 = enhanceGrid(sampleRules, gridFromString(".#./..#/###"))
//
//    println(sampleIterate1)

    val rulesStrings = Resource.getAsString("input21.txt")(Charset.forName("US-ASCII")).split("\n")

    val rules = rulesStrings.map(MultiTransform(_)).toList

    val sampleStart = gridFromString(".#./..#/###")

    //val sampleStart2 = gridFromString("#..#/..../..../#..#")
    val oopsie = iterate(sampleRules, sampleStart)

    println(oopsie)

    var x = 1
    x = 2


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
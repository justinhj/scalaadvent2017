import scala.io.Source

object Day9 {

  /*
--- Day 9: Stream Processing ---
A large stream blocks your path. According to the locals, it's not safe to cross the stream at the moment because
it's full of garbage. You look down at the stream; rather than water, you discover that it's a stream of characters.

You sit for a while and record part of the stream (your puzzle input). The characters represent groups -
sequences that begin with { and end with }. Within a group, there are zero or more other things, separated by commas:
either another group or garbage. Since groups can contain other groups, a } only closes the most-recently-opened
unclosed group - that is, they are nestable. Your puzzle input represents a single, large group which itself contains many smaller ones.

Sometimes, instead of a group, you will find garbage. Garbage begins with < and ends with >. Between those angle
 brackets, almost any character can appear, including { and }. Within garbage, < has no special meaning.

In a futile attempt to clean up the garbage, some program has canceled some of the characters within it using !:
inside garbage, any character that comes after ! should be ignored, including <, >, and even another !.

You don't see any characters that deviate from these rules. Outside garbage, you only find well-formed groups,
 and garbage always terminates according to the rules above.

Here are some self-contained pieces of garbage:

<>, empty garbage.
<random characters>, garbage containing random characters.
<<<<>, because the extra < are ignored.
<{!>}>, because the first > is canceled.
<!!>, because the second ! is canceled, allowing the > to terminate the garbage.
<!!!>>, because the second ! and the first > are canceled.
<{o"i!a,<{i<a>, which ends at the first >.
Here are some examples of whole streams and the number of groups they contain:

{}, 1 group.
{{{}}}, 3 groups.
{{},{}}, also 3 groups.
{{{},{},{{}}}}, 6 groups.
{<{},{},{{}}>}, 1 group (which itself contains garbage).
{<a>,<a>,<a>,<a>}, 1 group.
{{<a>},{<a>},{<a>},{<a>}}, 5 groups.
{{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).
Your goal is to find the total score for all groups in your input. Each group is assigned a score which is
one more than the score of the group that immediately contains it. (The outermost group gets a score of 1.)

{}, score of 1.
{{{}}}, score of 1 + 2 + 3 = 6.
{{},{}}, score of 1 + 2 + 2 = 5.
{{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
{<a>,<a>,<a>,<a>}, score of 1.
{{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
{{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
{{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
What is the total score for all groups in your input?
 */

  case class StreamState(
                          groupDepth: Int,
                          garbage: Boolean,
                          score: Int,
                          garbageCounter: Int,
                          ignoreNext: Boolean
                        )

  def evalStream(input: String): (Int, Int) = {

    val r = input.foldLeft(StreamState(0, false, 0, 0, false)) {

      case (streamState, c) =>

        if (streamState.ignoreNext) {
          streamState.copy(ignoreNext = false)
        }
        else {
          c match {
            case '{' if streamState.garbage == false =>
              streamState.copy(groupDepth = streamState.groupDepth + 1)

            case '}' if streamState.garbage == false =>
              streamState.copy(groupDepth = streamState.groupDepth - 1, score = streamState.score + streamState.groupDepth)

            case ',' if streamState.garbage == false =>
              streamState

            case '<' =>
              streamState.copy(garbage = true)

            case '>' =>
              streamState.copy(garbage = false)

            case '!' =>
              streamState.copy(ignoreNext = true)

            case _ if streamState.garbage == true =>
              streamState.copy(garbageCounter = streamState.garbageCounter + 1)

            case what =>
              streamState

          }
        }


    }

    (r.score, r.garbageCounter)

  }

  def main(args: Array[String]): Unit = {
    assert(evalStream("{}")._1 == 1)
    assert(evalStream("{{{}}}")._1 == 6)
    assert(evalStream("{{},{}}")._1 == 5)
    assert(evalStream("{{{},{},{{}}}}")._1 == 16)
    assert(evalStream("{<a>,<a>,<a>,<a>}")._1 == 1)
    assert(evalStream("{{<ab>},{<ab>},{<ab>},{<ab>}}")._1 == 9)
    assert(evalStream("{{<!!>},{<!!>},{<!!>},{<!!>}}")._1 == 9)
    assert(evalStream("{{<a!>},{<a!>},{<a!>},{<ab>}}")._1 == 3)

    assert(evalStream("{<a>,<a>,<a>,<a>}")._2 == 4)

    assert(evalStream("<>")._1 == 0)
    assert(evalStream("<random characters>")._1 == 0)

    val step1Input = Source.fromResource("input9.txt").mkString

    // 5136 (too low)

    println(s"step 1 : ${evalStream(step1Input)._1}")
    println(s"step 2 : ${evalStream(step1Input)._2}")


  }
}

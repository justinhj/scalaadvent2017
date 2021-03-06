

object Day17 {

  case class CircularBuffer(stepSize : Int, var buffer : Vector[Int] = Vector(0), var currentPos : Int = 0) {

    def insertNext(next: Int) : Unit = {

      var steps = stepSize

      val size = buffer.size

      currentPos = (currentPos + stepSize) % size + 1

      buffer = buffer.take(currentPos) ++ Vector(next) ++ buffer.drop(currentPos)

    }

  }

  def main(args: Array[String]): Unit = {

    val cb = CircularBuffer(3)

    cb.insertNext(1)
    assert(cb.buffer == Vector(0,1))
    assert(cb.currentPos == 1)

    cb.insertNext(2)
    assert(cb.buffer == Vector(0,2,1))
    assert(cb.currentPos == 1)

    val cb2 = CircularBuffer(3)

        (1 to 2017).foreach {
          n => cb2.insertNext(n)
          //println(cb2.buffer)
        }

    println(cb2.buffer(cb2.currentPos + 1))

    val step1CB = CircularBuffer(369)

    (1 to 2017).foreach {
      n =>
        step1CB.insertNext(n)
//        println(s"len ${step1CB.buffer.size} pos ${step1CB.currentPos} n $n")
//        if(step1CB.currentPos == 1) println(s"new at position 1 is ${step1CB.buffer(1)}")
    }

    println(step1CB.buffer(step1CB.currentPos + 1))

//    val step2CB = CircularBuffer(369)
//
//    val seconds = (1 to 50000000).map {
//      n =>
//        step2CB.insertNext(n)
//        step2CB.buffer(1)
//    }

//    println(s"seconds $seconds")
//
//    val nMap = seconds.foldLeft(scala.collection.immutable.SortedMap.empty[Int,Int]) {
//      case (acc, n) =>
//        val count = acc.getOrElse(n, 0)
//        acc updated (n, count + 1)
//    }
//
//    nMap.foreach {
//      case (key,value) =>
//        println(s"n $key count $value")
//
//    }

    // This is too slow to brute force so the idea must be to predict the element at (1)
    // Note that the element at position 1 only changes when current pos is at 1
    // and maybe that's predictable because it's a function of the growing size of the
    // circular buffer and when it does change it will be equal to the new current position

    // so you need a calculation for modulo over a linear increasing size

    /*Vector(0, [1])
      Vector(0, [2], 1)
      Vector(0, [2], 3, 1)
      Vector(0, 2, 3, 1, 4)
      Vector(0, 2, 3, 1, 5, 4)
      Vector(0, 2, 6, 3, 1, 5, 4)
      Vector(0, 7, 2, 6, 3, 1, 5, 4)
      Vector(0, 7, 2, 8, 6, 3, 1, 5, 4)
      Vector(0, 7, 2, 8, 9, 6, 3, 1, 5, 4)
      Vector(0, 7, 2, 8, 10, 9, 6, 3, 1, 5, 4)
      Vector(0, 7, 2, 8, 10, 9, 6, 3, 1, 5, 4, 11)
      Vector(0, 7, 2, 8, 10, 9, 6, 3, 1, 12, 5, 4, 11)
      Vector(0, 7, 13, 2, 8, 10, 9, 6, 3, 1, 12, 5, 4, 11)
      Vector(0, 7, 13, 2, 8, 10, 9, 6, 14, 3, 1, 12, 5, 4, 11)
      Vector(0, 7, 13, 15, 2, 8, 10, 9, 6, 14, 3, 1, 12, 5, 4, 11)
      Vector(0, 7, 13, 15, 2, 16, 8, 10, 9, 6, 14, 3, 1, 12, 5, 4, 11)
      Vector(0, 17, 7, 13, 15, 2, 16, 8, 10, 9, 6, 14, 3, 1, 12, 5, 4, 11)
      Vector(0, 17, 7, 13, 15, 2, 16, 8, 10, 9, 6, 18, 14, 3, 1, 12, 5, 4, 11)
      Vector(0, 19, 17, 7, 13, 15, 2, 16, 8, 10, 9, 6, 18, 14, 3, 1, 12, 5, 4, 11)
      Vector(0, 19, 17, 7, 13, 15, 2, 16, 8, 10, 9, 20, 6, 18, 14, 3, 1, 12, 5, 4, 11) */


    val steps = 369

    // need to iterate storing current length of buffer (increases by one each time) and current position (modulo
    // arithmetic. this takes a few seconds to get the right answer

    val result = (1 to 50000000).foldLeft((1, 0, 0)) {

      case ((len, curpos, pos1), n) =>

        val newLen = len + 1
        val newCurPos = (curpos + steps) % len + 1

        // If we are at position 1 record n as the last position 1 seen
        val newPos1 =
          if(newCurPos == 1) {
            n
          }
          else pos1

        (newLen, newCurPos, newPos1)

    }

    println(s"step 2 answer is ${result._2}")

    // result (50000001,39644617,31154878)

  }
}

/*
--- Day 17: Spinlock ---
Suddenly, whirling in the distance, you notice what looks like a massive, pixelated hurricane: a deadly spinlock. This
spinlock isn't just consuming computing power, but memory, too; vast, digital mountains are being ripped from the ground
and consumed by the vortex.

If you don't move quickly, fixing that printer will be the least of your problems.

This spinlock's algorithm is simple but efficient, quickly consuming everything in its path. It starts with a circular
buffer containing only the value 0, which it marks as the current position. It then steps forward through the circular
buffer some number of steps (your puzzle input) before inserting the first new value, 1, after the value it stopped on.
The inserted value becomes the current position. Then, it steps forward from there the same number of steps, and wherever
it stops, inserts after it the second new value, 2, and uses that as the new current position again.

It repeats this process of stepping forward, inserting a new value, and using the location of the inserted value as the
new current position a total of 2017 times, inserting 2017 as its final operation, and ending with a total of 2018 values
(including 0) in the circular buffer.


For example, if the spinlock were to step 3 times per insert, the circular buffer would begin to evolve like this (using
 parentheses to mark the current position after each iteration of the algorithm):

(0), the initial state before any insertions.
0 (1): the spinlock steps forward three times (0, 0, 0), and then inserts the first value, 1, after it. 1 becomes the
current position.
0 (2) 1: the spinlock steps forward three times (0, 1, 0), and then inserts the second value, 2, after it. 2 becomes the
current position.
0  2 (3) 1: the spinlock steps forward three times (1, 0, 2), and then inserts the third value, 3, after it. 3 becomes
the current position.
And so on:

0  2 (4) 3  1
0 (5) 2  4  3  1
0  5  2  4  3 (6) 1
0  5 (7) 2  4  3  6  1
0  5  7  2  4  3 (8) 6  1
0 (9) 5  7  2  4  3  8  6  1
Eventually, after 2017 insertions, the section of the circular buffer near the last insertion looks like this:

1512  1134  151 (2017) 638  1513  851
Perhaps, if you can identify the value that will ultimately be after the last value written (2017), you can short-circuit
the spinlock. In this example, that would be 638.

What is the value after 2017 in your completed circular buffer?

Your puzzle input is 369.


The spinlock does not short-circuit. Instead, it gets more angry. At least, you assume that's what happened; it's spinning
significantly faster than it was a moment ago.

You have good news and bad news.

The good news is that you have improved calculations for how to stop the spinlock. They indicate that you actually need to
identify the value after 0 in the current state of the circular buffer.

The bad news is that while you were determining this, the spinlock has just finished inserting its fifty millionth value
(50000000).

What is the value after 0 the moment 50000000 is inserted?

 */
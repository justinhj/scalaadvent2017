import scala.io.Source
import scala.util.Try

object Day25 {

  // A class to represent the input

  trait Move
  case object Left extends Move
  case object Right extends Move

  case class Op(writeValue: Int, move: Move, continueState: Char)

  case class State(zeroOp: Op, oneOp: Op)

  case class Program(startState: Char, checksumAtStep: Long, states : Map[Char, State])

  // The tape is made up of linked slots
  case class Slot(var value: Int = 0, var left: Option[Slot] = None, var right: Option[Slot] = None) {

    def leftMostSlot(slot: Slot) : Slot = {

      slot.left match {
        case Some(leftOfMe) => leftMostSlot(leftOfMe)
        case None => slot
      }

    }

    // not stack safe
    def tapeToString(slot: Slot, highlightSlot: Slot, currentSlot: Int) : String = {

      var s = if(highlightSlot == slot)
                s"[${slot.value}],"
              else
                s"${slot.value},"

      slot.right match {
        case Some(r) =>
          s ++ tapeToString(r, highlightSlot, currentSlot + 1)

        case None =>
          s

      }

    }

    def getTapeString(currentSlot : Slot) : String = {

      val left = leftMostSlot(this)

      val s = tapeToString(left, currentSlot, 0)

      s

    }

    def printTape(currentSlot : Slot) : Unit = {

      val left = leftMostSlot(this)

      val s = tapeToString(left, currentSlot, 0)

      println(s)

    }

    def countSetValuesToRight(slot: Slot) : Int = {

      val sum = slot.right match {

        case Some(s) => countSetValuesToRight(s)

        case None => 0

      }

      slot.value + sum

    }


    // get the count of the bits
    def count() : Int = {

      val leftSlot = leftMostSlot(this)

      countSetValuesToRight(leftSlot)

    }

  }

  object TuringMachine {

    def parseStartState(header: String) : Option[Char] = {

      // Example: Begin in state A.

      val pattern = """Begin in state ([A-Za-z]).""".r

      Try {
        val pattern(startState) = header

        startState.charAt(0)
      }.toOption

    }

    def parseChecksumSteps(header: String) : Option[Int] = {

      // Example: Perform a diagnostic checksum after 12261543 steps.

      val pattern = """Perform a diagnostic checksum after ([0-9]+) steps.""".r

      Try {
        val pattern(checksumpSteps) = header

        checksumpSteps.toInt

      }.toOption

    }

    def parseStates(input: List[String], states: Map[Char, State]) : Map[Char, State] = {

      val next10Lines = input.take(10)
      val remainingLines = input.drop(10)

      /* Example

          In state A:
            If the current value is 0:
              - Write the value 1.
              - Move one slot to the right.
              - Continue with state B.
            If the current value is 1:
              - Write the value 0.
              - Move one slot to the left.
              - Continue with state C.
         */

      val pattern = """
                      |In state ([A-Za-z]+):
                      |  If the current value is 0:
                      |    - Write the value ([0-9]+).
                      |    - Move one slot to the ([a-z]+).
                      |    - Continue with state ([A-Za-z]+).
                      |  If the current value is 1:
                      |    - Write the value ([0-9]+).
                      |    - Move one slot to the ([a-z]+).
                      |    - Continue with state ([A-Za-z]+).
                      |""".stripMargin.r


      val tenLines = next10Lines.foldLeft("") {
        case (acc, n) =>
          acc ++ n ++ "\n"
      }

      val parsedState = Try {
        val pattern(thisState, zeroWriteValue, zeroMoveDirection, zeroContinueState,
                    oneWriteValue, oneMoveDirection, oneContinueState) = tenLines

        val zeroMove = zeroMoveDirection match {
          case "left" => Left
          case "right" => Right
        }

        val oneMove = oneMoveDirection match {
          case "left" => Left
          case "right" => Right
        }

        thisState(0) -> State(Op(zeroWriteValue.toInt, zeroMove, zeroContinueState(0)), Op(oneWriteValue.toInt, oneMove, oneContinueState(0)))

      }.toOption

      parsedState match {

        case Some(newState) if remainingLines.isEmpty =>
          states updated (newState._1, newState._2)

        case Some(newState)  =>
          parseStates(remainingLines, states updated (newState._1, newState._2))

        case None =>
          states
      }

    }

    def parseStates(input: List[String]) : Option[Map[Char, State]] = {

      val states = parseStates(input, Map.empty)

      if(states.isEmpty) None
      else Some(states)

    }


    def fromInputStrings(input: List[String]) : Option[Program] = {

      // The first three lines are the header

      // By writing all our parse functions as returning option as well as interacting with the list using headOption
      // we are able to do the whole thing in a for comprehension ...

      val programOption = for (

        header1 <- input.headOption;
        startState <- parseStartState(header1);

        rest1 = input.tail;
        header2 <- rest1.headOption;
        checksumSteps <- parseChecksumSteps(header2);

        rest2 = rest1.tail;
        states <- parseStates(rest2)

      ) yield (startState, checksumSteps, states)

      programOption.map {
        program =>
          Program(program._1, program._2, program._3)

      }

    }
  }

  case class TuringMachine(currentSlot: Slot, currentState: Char, program: Program, steps : Int = 0) {

    override def toString: String = {

      val start = s"steps: $steps state: $currentState slots:"

      val end = currentSlot.getTapeString(currentSlot)

      start ++ end
    }

    // execute an operation

    def executeOp(op: Op) : TuringMachine = {

      //println(s"In state ${currentState}")

      currentSlot.value = op.writeValue

      op.move match {

        case Left =>

          currentSlot.left match {

            case Some(leftSlot) =>
              //println("Move left - existing slot")
              TuringMachine(currentSlot = leftSlot, op.continueState, program, steps + 1)

            case None =>
              //println("Move left - new slot")
              currentSlot.left = Some(Slot(0, None, Some(currentSlot)))
              TuringMachine(currentSlot = currentSlot.left.get, op.continueState, program, steps + 1)
          }

        case Right =>

          currentSlot.right match {

            case Some(rightSlot) =>
              //println("Move right - existing slot")
              TuringMachine(currentSlot = rightSlot, op.continueState, program, steps + 1)

            case None =>
              //println("Move right - new slot")
              currentSlot.right = Some(Slot(0, Some(currentSlot), None))
              TuringMachine(currentSlot.right.get, op.continueState, program, steps + 1)
          }

      }

    }

    // advance one step
    def step() : TuringMachine = {

      // This throws exception if we try to enter a non-existent state which is ok
      val state = program.states(currentState)

      // Execute the states op depending on if it is zero or one at current tape slot
      val currentValue = currentSlot.value

      if(currentValue == 0) executeOp(state.zeroOp)
      else executeOp(state.oneOp)

    }

    def runAllStepsGetCount() : Int = {

      def runStep(machine: TuringMachine) : TuringMachine = {

        //println(machine)

        if(machine.steps == program.checksumAtStep) machine
        else {
          val nextMachine = machine.step()
          runStep(nextMachine)
        }

      }

      val finalMachine = runStep(this)

      //finalMachine.currentSlot.printTape()

      finalMachine.currentSlot.count()
    }


  }

  // Sample program

  val sampleStates : Map[Char, State] = Map (
    'a' -> State(Op(1, Right, 'b'), Op(0, Left, 'b')),
    'b' -> State(Op(1, Left, 'a'), Op(1, Right, 'a')))

  val sample = Program(startState = 'a', checksumAtStep = 3, sampleStates)


  def main(args : Array[String]) : Unit = {

    val sample1 = TuringMachine(Slot(), 'a', Program('a', 6, sampleStates))

    val checkSumSample1 = sample1.runAllStepsGetCount()

    println(s"checksum sample1 = $checkSumSample1")

    val inputLines: List[String] = Source.fromResource("input25.txt").getLines.toList

    val step1 = TuringMachine.fromInputStrings(inputLines)

    val step1TuringMachine = TuringMachine(Slot(), step1.get.startState, step1.get)

    val step1Result = step1TuringMachine.runAllStepsGetCount()

    println(s"checksum step1 = $step1Result")

  }

}

/**
  *
  *
  * "Programs these days, don't know their origins. That's the Turing machine! It's what makes the whole computer work."
  * You try to explain that Turing machines are merely models of computation, but he cuts you off. "No, see, that's just
  * what they want you to think. Ultimately, inside every CPU, there's a Turing machine driving the whole thing! Too bad
  * this one's broken. We're doomed!"

You ask how you can help. "Well, unfortunately, the only way to get the computer running again would be to create a whole
new Turing machine from scratch, but there's no way you can-" He notices the look on your face, gives you a curious glance,
shrugs, and goes back to sweeping the floor.

You find the Turing machine blueprints (your puzzle input) on a tablet in a nearby pile of debris. Looking back up at the
broken Turing machine above, you can start to identify its parts:

A tape which contains 0 repeated infinitely to the left and right.
A cursor, which can move left or right along the tape and read or write values at its current position.
A set of states, each containing rules about what to do based on the current value under the cursor.
Each slot on the tape has two possible values: 0 (the starting value for all slots) and 1. Based on whether the cursor
is pointing at a 0 or a 1, the current state says what value to write at the current position of the cursor, whether to
move the cursor left or right one slot, and which state to use next.

For example, suppose you found the following blueprint:

Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
Running it until the number of steps required to take the listed diagnostic checksum would result in the following
tape configurations (with the cursor marked in square brackets):

... 0  0  0 [0] 0  0 ... (before any steps; about to run state A)
... 0  0  0  1 [0] 0 ... (after 1 step;     about to run state B)
... 0  0  0 [1] 1  0 ... (after 2 steps;    about to run state A)
... 0  0 [0] 0  1  0 ... (after 3 steps;    about to run state B)
... 0 [0] 1  0  1  0 ... (after 4 steps;    about to run state A)
... 0  1 [1] 0  1  0 ... (after 5 steps;    about to run state B)
... 0  1  1 [0] 1  0 ... (after 6 steps;    about to run state A)
The CPU can confirm that the Turing machine is working by taking a diagnostic checksum after a specific number of
steps (given in the blueprint). Once the specified number of steps have been executed, the Turing machine should pause;
once it does, count the number of times 1 appears on the tape. In the above example, the diagnostic checksum is 3.

Recreate the Turing machine and save the computer! What is the diagnostic checksum it produces once it's working again?


  */
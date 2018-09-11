import scala.io.Source
import scala.util.Try

object Day23 {

  trait Instruction {
    def op(m : Machine) : Machine
  }

  type Reg = Char

  case class Machine(regs : Map[Reg, Long] = Map.empty,
                     mulCount : Long = 0L,
                     nextInstructionOffset : Int = 1,
                     defaultAValue : Int = 0) {

    def get(reg: Reg): Long = {
      if(reg == 'a')
        regs.getOrElse(reg, defaultAValue)
      else
        regs.getOrElse(reg, 0)
    }

    def set(reg: Reg, value: Long) : Machine = {

      this.copy(regs = regs updated (reg, value))

    }

    def set(reg: Reg, reg2: Reg) : Machine = {

      val value = get(reg2)
      this.copy(regs = regs updated (reg, value))

    }

  }

  case class SetV(r: Reg, value: Long) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r, value)
    }
  }

  object SetV {
    def fromString(s: String) : Option[SetV] = {
      val m1 = "set ([a-z]) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        SetV(a.head, b.toInt)
      }.toOption
    }
  }

  case class SetR(r: Reg, value: Reg) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r, value)
    }
  }

  object SetR {
    def fromString(s: String) : Option[SetR] = {
      val m1 = "set ([a-z]) ([a-z])".r
      Try {
        val m1(a,b) = s
        SetR(a.head, b.head)
      }.toOption
    }
  }

  case class SubV(r: Reg, value: Long) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r, m.get(r) - value)
    }
  }

  object SubV {
    def fromString(s: String) : Option[SubV] = {
      val m1 = "sub ([a-z]) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        SubV(a.head, b.toInt)
      }.toOption
    }
  }

  case class SubR(r: Reg, r2: Reg) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r, m.get(r) - m.get(r2))
    }
  }

  object SubR {
    def fromString(s: String) : Option[SubR] = {
      val m1 = "sub ([a-z]) ([a-z])".r
      Try {
        val m1(a,b) = s
        SubR(a.head, b.head)
      }.toOption
    }
  }

  case class MulV(r1: Reg, value: Long) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r1, value * m.get(r1)).copy(mulCount = m.mulCount + 1)
    }
  }

  object MulV {
    def fromString(s: String) : Option[MulV] = {
      val m1 = "mul ([a-z]) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        MulV(a.head, b.toInt)
      }.toOption
    }
  }

  case class MulR(r1: Reg, r2: Reg) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r1, m.get(r1) * m.get(r2)).copy(mulCount = m.mulCount + 1)
    }
  }

  object MulR {
    def fromString(s: String) : Option[MulR] = {
      val m1 = "mul ([a-z]) ([a-z])".r
      Try {
        val m1(a,b) = s
        MulR(a.head, b.head)
      }.toOption
    }
  }

  case class JnzR(r1 : Reg, offsetR: Reg) extends Instruction {
    def op(m: Machine) : Machine = {

      if(m.get(r1) == 0)
        m.copy(nextInstructionOffset = 1)
      else
        m.copy(nextInstructionOffset = m.get(offsetR).toInt)
    }
  }

  object JnzR {
    def fromString(s: String) : Option[JnzR] = {
      val m1 = "jnz ([a-z]) ([a-z])".r
      Try {
        val m1(a,b) = s
        JnzR(a.head, b.head)
      }.toOption
    }
  }

  case class JnzVV(value : Long, offset: Long) extends Instruction {
    def op(m: Machine) : Machine = {

      if(value == 0)
        m.copy(nextInstructionOffset = 1)
      else
        m.copy(nextInstructionOffset = offset.toInt)
    }
  }

  object JnzVV {
    def fromString(s: String) : Option[JnzVV] = {
      val m1 = "jnz ([-]*[0-9]+) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        JnzVV(a.toInt, b.toInt)
      }.toOption
    }
  }

  case class JnzV(r1 : Reg, offset: Long) extends Instruction {
    def op(m: Machine) : Machine = {

      if(m.get(r1) == 0)
        m.copy(nextInstructionOffset = 1)
      else
        m.copy(nextInstructionOffset = offset.toInt)
    }
  }

  object JnzV {
    def fromString(s: String) : Option[JnzV] = {
      val m1 = "jnz ([a-z]) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        JnzV(a.head, b.toInt)
      }.toOption
    }
  }

  object Instruction {
    def fromString(s: String) : Instruction = {

      val instruction = List(
        MulR.fromString(s),
        MulV.fromString(s),
        SubR.fromString(s),
        SubV.fromString(s),
        SetR.fromString(s),
        SetV.fromString(s),
        JnzR.fromString(s),
        JnzV.fromString(s),
        JnzVV.fromString(s)
      ).flatten

      instruction.head
    }

  }

  def readInstructionsFromString(s: String) : Vector[Instruction] =
    s.lines.toVector.map{Instruction.fromString}

  def execute(machine: Machine, program: Vector[Instruction], pc: Int = -1) : Long = {

    val nextPC = pc + machine.nextInstructionOffset

    if(nextPC < program.size) {
      val nextInstruction = program(nextPC)

      println(s"execute $nextInstruction")
      println(s"pre machine $machine")

      val newMachine = nextInstruction.op(machine.copy(nextInstructionOffset = 1))

      println(s"postmachine $newMachine")

      execute(newMachine, program, nextPC)
    }
    else {
      machine.mulCount
    }

  }

  // Sieve of Eratosthenes to get the primes from 2 to n
  // Not actually needed for this puzzle but I mistakenly thought it was
  def genPrimes(n: Int) : Set[Int] = {

    val start = (2 to n).toSet

    (2 to n).foldLeft(start){
      case (remaining, num) =>
        // remove all numbers evenly divisible by num
        remaining.filter(p => p == num || (p % num != 0))
    }

  }


  def main(args: Array[String]) : Unit = {

    val step1Input = Source.fromResource("input23.txt").mkString

    val step1Program = readInstructionsFromString(step1Input)

    val s1 = execute(Machine(), step1Program)

    println(s"step1 mul was called $s1 times") // 4225

    // part two

    // Through rewriting the code in C and learning how it works I discovered that it is doing the following:
    // For all the numbers from 106700 to 123700 (in steps of 17) find the number that have factors (other than 2 and
    // themselves). The register h accumulates the sum of these.

    def hasFactors(n: Int) : Int = {

      (2 until n).foldLeft(0){
        case (acc, d) =>

          if(n % d == 0) 1
          else acc

      }

    }

    println(s"There are ${hasFactors(106700)} factors of 106700")

    val step2Answer = 106700.to(123700, 17).foldLeft(0) {
      case (acc, p) =>
        acc + hasFactors(p)

    }

    println(s"Answer 2 $step2Answer") // 905

  }

}

/*

--- Day 23: Coprocessor Conflagration ---
You decide to head directly to the CPU and fix the printer from there. As you get close, you find an experimental coprocessor
doing so much work that the local programs are afraid it will halt and catch fire. This would cause serious issues for the
 rest of the computer, so you head in and see what you can do.

The code it's running seems to be a variant of the kind you saw recently on that tablet. The general functionality seems
very similar, but some of the instructions are different:

set X Y sets register X to the value of Y.
sub X Y decreases register X by the value of Y.
mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next
instruction, an offset of -1 jumps to the previous instruction, and so on.)
Only the instructions listed above are used. The eight registers here, named a through h, all start at 0.

The coprocessor is currently set to some kind of debug mode, which allows for testing, but prevents it from doing any
meaningful work.

If you run the program (your puzzle input), how many times is the mul instruction invoked?

--- Part Two ---
Now, it's time to fix the problem.

The debug mode switch is wired directly to register a. You flip the switch, which makes register a now start at 1 when
the program is executed.

Immediately, the coprocessor begins to overheat. Whoever wrote this program obviously didn't choose a very efficient
implementation. You'll need to optimize the program if it has any hope of completing before Santa needs that printer working.


The coprocessor's ultimate goal is to determine the final value left in register h once the program completes.
 Technically, if it had that... it wouldn't even need to run the program.

After setting register a to 1, if the program were to run to completion, what value would be left in register h?

 */
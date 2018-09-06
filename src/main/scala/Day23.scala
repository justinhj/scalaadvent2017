import scala.io.Source
import scala.util.Try

object Day23 {

  trait Instruction {
    def op(m : Machine) : Machine
  }

  type Reg = Char

  case class Machine(regs : Map[Reg, Long] = Map.empty,
                     mulCount : Long = 0L,
                     nextInstructionOffset : Int = 1) {

    def get(reg: Reg): Long = regs.getOrElse(reg, 0)

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

  def main(args: Array[String]) : Unit = {

    val step1Input = Source.fromResource("input23.txt").mkString

    val step1Program = readInstructionsFromString(step1Input)

    val s1 = execute(Machine(), step1Program)

    println(s"step1 mul was called $s1 times")

  }

}

/*

--- Day 23: Coprocessor Conflagration ---
You decide to head directly to the CPU and fix the printer from there. As you get close, you find an experimental coprocessor doing so much work that the local programs are afraid it will halt and catch fire. This would cause serious issues for the rest of the computer, so you head in and see what you can do.

The code it's running seems to be a variant of the kind you saw recently on that tablet. The general functionality seems very similar, but some of the instructions are different:

set X Y sets register X to the value of Y.
sub X Y decreases register X by the value of Y.
mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
Only the instructions listed above are used. The eight registers here, named a through h, all start at 0.

The coprocessor is currently set to some kind of debug mode, which allows for testing, but prevents it from doing any meaningful work.

If you run the program (your puzzle input), how many times is the mul instruction invoked?
 */
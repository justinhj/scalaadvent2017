import scala.io.Source
import scala.util.Try

object Day18 {

  private val sampleInstructions = """set a 1
                             |add a 2
                             |mul a a
                             |mod a 5
                             |snd a
                             |set a 0
                             |rcv a
                             |jgz a -1
                             |set a 1
                             |jgz a -2""".stripMargin

  trait Instruction {
    def op(m : Machine) : Machine
  }

  type Reg = Char

  case class Machine(regs : Map[Reg, Int] = Map.empty,
                     lastSoundPlayed : Int = 0,
                     lastSoundRecovered : Int = 0,
                     nextInstructionOffset : Int = 1) {

    def get(reg: Reg): Int = regs.getOrElse(reg, 0)

    def set(reg: Reg, value: Int) : Machine = {

      this.copy(regs = regs updated (reg, value))

    }

    def set(reg: Reg, reg2: Reg) : Machine = {

      val value = get(reg2)
      this.copy(regs = regs updated (reg, value))

    }

  }

  case class Snd(r: Reg) extends Instruction {
    def op(m: Machine) : Machine = {
      m.copy(lastSoundPlayed = m.get(r))
    }
  }

  object Snd {
    def fromString(s: String) : Option[Snd] = {
      val m1 = "snd ([a-z])".r
      Try {
        val m1(a) = s
        Snd(a.head)
      }.toOption
    }
  }

  case class SetV(r: Reg, value: Int) extends Instruction {
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

  case class AddV(r: Reg, value: Int) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r, value + m.get(r))
    }
  }

  object AddV {
    def fromString(s: String) : Option[AddV] = {
      val m1 = "add ([a-z]) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        AddV(a.head, b.toInt)
      }.toOption
    }
  }

  case class AddR(r: Reg, r2: Reg) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r, m.get(r2) + m.get(r))
    }
  }

  object AddR {
    def fromString(s: String) : Option[AddR] = {
      val m1 = "add ([a-z]) ([a-z])".r
      Try {
        val m1(a,b) = s
        AddR(a.head, b.head)
      }.toOption
    }
  }

  case class MulV(r1: Reg, value: Int) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r1, value * m.get(r1))
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
      m.set(r1, m.get(r1) * m.get(r1))
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

  case class ModV(r1: Reg, value: Int) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r1, m.get(r1) % value)
    }
  }

  object ModV {
    def fromString(s: String) : Option[ModV] = {
      val m1 = "mod ([a-z]) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        ModV(a.head, b.toInt)
      }.toOption
    }
  }

  case class ModR(r1: Reg, r2: Reg) extends Instruction {
    def op(m: Machine) : Machine = {
      m.set(r1, m.get(r1) % m.get(r2))
    }
  }

  object ModR {
    def fromString(s: String) : Option[ModR] = {
      val m1 = "mod ([a-z]) ([a-z])".r
      Try {
        val m1(a,b) = s
        ModR(a.head, b.head)
      }.toOption
    }
  }

  case class Rcv(r1 : Reg) extends Instruction {
    def op(m: Machine) : Machine = {
      val ls = m.get(r1)

      if(ls != 0) {
        m.copy(lastSoundRecovered = m.lastSoundPlayed)
      } else
        m
    }
  }

  object Rcv {
    def fromString(s: String) : Option[Rcv] = {
      val m1 = "rcv ([a-z])".r
      Try {
        val m1(a) = s
        Rcv(a.head)
      }.toOption
    }
  }

  case class JgzR(r1 : Reg, offsetR: Reg) extends Instruction {
    def op(m: Machine) : Machine = {

      if(m.get(r1) == 0)
        m.copy(nextInstructionOffset = 1)
      else
        m.copy(nextInstructionOffset = m.get(offsetR))
    }
  }

  object JgzR {
    def fromString(s: String) : Option[JgzR] = {
      val m1 = "jgz ([a-z]) ([a-z])".r
      Try {
        val m1(a,b) = s
        JgzR(a.head, b.head)
      }.toOption
    }
  }

  case class JgzVV(value : Int, offset: Int) extends Instruction {
    def op(m: Machine) : Machine = {

      if(value == 0)
        m.copy(nextInstructionOffset = 1)
      else
        m.copy(nextInstructionOffset = offset)
    }
  }

  object JgzVV {
    def fromString(s: String) : Option[JgzVV] = {
      val m1 = "jgz ([-]*[0-9]+) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        JgzVV(a.toInt, b.toInt)
      }.toOption
    }
  }

  case class JgzV(r1 : Reg, offset: Int) extends Instruction {
    def op(m: Machine) : Machine = {

      if(m.get(r1) == 0)
        m.copy(nextInstructionOffset = 1)
      else
        m.copy(nextInstructionOffset = offset)
    }
  }

  object JgzV {
    def fromString(s: String) : Option[JgzV] = {
      val m1 = "jgz ([a-z]) ([-]*[0-9]+)".r
      Try {
        val m1(a,b) = s
        JgzV(a.head, b.toInt)
      }.toOption
    }
  }

  object Instruction {
    def fromString(s: String) : Instruction = {

      val instruction = List(Snd.fromString(s),
        MulR.fromString(s),
        MulV.fromString(s),
        AddR.fromString(s),
        AddV.fromString(s),
        SetR.fromString(s),
        SetV.fromString(s),
        ModR.fromString(s),
        ModV.fromString(s),
        Rcv.fromString(s),
        JgzR.fromString(s),
        JgzV.fromString(s),
        JgzVV.fromString(s)
      ).flatten

      instruction.head
    }

  }

  def readInstructionsFromString(s: String) : Vector[Instruction] =
    s.lines.toVector.map{Instruction.fromString}

  def execute(machine: Machine, program: Vector[Instruction], pc: Int = -1) : Int = {

    val nextPC = pc + machine.nextInstructionOffset

    val nextInstruction = program(nextPC)

    println(s"execute $nextInstruction")
    println(s"pre machine $machine")

    val newMachine = nextInstruction.op(machine.copy(nextInstructionOffset = 1))

    println(s"postmachine $newMachine")

    if(newMachine.lastSoundRecovered != 0) {
      newMachine.lastSoundRecovered
    }
    else {
      execute(newMachine, program, nextPC)
    }

  }

  def main(args: Array[String]) : Unit = {

    println(s"snd ${Snd.fromString("snd a")}")
    println(s"set ${SetV.fromString("set c -12")}")
    println(s"set ${SetV.fromString("set z 42")}")

    val sampleProgram = readInstructionsFromString(sampleInstructions)

    // Now to execute the program until the first time a rcv
    // instruction is executed with a non-zero value

    val f = execute(Machine(), sampleProgram)

    println(s"sample freq = $f")

    val step1Input = Source.fromResource("input18.txt").mkString

    val step1Program = readInstructionsFromString(step1Input)

    val s1 = execute(Machine(), step1Program)

    println(s"step1 freq = $s1")


  }




}

/*

--- Day 18: Duet ---
You discover a tablet containing some strange assembly code labeled simply "Duet". Rather than bother the sound card
with it, you decide to run the code yourself. Unfortunately, you don't see any documentation, so you're left to figure
out what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that are each named with a single letter and that
can each hold a single integer. You suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out what they do. Here's what you determine:

snd X plays a sound with a frequency equal to the value of X.
set X Y sets register X to the value of Y.
add X Y increases register X by the value of Y.
mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it
 sets X to the result of X modulo Y).
rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the
command does nothing.)
jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips
the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
Many of the instructions can take either a register (a single letter) or a number. The value of a register is the
 integer it contains; the value of a number is that number.

After each jump instruction, the program continues with the instruction to which the jump jumped. After any other
instruction, the program continues with the next instruction. Continuing (or jumping) off either end of the program
terminates it.

For example:

set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2
The first four instructions set a to 1, add 2 to it, square it, and then set it to itself modulo 5, resulting in a
 value of 4.
Then, a sound with frequency 4 (the value of a) is played.
After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be skipped (rcv because a is 0,
 and jgz because a is not greater than 0).
Finally, a is set to 1, causing the next jgz instruction to activate, jumping back two instructions to another jump,
which jumps again to the rcv, which ultimately triggers the recover operation.
At the time the recover operation is executed, the frequency of the last sound played is 4.

What is the value of the recovered frequency (the value of the most recently played sound) the first time a rcv
instruction is executed with a non-zero value?



 */
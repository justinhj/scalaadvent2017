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

  val primes = Set(
    106703, 106721, 106727, 106739, 106747, 106753, 106759, 106781, 106783,
    106787, 106801, 106823, 106853, 106859, 106867, 106871, 106877, 106903,
    106907, 106921, 106937, 106949, 106957, 106963, 106979, 106993, 107021,
    107033, 107053, 107057, 107069, 107071, 107089, 107099, 107101, 107119,
    107123, 107137, 107171, 107183, 107197, 107209, 107227, 107243, 107251,
    107269, 107273, 107279, 107309, 107323, 107347, 107351, 107357, 107377,
    107441, 107449, 107453, 107467, 107473, 107509, 107563, 107581, 107599,
    107603, 107609, 107621, 107641, 107647, 107687, 107693, 107699, 107713,
    107717, 107719, 107741, 107747, 107761, 107777, 107791, 107827, 107837,
    107839, 107843, 107857, 107867, 107873, 107897, 107903, 107923, 107927,
    107941, 107951, 107971, 107981, 107999, 108011, 108013, 108023, 108037,
    108041, 108061, 108079, 108089, 108107, 108127, 108131, 108139, 108161,
    108179, 108187, 108191, 108193, 108203, 108217, 108223, 108233, 108247,
    108263, 108271, 108287, 108289, 108293, 108343, 108347, 108359, 108377,
    108379, 108401, 108413, 108421, 108439, 108461, 108463, 108497, 108499,
    108503, 108517, 108529, 108533, 108541, 108557, 108571, 108587, 108631,
    108637, 108643, 108649, 108677, 108707, 108727, 108739, 108751, 108761,
    108769, 108791, 108793, 108799, 108803, 108827, 108863, 108869, 108877,
    108881, 108883, 108887, 108893, 108907, 108923, 108929, 108943, 108947,
    108949, 108959, 108961, 108967, 108971, 108001, 109013, 109037, 109049,
    109063, 109073, 109097, 109103, 109111, 109133, 109139, 109141, 109147,
    109159, 109169, 109171, 109199, 109201, 109229, 109253, 109267, 109279,
    109297, 109303, 109313, 109321, 109331, 109363, 109367, 109379, 109387,
    109391, 109397, 109423, 109433, 109441, 109453, 109469, 109471, 109481,
    109507, 109517, 109519, 109537, 109541, 109567, 109579, 109583, 109589,
    109597, 109609, 109619, 109621, 109639, 109663, 109673, 109717, 109721,
    109741, 109751, 109789, 109793, 109807, 109829, 109831, 109841, 109843,
    109847, 109849, 109859, 109873, 109883, 109897, 109903, 109913, 109919,
    109937, 109943, 109961, 109987, 110017, 110039, 110051, 110059, 110063,
    110069, 110083, 110119, 110129, 110161, 110221, 110233, 110237, 110251,
    110261, 110269, 110273, 110281, 110291, 110321, 110323, 110339, 110359,
    110419, 110431, 110437, 110441, 110459, 110479, 110491, 110501, 110503,
    110527, 110533, 110543, 110557, 110563, 110569, 110573, 110581, 110587,
    110597, 110603, 110609, 110623, 110629, 110647, 110651, 110681, 110711,
    110729, 110731, 110749, 110753, 110771, 110807, 110813, 110819, 110821,
    110849, 110863, 110879, 110881, 110899, 110917, 110921, 110923, 110927,
    110933, 110939, 110947, 110951, 110969, 110989, 111029, 111031, 111043,
    111049, 111053, 111091, 111103, 111109, 111121, 111127, 111143, 111149,
    111187, 111191, 111211, 111217, 111227, 111253, 111263, 111269, 111271,
    111301, 111317, 111323, 111337, 111341, 111373, 111409, 111427, 111431,
    111439, 111443, 111467, 111487, 111491, 111497, 111509, 111521, 111533,
    111539, 111577, 111581, 111593, 111599, 111623, 111637, 111641, 111653,
    111659, 111667, 111697, 111721, 111731, 111751, 111767, 111773, 111779,
    111781, 111791, 111799, 111821, 111827, 111833, 111847, 111857, 111863,
    111869, 111871, 111893, 111913, 111919, 111953, 111959, 111973, 111977,
    111997, 112019, 112031, 112061, 112067, 112087, 112097, 112103, 112111,
    112121, 112129, 112139, 112153, 112163, 112199, 112207, 112213, 112223,
    112237, 112241, 112247, 112249, 112253, 112279, 112289, 112291, 112297,
    112303, 112327, 112331, 112337, 112339, 112361, 112363, 112397, 112403,
    112429, 112459, 112481, 112501, 112507, 112559, 112571, 112573, 112577,
    112583, 112589, 112601, 112603, 112621, 112657, 112663, 112687, 112691,
    112741, 112757, 112759, 112771, 112787, 112807, 112831, 112843, 112859,
    112877, 112901, 112909, 112913, 112919, 112927, 112939, 112951, 112967,
    112979, 112997, 113011, 113017, 113021, 113027, 113039, 113041, 113051,
    113063, 113081, 113083, 113089, 113093, 113117, 113123, 113131, 113143,
    113147, 113149, 113153, 113159, 113161, 113171, 113173, 113177, 113189,
    113209, 113213, 113227, 113233, 113279, 113327, 113329, 113341, 113357,
    113359, 113363, 113371, 113381, 113383, 113437, 113453, 113467, 113489,
    113497, 113501, 113513, 113537, 113539, 113567, 113591, 113621, 113623,
    113647, 113657, 113683, 113717, 113719, 113731, 113749, 113759, 113761,
    113777, 113779, 113783, 113797, 113809, 113837, 113843, 113891, 113899,
    113903, 113909, 113921, 113933, 113947, 113963, 113969, 113983, 113989,
    114001, 114013, 114031, 114041, 114043, 114073, 114077, 114083, 114089,
    114113, 114143, 114157, 114161, 114167, 114197, 114199, 114203, 114217,
    114221, 114229, 114259, 114269, 114277, 114299, 114311, 114319, 114329,
    114343, 114371, 114377, 114407, 114419, 114467, 114473, 114479, 114487,
    114493, 114547, 114553, 114571, 114577, 114599, 114601, 114613, 114617,
    114641, 114643, 114649, 114659, 114661, 114679, 114689, 114691, 114713,
    114743, 114749, 114757, 114761, 114769, 114781, 114797, 114799, 114809,
    114827, 114833, 114847, 114859, 114883, 114901, 114913, 114941, 114967,
    114973, 114997, 115001, 115013, 115019, 115057, 115061, 115067, 115079,
    115099, 115117, 115123, 115127, 115133, 115153, 115163, 115183, 115201,
    115211, 115223, 115237, 115249, 115259, 115301, 115303, 115309, 115319,
    115321, 115327, 115331, 115337, 115343, 115363, 115399, 115421, 115429,
    115459, 115469, 115471, 115499, 115513, 115547, 115553, 115561, 115571,
    115589, 115597, 115601, 115603, 115613, 115637, 115657, 115663, 115679,
    115693, 115727, 115733, 115741, 115751, 115763, 115769, 115771, 115777,
    115781, 115783, 115793, 115807, 115811, 115831, 115837, 115849, 115853,
    115859, 115861, 115873, 115877, 115879, 115891, 115901, 115903, 115931,
    115933, 115963, 115979, 115981, 115987, 116027, 116041, 116047, 116089,
    116099, 116101, 116107, 116113, 116131, 116159, 116167, 116177, 116189,
    116191, 116201, 116239, 116243, 116257, 116273, 116279, 116293, 116329,
    116341, 116351, 116359, 116371, 116381, 116411, 116423, 116437, 116443,
    116447, 116461, 116471, 116483, 116491, 116531, 116533, 116537, 116539,
    116549, 116579, 116593, 116639, 116657, 116681, 116687, 116689, 116707,
    116719, 116731, 116741, 116747, 116789, 116797, 116803, 116819, 116827,
    116833, 116849, 116867, 116881, 116903, 116923, 116927, 116929, 116933,
    116953, 116959, 116969, 116981, 116989, 116017, 117023, 117037, 117041,
    117043, 117053, 117071, 117101, 117109, 117127, 117133, 117163, 117167,
    117191, 117193, 117203, 117209, 117223, 117241, 117251, 117259, 117269,
    117281, 117307, 117319, 117329, 117331, 117361, 117371, 117373, 117389,
    117413, 117427, 117431, 117437, 117443, 117499, 117503, 117511, 117517,
    117529, 117539, 117541, 117563, 117571, 117617, 117619, 117643, 117659,
    117671, 117673, 117679, 117701, 117703, 117721, 117727, 117731, 117751,
    117757, 117763, 117773, 117779, 117787, 117809, 117811, 117833, 117839,
    117841, 117851, 117877, 117881, 117883, 117899, 117911, 117917, 117937,
    117959, 117973, 117977, 117979, 117989, 117033, 118037, 118043, 118051,
    118057, 118061, 118081, 118093, 118127, 118163, 118169, 118171, 118189,
    118211, 118213, 118219, 118247, 118249, 118259, 118273, 118277, 118297,
    118343, 118361, 118369, 118373, 118387, 118409, 118411, 118423, 118429,
    118453, 118457, 118463, 118471, 118493, 118543, 118549, 118571, 118583,
    118589, 118603, 118619, 118621, 118633, 118669, 118673, 118681, 118687,
    118691, 118709, 118717, 118739, 118747, 118757, 118787, 118799, 118801,
    118819, 118831, 118843, 118861, 118873, 118897, 118901, 118903, 118907,
    118913, 118927, 118931, 118967, 118973, 119033, 119039, 119047, 119057,
    119069, 119083, 119087, 119089, 119099, 119107, 119129, 119131, 119159,
    119173, 119179, 119183, 119191, 119227, 119237, 119243, 119267, 119291,
    119293, 119297, 119299, 119311, 119321, 119363, 119389, 119417, 119419,
    119429, 119447, 119489, 119503, 119513, 119549, 119551, 119557, 119563,
    119569, 119591, 119611, 119617, 119627, 119653, 119657, 119659, 119671,
    119677, 119687, 119689, 119699, 119701, 119737, 119747, 119759, 119771,
    119773, 119783, 119797, 119809, 119813, 119831, 119839, 119849, 119851,
    119869, 119881, 119891, 119921, 119923, 119953, 119963, 119971, 119981,
    119983, 119993, 120011, 120017, 120041, 120049, 120067, 120077, 120079,
    120091, 120097, 120103, 120121, 120157, 120167, 120181, 120193, 120199,
    120209, 120223, 120233, 120247, 120277, 120293, 120299, 120319, 120331,
    120349, 120371, 120383, 120391, 120397, 120413, 120427, 120431, 120473,
    120503, 120511, 120539, 120551, 120557, 120569, 120577, 120587, 120607,
    120619, 120623, 120641, 120647, 120661, 120677, 120689, 120691, 120709,
    120713, 120721, 120737, 120739, 120749, 120767, 120779, 120811, 120817,
    120823, 120829, 120833, 120847, 120851, 120871, 120877, 120889, 120899,
    120907, 120917, 120919, 120929, 120937, 120943, 120947, 120977, 120997,
    121001, 121007, 121013, 121019, 121021, 121061, 121063, 121067, 121081,
    121123, 121139, 121151, 121157, 121169, 121181, 121189, 121229, 121259,
    121267, 121271, 121283, 121291, 121309, 121321, 121327, 121333, 121343,
    121349, 121351, 121357, 121367, 121369, 121403, 121421, 121439, 121441,
    121447, 121453, 121469, 121487, 121493, 121507, 121523, 121531, 121547,
    121553, 121559, 121571, 121577, 121579, 121607, 121609, 121621, 121631,
    121633, 121637, 121661, 121687, 121697, 121721, 121727, 121763, 121787,
    121789, 121843, 121853, 121867, 121883, 121909, 121921, 121931, 121937,
    121949, 121951, 121963, 121967, 121993, 121011, 122021, 122027, 122029,
    122033, 122039, 122041, 122051, 122053, 122081, 122099, 122117, 122131,
    122147, 122149, 122167, 122173, 122201, 122207, 122209, 122219, 122231,
    122251, 122263, 122267, 122273, 122279, 122321, 122323, 122327, 122347,
    122363, 122387, 122389, 122393, 122399, 122443, 122449, 122453, 122471,
    122477, 122489, 122497, 122501, 122503, 122527, 122533, 122557, 122561,
    122579, 122597, 122599, 122609, 122611, 122653, 122663, 122693, 122701,
    122719, 122741, 122743, 122753, 122761, 122789, 122819, 122827, 122833,
    122839, 122849, 122861, 122867, 122869, 122891, 122921, 122929, 122939,
    122953, 122957, 122963, 122971, 123001, 123017, 123031, 123049, 123059,
    123077, 123083, 123091, 123113, 123121, 123143, 123169, 123191, 123203,
    123209, 123217, 123229, 123239, 123259, 123289, 123307, 123311, 123323,
    123341, 123373, 123377, 123379, 123397, 123407, 123419, 123427, 123433,
    123439, 123449, 123457, 123479, 123491, 123499, 123503, 123517, 123527,
    123547, 123551, 123553, 123581, 123583, 123601, 123619, 123631, 123637,
    123653, 123661, 123667, 123677)


  def step2(): Int = {

    (106700 to 123700).foldLeft(0){
      case (acc, num) =>

        if(num % 17 == 0) {

          if(primes.contains(num)) {
            println(s"$num is prime")
            acc + 1
          }
          else acc

        } else acc

    }


  }


  def main(args: Array[String]) : Unit = {

    val step1Input = Source.fromResource("input23.txt").mkString

    val step1Program = readInstructionsFromString(step1Input)

    //val s1 = execute(Machine(), step1Program)

    //println(s"step1 mul was called $s1 times") // 4225

    // part two

    //val s2 = execute(Machine(defaultAValue = 0), step1Program)

    println(s"step2 ${step2()}")


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
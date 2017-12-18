import Day8.TestType.TestType


object Day8 {

  object TestType extends Enumeration {
    type TestType = Value
    val GreaterThan, GreaterThanOrEqual, LessThan, LessThanOrEqual, NotEqual, Equal = Value

    def fromString(s: String) : Option[TestType] = {

      s match {
        case ">" => Some(GreaterThan)
        case "<" => Some(LessThan)
        case ">=" => Some(GreaterThanOrEqual)
        case "<=" => Some(LessThanOrEqual)
        case "==" => Some(Equal)
        case "!=" => Some(NotEqual)
        case _ => None
      }
    }
  }

  object Instruction {
    def fromInput(register: String, isIncrement : String, amount: String, testRegister : String, testType: String, testAmount: String) = {

      Instruction(
        register = register,
        isIncrement = if(isIncrement == "inc") true else false,
        amount = amount.toInt,
        testRegister = testRegister,
        testType = TestType.fromString(testType).get,
        testAmount = testAmount.toInt
      )
    }
  }

  case class Instruction(register: String, isIncrement : Boolean, amount: Int, testRegister: String, testType: TestType, testAmount: Int)

  def parseInstruction(input: String) : Instruction = {

    val pattern = """([a-z]+) (inc|dec) ([-]*\d+) if ([a-z]+) (>|<|>=|<=|!=|==) ([-]*\d+)""".r

    val pattern(register, isIncrement, amount, testRegister, testType, testAmount) = input

    Instruction.fromInput(register, isIncrement, amount, testRegister, testType, testAmount)
  }

  def parseInput(input: List[String]) : List[Instruction] = {
    input.map{parseInstruction}
  }

  import TestType._

  def eval(instructions: List[Instruction]) : Map[String, Int] = {

    instructions.foldLeft(Map.empty[String, Int]) {
      case (regs, ins) =>

        val targetRegisterValue = regs.getOrElse(ins.register, 0)
        val compareRegisterValue = regs.getOrElse(ins.testRegister, 0)

        val conditionPassed = ins.testType match {
          case GreaterThan =>
            if(compareRegisterValue > ins.testAmount) true else false
          case GreaterThanOrEqual =>
            if(compareRegisterValue >= ins.testAmount) true else false
          case LessThan =>
            if(compareRegisterValue < ins.testAmount) true else false
          case LessThanOrEqual =>
            if(compareRegisterValue <= ins.testAmount) true else false
          case Equal =>
            if(compareRegisterValue == ins.testAmount) true else false
          case NotEqual =>
            if(compareRegisterValue != ins.testAmount) true else false
        }

        if(conditionPassed) {
          if(ins.isIncrement)
            regs updated (ins.register, targetRegisterValue + ins.amount)
          else
            regs updated (ins.register, targetRegisterValue - ins.amount)
        }
        else
          regs

    }

  }

  def largestValue(regs : Map[String, Int]) =
    regs.values.max

  // Copy of eval that tracks the max register value

  def evalTrackMax(instructions: List[Instruction]) : (Int, Map[String, Int]) = {

    instructions.foldLeft((0, Map.empty[String, Int])) {
      case ((max, regs), ins) =>

        val targetRegisterValue = regs.getOrElse(ins.register, 0)
        val compareRegisterValue = regs.getOrElse(ins.testRegister, 0)

        val conditionPassed = ins.testType match {
          case GreaterThan =>
            if(compareRegisterValue > ins.testAmount) true else false
          case GreaterThanOrEqual =>
            if(compareRegisterValue >= ins.testAmount) true else false
          case LessThan =>
            if(compareRegisterValue < ins.testAmount) true else false
          case LessThanOrEqual =>
            if(compareRegisterValue <= ins.testAmount) true else false
          case Equal =>
            if(compareRegisterValue == ins.testAmount) true else false
          case NotEqual =>
            if(compareRegisterValue != ins.testAmount) true else false
        }

        if(conditionPassed) {
          if(ins.isIncrement) {
            val newValue = targetRegisterValue + ins.amount
            val newMax = Math.max(max, newValue)
            (newMax, regs updated(ins.register, newValue))
          }
          else {
            val newValue = targetRegisterValue - ins.amount
            val newMax = Math.max(max, newValue)
            (newMax, regs updated(ins.register, newValue))
          }
        }
        else
          (max, regs)

    }

  }

  def main(args: Array[String]) : Unit = {

    val testInput = """b inc 5 if a > 1
                      |a inc 1 if b < 5
                      |c dec -10 if a >= 1
                      |c inc -20 if c == 10""".stripMargin


    val parsedInput = parseInput(testInput.lines.toList)

    val evaluatedInput = eval(parsedInput)

    val exampleMax = largestValue(evaluatedInput)
    val exampleMax2 = evalTrackMax(parsedInput)._1

    println(s"Largest reg in example $exampleMax")
    println(s"Largest intermediate reg in example $exampleMax2")

    val parsedInputStep1 = parseInput(input.lines.toList)

    val evaluatedInputStep1 = eval(parsedInputStep1)

    val maxStep1 = largestValue(evaluatedInputStep1)
    val maxStep2 = evalTrackMax(parsedInputStep1)._1

    println(s"Largest reg in step 1 $maxStep1")
    println(s"Largest intermediate reg in step 2 $maxStep2")

  }


  val input = """yxr inc 119 if nev != 6
                |piw dec -346 if tl != 4
                |cli inc 165 if nev >= -5
                |nev dec 283 if xuu > -2
                |tem inc 745 if qym >= -9
                |xuu dec -104 if cli == 165
                |h dec 192 if u <= 5
                |ln dec -616 if ej > -7
                |tem dec -555 if ar > -5
                |tem dec 687 if tl > -8
                |h inc 120 if re < -8
                |bq dec -410 if ej > -4
                |re dec 476 if tem == 613
                |ej dec 686 if cli != 163
                |ar dec 676 if tl > -6
                |nfo inc 633 if s == 0
                |tl inc -471 if tem > 607
                |bb inc 157 if piw > 342
                |cli inc -830 if piw != 342
                |piw inc -645 if ar >= -674
                |bq dec 304 if piw != 356
                |u inc -274 if bb != 147
                |yxr inc -520 if ec >= 1
                |ec dec 631 if bb > 147
                |ar dec 732 if s >= 0
                |foz inc -617 if bb == 157
                |qym dec -197 if ej == -686
                |bb dec 111 if h <= -199
                |ln dec -585 if re != -476
                |foz dec -181 if nev < -280
                |foz dec 989 if xuu != 106
                |ec inc -930 if ec <= -626
                |foz dec 862 if s >= 5
                |tem inc 241 if xuu == 104
                |ar dec -460 if h >= -201
                |cli inc -317 if cli == -665
                |bb dec -483 if h > -194
                |tem inc -718 if tl <= -466
                |ln dec 362 if hb == 0
                |qym inc 95 if hb > 3
                |piw dec -463 if ln < 246
                |xuu inc 159 if s <= 5
                |ec inc -710 if cli < -977
                |bb inc -98 if xuu >= 260
                |qym inc 159 if bb >= 540
                |acc inc 610 if fe <= 9
                |ar dec 381 if ej < -695
                |acc dec 807 if nev > -284
                |foz dec 926 if h < -188
                |tl inc 279 if ar != -957
                |ec dec -39 if acc < -191
                |u dec -110 if piw == 346
                |ej dec 980 if ar == -948
                |ln dec -165 if hb < 2
                |nfo inc -370 if tem > 128
                |ar dec -886 if ar < -947
                |bb dec 624 if ej == -1666
                |tl inc 36 if ec > -2227
                |yxr dec 320 if cli <= -976
                |tem inc -808 if ar >= -52
                |cli dec 213 if h >= -198
                |ej inc -141 if nev < -281
                |bq inc -736 if yxr > -211
                |u dec -742 if nfo > 258
                |bq dec -727 if ar >= -64
                |u dec -248 if bq > 89
                |xuu inc 431 if yxr > -203
                |hb dec -407 if hb >= -7
                |ar inc 789 if acc >= -197
                |yxr inc -753 if piw <= 350
                |piw dec -973 if yxr >= -956
                |xuu dec 457 if ec <= -2228
                |bq inc -201 if bq >= 97
                |bq dec -923 if ar == 727
                |cli inc -560 if piw < 1329
                |ej dec -150 if ar != 733
                |h inc -198 if nev == -283
                |u inc -388 if piw == 1319
                |hb inc -212 if s < 8
                |xuu inc -988 if ej < -1653
                |ec inc 58 if re >= -485
                |hb inc -126 if xuu < -741
                |ar inc 534 if nev == -273
                |fe inc 668 if s <= 7
                |tem dec 25 if h >= -390
                |u inc -632 if tl > -198
                |u dec 721 if bq > 813
                |piw dec -299 if foz >= -2348
                |hb inc -407 if ej > -1663
                |h dec 563 if foz == -2351
                |bb dec 362 if fe != 670
                |nev inc -512 if bb != -437
                |fe dec -419 if yxr < -948
                |tl dec 576 if ej > -1665
                |acc dec -510 if xuu > -759
                |s dec -108 if u < -906
                |xuu dec -407 if xuu < -748
                |yxr dec 771 if bb > -454
                |u dec -879 if tem != 110
                |bq inc -759 if u < -27
                |s dec -295 if qym < 364
                |qym inc 807 if xuu == -344
                |h inc 545 if hb == -338
                |ej inc -896 if s != 394
                |s dec -961 if yxr > -1723
                |s dec -142 if nev != -799
                |s inc -692 if ln != 427
                |ar dec 152 if tl == -763
                |acc dec 304 if piw != 1328
                |ar inc -532 if piw != 1324
                |bq inc 124 if tem < 109
                |h inc 480 if nfo == 268
                |xuu dec -291 if ej >= -2556
                |hb inc -56 if nfo >= 257
                |ar dec -656 if bb > -447
                |tl dec -458 if ec <= -2167
                |xuu inc -100 if hb <= -397
                |nev dec 384 if foz <= -2344
                |acc dec -779 if acc == 9
                |u inc 949 if h != -416
                |xuu inc -865 if nev < -1182
                |nev dec 317 if bb < -434
                |foz inc -786 if bq > 51
                |xuu inc -901 if ej >= -2557
                |nev dec 750 if h != -398
                |qym dec 783 if re != -480
                |bb dec 54 if cli < -1752
                |tl dec -803 if yxr < -1724
                |s inc 690 if xuu != -954
                |piw inc -667 if s != -151
                |foz dec -946 if cli <= -1757
                |h dec -885 if re == -486
                |ar inc -765 if re < -468
                |h dec 340 if ec != -2170
                |fe dec 440 if tl > 492
                |re dec 398 if hb != -384
                |xuu inc -49 if fe > 641
                |s dec 623 if qym != 375
                |piw inc 429 if ec == -2174
                |ej inc 747 if hb != -394
                |xuu inc 761 if nfo > 255
                |re inc 519 if yxr != -1716
                |nev inc 863 if tl <= 499
                |qym inc -357 if foz > -3138
                |tl dec 186 if u < 915
                |s dec -442 if nev < -1373
                |bq inc -999 if ec == -2174
                |cli inc 735 if acc > 781
                |ar dec -983 if nev >= -1389
                |cli inc 285 if u < 917
                |xuu inc -970 if h >= -747
                |bq inc 716 if tl < 310
                |xuu dec 984 if bb > -503
                |h inc -881 if ej >= -2546
                |u inc 719 if fe >= 646
                |u inc 396 if hb > -393
                |ej inc 734 if acc != 784
                |fe inc 581 if re <= -350
                |piw dec -69 if hb < -384
                |ec dec 540 if acc < 798
                |cli dec -615 if foz <= -3130
                |ln inc 261 if foz <= -3130
                |acc dec 739 if ar > 1065
                |ln dec -520 if qym > 19
                |re inc -260 if bq != -230
                |ln inc -28 if h > -750
                |hb dec -794 if s == -325
                |nfo inc 79 if ln < 1176
                |acc inc 173 if ec == -2714
                |yxr inc 756 if re > -616
                |re inc 415 if fe >= 1219
                |hb dec -701 if u < 1638
                |xuu dec -325 if re != -200
                |ln dec -552 if piw != 1157
                |yxr inc -568 if qym >= 17
                |tl dec 756 if ec <= -2712
                |nev dec 430 if xuu <= -1219
                |bq dec -580 if yxr >= -1533
                |ej dec -357 if bb >= -493
                |ec inc 722 if ej >= -1822
                |tl dec 168 if nev == -1813
                |bb dec -393 if foz >= -3130
                |cli inc -298 if s == -328
                |h inc 885 if yxr != -1536
                |nev dec -188 if cli < -427
                |yxr dec 41 if cli > -416
                |qym inc 748 if u == 1632
                |re inc 457 if nfo > 339
                |yxr inc 781 if piw < 1153
                |qym inc 628 if cli != -417
                |bq inc -476 if piw < 1142
                |bb dec 547 if nfo >= 341
                |s dec 590 if piw == 1150
                |piw inc -804 if nfo != 342
                |ar dec -838 if xuu == -1226
                |ec inc 224 if h > 132
                |ec dec -963 if ln >= 1731
                |xuu inc 109 if u <= 1638
                |s dec -191 if fe >= 1220
                |ln dec -718 if h >= 133
                |u dec 46 if nev >= -1811
                |nfo inc -56 if s < -718
                |nev dec -346 if ar != 1905
                |h dec -501 if hb != 306
                |acc dec 651 if bb >= -1039
                |xuu dec 714 if ar > 1905
                |qym dec 304 if piw <= 1157
                |tl dec -62 if foz < -3138
                |h inc 448 if qym != 1095
                |hb inc 780 if tem != 112
                |hb dec 332 if tem <= 114
                |s dec -447 if acc != 226
                |s dec -560 if ej > -1829
                |nev dec -435 if piw == 1150
                |nev dec 259 if piw < 1155
                |tem inc 293 if nfo == 286
                |hb dec 656 if s < 284
                |hb inc -484 if acc < 213
                |ln inc 831 if s < 280
                |hb dec -394 if ec != -1771
                |nfo inc 124 if h != 628
                |ln dec -675 if yxr < -754
                |bb dec -69 if hb == 494
                |ln inc -139 if re <= 261
                |nev inc 325 if yxr >= -753
                |tl inc 551 if foz != -3137
                |bb dec -598 if nfo != 413
                |qym inc 771 if s >= 271
                |tl inc 903 if ej != -1820
                |fe inc -388 if re != 250
                |ln dec -163 if ej != -1827
                |nev dec -209 if re <= 261
                |bq inc -59 if h == 638
                |ej inc 793 if yxr < -749
                |ec inc -224 if s >= 276
                |ln inc 895 if foz <= -3132
                |acc dec -681 if ec < -1989
                |tem dec -161 if hb == 493
                |ar dec -436 if nfo >= 408
                |u dec -56 if re != 250
                |nfo inc -497 if hb < 495
                |ec dec -874 if acc > 911
                |bb dec 988 if xuu == -1838
                |nfo dec -257 if fe > 836
                |ec inc -332 if foz >= -3146
                |u inc 697 if u <= 1691
                |ej dec 777 if bb <= -440
                |nfo dec -99 if bq > -277
                |acc dec 552 if tem <= 573
                |fe dec 74 if ec == -2317
                |qym dec 829 if foz >= -3146
                |bq dec -149 if ec <= -2324
                |u dec -99 if hb == 493
                |yxr dec -546 if tl > 288
                |u inc 261 if cli >= -412
                |ej inc -341 if tem < 574
                |ec inc -83 if ar <= 2346
                |ec inc 847 if xuu <= -1830
                |re dec -364 if xuu < -1839
                |yxr inc -483 if ln <= 4033
                |tl dec 965 if tl == 286
                |re inc -63 if tl >= -674
                |cli inc 708 if acc < 352
                |xuu dec 494 if ej > -2154
                |tl inc 8 if foz > -3131
                |ec inc 983 if nfo == 170
                |bb inc 486 if cli < 292
                |ar dec -461 if ec >= -586
                |yxr dec -852 if hb < 500
                |u inc -874 if acc < 355
                |xuu inc 883 if re > 266
                |xuu dec 809 if ec == -577
                |tl dec 144 if ej > -2145
                |u inc -363 if fe >= 839
                |bb inc -194 if piw >= 1150
                |ln inc 780 if fe != 835
                |cli inc 213 if re >= 256
                |xuu dec -718 if hb >= 491
                |yxr dec -730 if u < 1251
                |nfo dec 1 if hb > 491
                |ar inc -556 if nev > -1085
                |ej dec -275 if bq < -128
                |nev dec -578 if foz != -3137
                |h inc -391 if h == 638
                |foz inc -291 if bq >= -134
                |bb inc -123 if fe == 840
                |tl dec 72 if bq == -133
                |qym dec -155 if yxr < 831
                |re inc 515 if s < 289
                |nfo dec -313 if ar >= 2242
                |cli dec 486 if tl < -904
                |bb dec -844 if ln >= 4810
                |hb inc -551 if tem == 565
                |ec inc -28 if yxr <= 828
                |cli inc -673 if h >= 241
                |re dec -937 if bb > 562
                |ec inc 214 if bb != 571
                |piw inc -142 if ar > 2240
                |ar dec 750 if tl < -898
                |re dec -844 if ej != -1877
                |bq inc -17 if qym <= 1200
                |acc dec -154 if foz > -3429
                |piw inc -13 if cli > -175
                |cli inc -374 if acc > 497
                |acc dec -751 if ln != 4821
                |xuu dec 821 if bq < -142
                |nev dec 867 if bb < 565
                |qym dec -883 if hb != -65
                |h inc -289 if ln <= 4818
                |hb dec 839 if s <= 282
                |cli dec 662 if ar > 2253
                |nev inc 975 if acc < 1249
                |foz inc -459 if nfo != 479
                |tem inc 721 if ar < 2244
                |hb dec 645 if bb >= 561
                |cli dec 973 if bq <= -148
                |qym inc 937 if piw <= 1003
                |bq inc -548 if tem != 561
                |nfo dec 98 if nev != -1082
                |u dec 789 if bq >= -688
                |u dec 389 if acc <= 1258
                |bq inc 818 if qym <= 3020
                |acc inc 224 if nfo < 492
                |ln inc -739 if re >= 2547
                |re dec -364 if fe != 843
                |piw dec -836 if piw > 991
                |foz inc 484 if cli < -1519
                |ln inc 869 if hb < -1533
                |piw dec 872 if yxr <= 834
                |bq inc -1 if bq >= 113
                |tl inc -649 if u >= 854
                |ec dec -447 if bb == 566
                |ar dec -702 if u < 861
                |s inc -445 if bb <= 574
                |xuu dec 323 if cli <= -1523
                |foz dec 589 if ln <= 4947
                |s inc -399 if nfo != 484
                |cli inc -704 if acc < 1481
                |yxr dec 712 if hb != -1534
                |nev dec -347 if nev <= -1087
                |ln dec -801 if foz > -4480
                |xuu dec -549 if h >= -47
                |re dec 127 if xuu > -2692
                |hb inc 96 if fe <= 845
                |acc dec 159 if hb >= -1451
                |bq inc 754 if u >= 865
                |u inc 23 if yxr > 106
                |re dec 940 if nfo <= 489
                |qym dec -599 if ar >= 2948
                |qym dec -481 if nfo == 482
                |nfo inc 767 if cli <= -2225
                |bb inc -174 if ec == 56
                |tem inc 615 if ec <= 55
                |tem inc 845 if re > 1848
                |hb inc 940 if tl > -1543
                |bq dec -892 if xuu >= -2686
                |bq inc 996 if bq >= 117
                |re dec 456 if u < 885
                |ln inc 879 if s < -568
                |s dec 605 if ar >= 2941
                |foz dec 585 if hb > -1449
                |ej inc 980 if ej <= -1866
                |re dec -409 if foz <= -5059
                |tem dec -394 if nfo != 476
                |yxr dec -765 if ej <= -888
                |ec inc -540 if fe < 843
                |cli dec 758 if foz != -5056
                |nfo inc 846 if re > 1802
                |re inc 800 if nev < -1078
                |nev dec 2 if yxr < 883
                |piw inc 282 if acc >= 1313
                |h inc -993 if ln <= 5741
                |ar inc 594 if tem <= 1804
                |h inc 502 if h >= -45
                |xuu inc -73 if acc <= 1326
                |re dec 116 if piw >= 1243
                |bq inc 62 if u <= 880
                |acc dec 166 if fe <= 846
                |ec dec -441 if re > 2612
                |re inc 214 if re != 2602
                |u dec 410 if s < -1160
                |ln dec -701 if piw <= 1244
                |tl inc -126 if nfo <= 1330
                |fe inc -861 if tl <= -1669
                |ar dec -998 if yxr <= 882
                |fe inc 629 if ln > 6445
                |hb dec -407 if foz <= -5071
                |s dec 654 if acc <= 1161
                |bb inc 304 if fe <= 615
                |xuu inc 979 if cli == -2969
                |cli inc -973 if fe == 607
                |cli inc 859 if tl <= -1665
                |tl dec 257 if foz > -5064
                |bq dec -253 if hb != -1446
                |s inc 82 if fe == 611
                |h inc -462 if xuu == -2761
                |nev dec 231 if bq <= 1123
                |fe inc -719 if re == 2817
                |nfo inc 553 if ec != -484
                |nev inc -457 if nfo != 1318
                |fe inc -936 if piw <= 1247
                |nev inc -615 if ar > 4539
                |fe inc -267 if ec >= -475
                |qym dec 336 if tem > 1802
                |ej inc 442 if foz > -5064
                |qym inc 864 if h < 0
                |nev inc 565 if foz >= -5057
                |tem inc 257 if ej <= -457
                |hb dec -202 if piw > 1243
                |bq dec -192 if ar != 4550
                |foz dec 374 if ln >= 6454
                |tem inc 381 if ej != -454
                |ec dec -630 if ej == -438
                |foz dec -821 if acc > 1161
                |nev dec 903 if bq != 1315
                |re inc -677 if ej > -448
                |re inc 588 if u < 473
                |xuu dec 114 if foz > -5062
                |acc inc -804 if ec > -492
                |nfo inc 728 if cli == -2120
                |xuu dec -258 if foz > -5064
                |tem dec -370 if ln < 6450
                |acc dec -717 if ej != -442
                |ec dec 262 if qym > 4613
                |nev inc 31 if u >= 468
                |xuu dec 84 if re == 2728
                |u inc 457 if xuu != -2697
                |acc dec 251 if acc != 1068
                |tem dec -745 if acc <= 1076
                |tl dec 581 if tem <= 3304
                |s dec -730 if hb <= -1444
                |ec dec 409 if tem < 3293
                |acc dec -884 if acc >= 1077
                |acc inc 160 if bb < 706
                |ar dec 110 if nfo != 2058
                |ej dec -349 if fe < -1038
                |s inc 681 if s >= -1096
                |cli inc -712 if fe < -1043
                |nfo dec 693 if u != 924
                |ln inc 170 if xuu != -2702
                |bb inc 759 if tl > -2518
                |ln inc 647 if tem > 3296
                |ej inc -35 if re >= 2734
                |qym inc 710 if nev < -3258
                |xuu inc 304 if fe == -1052
                |piw dec 237 if xuu >= -2707
                |tem dec -683 if hb <= -1441
                |piw inc -297 if re >= 2733
                |nfo dec -836 if bb == 1455
                |s inc 901 if u <= 921
                |bb inc 780 if bq == 1307
                |xuu dec 467 if yxr >= 878
                |acc dec -24 if h < 6
                |ec dec 412 if foz >= -5054
                |ec inc -990 if fe == -1047
                |nev dec -940 if nfo != 2208
                |ar dec -670 if qym == 5339
                |foz inc 391 if hb != -1438
                |yxr inc -86 if acc <= 1260
                |qym dec -36 if piw < 1008
                |nev inc -21 if yxr == 788
                |piw inc 581 if hb == -1446
                |bb inc -355 if bb != 2232
                |yxr dec 238 if ar < 4441
                |tem dec -974 if cli < -2826
                |xuu inc -237 if s <= -406
                |ej dec 51 if foz <= -4661
                |cli inc -416 if h < 6
                |fe inc 23 if s < -407
                |bq dec 598 if ej <= -152
                |cli inc -15 if piw > 1584
                |foz dec -781 if nfo < 2209
                |u inc 17 if bb < 1883
                |xuu dec 799 if ej >= -157
                |bb inc 914 if tem != 4960
                |foz dec 853 if fe != -1024
                |piw dec 628 if nfo != 2199
                |qym dec 303 if xuu >= -4207
                |bq dec 854 if yxr >= 549
                |hb inc -123 if xuu >= -4211
                |bb inc 767 if nev <= -2318
                |s inc 482 if nfo > 2196
                |yxr inc 185 if bb < 3570
                |yxr dec 683 if bq != 444
                |fe dec -316 if ec >= -1733
                |bb inc 368 if s < 78
                |tl dec 132 if yxr != 63
                |yxr dec 357 if bq == 453
                |fe inc -256 if ec > -1744
                |xuu inc -193 if ej <= -142
                |xuu dec -605 if cli != -3263
                |ar dec 571 if u != 945
                |nfo inc -430 if nfo < 2209
                |hb inc 236 if hb > -1564
                |ar dec -460 if fe < -1278
                |tl dec -127 if bq > 449
                |s inc -343 if nfo > 1764
                |fe inc 761 if nev < -2326
                |piw dec -136 if xuu != -4391
                |cli dec 639 if yxr >= -301
                |tl dec -524 if ar != 4890
                |ln dec -75 if ln < 7269
                |ln dec -376 if tem > 4956
                |yxr inc -317 if yxr >= -297
                |tem inc 593 if ej == -149
                |cli dec 297 if ar < 4894
                |ej dec -576 if xuu <= -4395
                |bq dec -681 if s >= -277
                |ec dec -404 if u >= 939
                |xuu dec -82 if yxr >= -292
                |nev inc -661 if u <= 954
                |yxr inc 941 if foz > -3896
                |ar dec 559 if ar < 4895
                |re inc 470 if tl <= -1981
                |qym inc 423 if hb <= -1564
                |ec inc -353 if cli >= -4208
                |qym inc 166 if nfo < 1770
                |ln inc 110 if h > -4
                |bb dec 459 if nfo < 1776
                |tem inc -466 if ej > 418
                |tem inc -223 if fe < -1271
                |bb inc 361 if qym > 5650
                |fe dec 698 if nfo <= 1768
                |h inc -761 if acc >= 1258
                |tl dec 307 if cli < -4195
                |ec dec -428 if bq == 1134
                |ec inc 200 if u == 945
                |fe dec 999 if re >= 3198
                |ej dec -150 if ej != 429
                |ar inc -19 if bb < 3822
                |cli inc -271 if piw == 1721
                |piw inc -317 if nev <= -2985
                |xuu dec 732 if xuu == -4393
                |ar dec -738 if bq != 1133
                |cli dec -925 if xuu >= -4400
                |tem dec -336 if acc == 1261
                |xuu dec -396 if tl < -2291
                |tem inc -726 if xuu < -3995
                |xuu inc 586 if yxr < 643
                |nfo inc 554 if re >= 3194
                |ln dec 425 if s < -276
                |s inc -678 if h >= -2
                |acc inc -669 if u == 945
                |nfo inc 136 if re <= 3207
                |nev inc -486 if fe > -2272
                |acc inc 146 if yxr < 650
                |bq inc 637 if u != 945
                |bq inc -562 if u < 949
                |fe dec -164 if tem >= 4128
                |ej inc -868 if ec > -1058
                |foz inc -646 if ej < -295
                |u inc 960 if bb < 3825
                |ln dec -6 if ln > 7826
                |bq dec 972 if acc > 723
                |piw inc 958 if fe != -2115
                |ln inc 179 if ln <= 7830
                |foz inc 206 if bb <= 3828
                |u inc -789 if bb > 3826
                |nev inc -597 if ln <= 8005
                |nev dec -112 if yxr <= 642
                |ej inc 670 if s <= -942
                |ec dec 623 if qym >= 5652
                |bb inc 35 if nev != -3470
                |u dec -50 if ej != 379
                |ar dec 952 if tl != -2306
                |tem inc -92 if re == 3192
                |yxr dec 866 if qym == 5652
                |nfo inc -457 if bb != 3876
                |tem inc 395 if fe != -2125
                |acc inc -37 if piw > 1711
                |tem dec -647 if fe <= -2113
                |tem dec -546 if xuu > -3422
                |piw dec 219 if acc < 699
                |ar dec -392 if piw == 1502
                |s inc 330 if bq > -407
                |bq inc -508 if nev <= -3473
                |ln dec -333 if cli != -3545
                |ln dec 42 if ec > -1683
                |acc inc -247 if ln <= 7966
                |foz inc 563 if tl <= -2287
                |ej inc -390 if ej <= 384
                |acc dec 659 if acc != 443
                |xuu dec -293 if ej >= -18
                |qym dec -169 if yxr > -217
                |cli inc 778 if ln >= 7960
                |xuu dec -179 if u != 163
                |nfo dec 600 if ej == -11
                |ar dec 261 if piw > 1492
                |re inc -889 if yxr > -228
                |h inc 37 if xuu != -2943
                |ln inc 148 if tem > 5731
                |bq dec 564 if piw == 1502
                |piw dec 822 if ar != 4242
                |xuu inc -35 if nfo != 1402
                |u dec -284 if cli > -2777
                |ar dec -521 if yxr >= -225
                |bb dec 412 if yxr <= -222
                |piw inc -199 if tem != 5715
                |bb inc 331 if qym < 5653
                |bq dec -359 if ec > -1687
                |acc inc 298 if bq == -605
                |bq inc -69 if tl < -2305
                |h inc -626 if h == 8
                |qym dec 609 if s <= -619
                |fe inc 165 if nev < -3456
                |xuu inc 690 if ej != -15
                |ec dec -280 if tl < -2294
                |qym dec 829 if bq != -610
                |xuu dec 888 if piw > 489
                |u inc 81 if acc > 80
                |qym dec 247 if h > -9
                |tl dec -558 if bq < -604
                |nfo dec 692 if ln == 7963
                |acc inc -23 if tl > -1735
                |xuu dec 478 if fe == -1950
                |s dec 551 if tl < -1736
                |ec inc -479 if fe == -1950
                |s dec -17 if foz > -3328
                |piw dec 351 if acc == 84
                |bq dec -437 if re == 2309
                |qym dec -808 if cli > -2774
                |h dec -390 if tl == -1738
                |foz dec 676 if ej >= -4
                |re dec 891 if bq != -167
                |bb dec -191 if re != 1418
                |nfo inc 380 if s == -1155
                |ec inc 69 if u < 531
                |nev dec -690 if s >= -1164
                |s dec 571 if bb >= 3776
                |re dec -224 if hb <= -1563
                |bq inc -252 if piw < 131
                |yxr dec -488 if tl == -1741
                |re inc 755 if re > 1638
                |bq inc -208 if qym != 4769
                |qym inc 695 if hb == -1563
                |fe inc -157 if piw < 121
                |xuu dec 579 if acc != 79
                |ln dec -262 if nfo > 1086
                |ec dec 235 if tl >= -1739
                |hb inc 188 if ej >= -13
                |s inc 124 if cli >= -2760
                |ec dec -643 if nfo != 1088
                |re dec -934 if fe > -1959
                |ej inc -58 if s == -1727
                |qym dec -558 if tl >= -1746
                |ln inc -555 if ec != -1402
                |foz inc -833 if cli != -2774
                |ln dec -589 if cli == -2775
                |tl inc -906 if re < 3334
                |tl inc 552 if fe < -1940
                |cli inc 976 if re >= 3322
                |bq inc -101 if xuu >= -3301
                |nev dec -217 if tl < -2087
                |ej dec 945 if u > 519
                |piw inc 447 if fe >= -1957
                |h inc 905 if piw <= 586
                |hb inc -995 if u == 521
                |acc inc 713 if tl >= -2100
                |ar inc -936 if ec <= -1400
                |tl inc 199 if u < 530
                |u dec 584 if ar != 3829
                |tl inc 787 if fe > -1954
                |ar inc 631 if qym >= 5325
                |tl inc -993 if ln < 8232
                |bq dec 900 if yxr > -231
                |u dec -226 if ln < 8235
                |yxr inc -966 if bq <= -1520
                |ar inc -83 if bq < -1520
                |bq dec 439 if bq == -1528
                |ej inc 608 if h < 1294
                |yxr inc 140 if nev >= -2567
                |qym dec 685 if nev >= -2566
                |re dec 957 if hb > -2378
                |tem dec -851 if bq <= -1966
                |nfo dec 421 if qym < 4649
                |acc inc 473 if ar == 4383
                |tem inc -407 if qym >= 4642
                |s dec -949 if tl >= -2103
                |h inc 549 if s >= -784
                |qym dec -311 if xuu >= -3308
                |s dec 751 if yxr > -1060
                |s inc -438 if ar != 4386
                |fe dec 734 if s <= -1965
                |ej inc 989 if hb == -2376
                |tl dec 84 if yxr > -1055
                |re dec 919 if ar < 4393
                |ec dec 314 if qym >= 4657
                |acc dec 572 if piw <= 579
                |bq dec -742 if tem >= 6166
                |ln inc 891 if qym < 4651
                |piw inc 125 if s >= -1956
                |nev inc 530 if piw != 577
                |ln inc 703 if nfo <= 670
                |bb inc 741 if foz >= -4155
                |ln inc -429 if u >= 163
                |ln dec -47 if nfo > 678
                |cli dec 217 if bq < -1222
                |tl dec -485 if fe >= -2690
                |tl dec -948 if fe == -2684
                |nfo inc 786 if qym != 4642
                |bb inc 336 if re != 1455
                |tl inc -8 if nfo != 1455
                |hb inc 173 if bq == -1225
                |s dec -53 if piw < 581
                |bq inc 905 if h == 1842
                |acc inc -450 if nev < -2551
                |ar dec 241 if s != -1914
                |bb inc 671 if ec <= -1400
                |acc inc 895 if fe <= -2691
                |fe dec 776 if s == -1913
                |foz inc -104 if nev >= -2558
                |nfo dec 629 if hb <= -2201
                |u dec 531 if ec < -1404
                |hb dec -37 if yxr < -1049
                |foz dec -586 if h <= 1848
                |nev inc -895 if acc == 248
                |qym dec 475 if bb <= 4461
                |piw inc 773 if fe < -3459
                |nfo dec -142 if cli == -2008
                |hb inc 103 if cli != -1999
                |bq inc 795 if ln == 9390
                |re dec 82 if nfo <= 975
                |u inc -275 if u != 163
                |xuu inc -599 if acc == 248
                |ln inc -642 if hb < -2069
                |hb dec 874 if nfo == 968
                |bb inc -283 if ej != 640
                |bq inc -999 if fe == -3460
                |tem dec 670 if tem != 6174
                |bb inc 890 if re > 1369
                |xuu dec 880 if foz >= -3678
                |ar dec -702 if ej == 641
                |h inc -43 if nev != -3453
                |tl inc -803 if qym >= 4166
                |ej inc 583 if foz > -3687
                |piw inc -517 if qym != 4173
                |nev dec -258 if tl >= -1552
                |tem inc -18 if ln >= 9398
                |qym inc -463 if tem == 5497
                |nfo inc -279 if ln >= 9392
                |nfo dec 627 if ln > 9388
                |acc inc -544 if ej >= 1219
                |xuu dec 201 if foz < -3667
                |qym inc -353 if s >= -1905
                |xuu inc -927 if tem <= 5506
                |cli dec -347 if bb != 5065
                |tl dec -339 if hb == -2937
                |s dec 503 if fe > -3470
                |s dec -187 if acc > -306
                |hb inc 77 if ej > 1214
                |u dec 103 if acc > -296
                |s dec 620 if bb < 5072
                |fe dec -752 if bb == 5063
                |u dec 930 if ec == -1410
                |ec inc -235 if cli != -1661
                |bq inc -361 if cli < -1664
                |xuu inc -582 if s <= -2849
                |re dec 812 if acc <= -292
                |h inc -938 if cli != -1652
                |ec inc -830 if re > 560
                |ec dec -888 if ln < 9385
                |cli inc -520 if tem < 5506
                |qym inc -224 if acc >= -302
                |tl inc -399 if nfo >= 342
                |tem dec 154 if foz == -3671
                |qym inc 500 if nev < -3443
                |tem dec 603 if acc != -295
                |nev inc 227 if ar == 4844
                |fe dec 658 if s > -2857
                |bb dec 327 if xuu < -6498
                |s inc -204 if acc <= -294
                |s inc 393 if s <= -3051
                |foz dec -71 if ec == -2232
                |piw inc 247 if acc != -306
                |tl dec 749 if cli >= -2189
                |bb dec -17 if qym != 3996
                |acc inc 890 if bq != -524
                |ar dec -532 if re == 561
                |ej dec -406 if ej >= 1224
                |ar dec -82 if hb < -2857
                |ej inc -685 if u <= 163
                |nfo dec -354 if yxr < -1053
                |ar dec -346 if yxr >= -1048
                |ej dec -575 if hb > -2864
                |bq inc 220 if bq != -521
                |fe dec -843 if cli <= -2185
                |ej inc -819 if ln > 9390
                |cli inc 105 if acc != -290
                |bb inc -828 if hb > -2866
                |s dec -508 if tem != 4900
                |re inc 153 if re <= 566
                |piw dec 370 if foz <= -3602
                |nfo inc 987 if bb >= 3922
                |fe inc -10 if foz > -3607
                |nfo inc -687 if yxr > -1060
                |nfo inc -580 if nev == -3226
                |fe inc -661 if s <= -2149
                |tl inc -521 if h == 896
                |nev dec 891 if tem > 4891
                |hb inc -162 if cli != -2076
                |yxr dec 696 if nev == -4112
                |tl inc 754 if s <= -2145
                |bq inc -457 if re >= 706
                |re dec -926 if acc >= -286
                |u inc 630 if re <= 719
                |ej dec -635 if u > 791
                |ln dec 631 if ln != 9400
                |qym inc 470 if re >= 722
                |ej dec 419 if cli == -2076
                |foz dec -108 if tl < -1201
                |fe inc -162 if cli <= -2078
                |ar dec 385 if yxr == -1059
                |ec inc 695 if hb < -2852
                |re dec 535 if cli > -2071
                |ej dec 135 if bb < 3926
                |ej dec 13 if hb != -2860
                |ec inc 967 if ec < -1540
                |ar dec 713 if ar >= 5453
                |piw inc -38 if tem >= 4887
                |piw inc 664 if bq == -761
                |ar dec 767 if hb > -2863
                |ej dec 908 if re > 715
                |nev inc 905 if acc <= -291
                |nev inc 395 if hb < -2854
                |piw inc 850 if yxr > -1050
                |tl dec 290 if nfo == 61
                |re inc -770 if xuu < -6496
                |fe inc 828 if ar == 3978
                |fe dec -295 if ar != 3980
                |fe dec 431 if foz >= -3506
                |nfo inc -433 if piw == 1853
                |tl dec -744 if acc > -299
                |s dec -609 if bb <= 3919
                |ar inc 390 if ec != -1547
                |acc inc 404 if hb == -2860
                |nfo inc -17 if s < -2152
                |u inc 338 if h != 910
                |s inc 921 if s >= -2159
                |ec inc -49 if hb > -2863
                |tem inc -144 if tem >= 4893
                |nev dec -847 if qym > 3985
                |yxr dec -885 if ln < 8762
                |cli dec 10 if u == 1131
                |tem dec -592 if yxr != -159
                |ec dec -594 if re >= -57
                |tem inc 66 if u == 1131
                |foz inc -810 if u == 1131
                |tem dec 880 if u > 1126
                |bq inc -604 if acc == 108
                |tem inc 344 if acc < 114
                |foz dec -550 if ej != 1594
                |ar inc 849 if s != -1233
                |nfo dec -903 if u == 1123
                |nev dec -945 if ec <= -987
                |re dec 715 if nev < -1019
                |ar dec -879 if hb == -2860
                |tl inc 432 if bb != 3926
                |s dec 474 if re <= -767
                |ej inc 356 if h >= 896
                |ec dec -229 if hb != -2863
                |bq inc 121 if ec <= -756
                |xuu inc 428 if u == 1131
                |ej inc 796 if xuu >= -6070
                |fe inc 889 if fe < -3341
                |cli dec 524 if acc > 102
                |piw dec -360 if acc != 117
                |fe dec 907 if h >= 907
                |piw dec -184 if ar != 6092
                |u dec 189 if hb >= -2866
                |nev dec -381 if nfo < -362
                |hb inc -568 if re == -771
                |hb inc -365 if xuu != -6079
                |nev dec 490 if qym == 3995
                |u dec 311 if u >= 933
                |hb inc 692 if yxr == -166
                |ej dec -524 if re > -767
                |ln dec 5 if h > 896
                |u dec 801 if u >= 628
                |ar dec -861 if s < -1695
                |nfo inc -921 if tem > 4862
                |ej inc -57 if bq == -1244
                |re dec 726 if ar == 6957
                |yxr dec -344 if s == -1705
                |ln dec -300 if foz != -3764
                |bb dec -372 if bq >= -1251
                |h inc 630 if tl <= -321
                |ln inc -586 if u != -161
                |ec dec -356 if ar != 6959
                |u dec -225 if piw >= 2399
                |ln inc -715 if tl < -321
                |nev inc -649 if u < -164
                |h inc 232 if ec != -407
                |bq dec 689 if piw <= 2399
                |bb inc 52 if xuu == -6071
                |foz inc 8 if s == -1705
                |fe inc 80 if nfo > -1299
                |ec inc 686 if u <= -166
                |foz inc 579 if bb < 4347
                |piw dec -551 if nev <= -1288
                |fe dec -271 if foz < -3743
                |nev dec -386 if qym <= 3993
                |nfo inc 791 if hb == -3101
                |xuu inc -206 if xuu != -6081
                |qym inc 53 if h > 1532
                |hb inc -238 if piw != 2955
                |h inc 947 if yxr < 181
                |re inc 668 if h < 2484
                |cli dec -989 if foz > -3754
                |tl inc -495 if h <= 2486
                |tl dec 884 if tl < -810
                |s inc 405 if fe >= -2107
                |ec dec 842 if yxr >= 174
                |h inc -451 if yxr < 180
                |yxr inc 364 if hb < -3331
                |nfo dec -736 if xuu < -6272
                |ec inc 37 if piw <= 2957
                |cli inc 839 if piw <= 2955
                |foz dec 787 if cli <= -777
                |ar inc 343 if hb == -3339
                |piw dec 939 if ej != 1892
                |piw inc -36 if bq != -1927
                |ej dec -684 if tl != -1710
                |acc inc 882 if h == 2030
                |re dec 372 if acc == 990
                |h inc 23 if qym <= 4041
                |ej dec 446 if u >= -169
                |foz dec 293 if yxr <= 539
                |tl dec -526 if fe > -2110
                |ar inc 35 if ec <= -521
                |ej dec -839 if s != -1295
                |bq inc -816 if ec > -534
                |tl dec -913 if fe >= -2108
                |bb dec 888 if acc >= 983
                |tem inc -951 if hb == -3339
                |ec dec -562 if qym < 4046
                |nfo dec -696 if h >= 2049
                |u dec -376 if yxr == 542
                |tem dec 130 if ej <= 3429
                |nev inc -484 if h >= 2046
                |nev dec -902 if yxr >= 539
                |qym inc 306 if nev >= -498
                |bq inc -306 if hb <= -3334
                |tl dec -807 if fe > -2113
                |nev inc -47 if nev <= -483
                |u dec -228 if xuu == -6277
                |hb dec 925 if s < -1297
                |acc dec 236 if cli > -786
                |h inc 684 if u >= 429
                |nfo dec 912 if u < 440
                |xuu inc 529 if bb < 3464
                |tl inc 143 if qym != 4354
                |ec dec 694 if tem == 3791
                |fe dec -542 if u < 441
                |piw dec 433 if yxr >= 540
                |ln dec -537 if hb > -4272
                |acc dec 419 if acc > 744
                |xuu dec 31 if fe == -1557
                |ej dec -297 if yxr < 533
                |foz inc -281 if ec <= -656
                |cli dec 712 if tl > 677
                |bq dec -871 if foz >= -4817
                |bb inc 381 if ln < 8294
                |nev inc -670 if tem > 3783
                |yxr inc 860 if nfo < 14
                |foz inc -120 if nfo > 25
                |ec inc 793 if fe >= -1563
                |qym dec -827 if qym < 4351
                |ar inc 687 if cli >= -1495
                |bq inc 975 if re < -1191
                |xuu inc 177 if u != 442
                |tem inc 363 if nev != -1207
                |acc inc 707 if cli < -1491
                |nfo dec -381 if tem > 4148
                |ec inc -347 if tl != 697
                |ej dec 995 if s > -1303
                |acc dec 452 if bq == -2080
                |u dec -851 if piw > 1539
                |ln inc -993 if re != -1211
                |nfo dec -591 if tl >= 692
                |s dec -410 if yxr != 549
                |yxr dec -54 if u >= 1288
                |bb dec -733 if re <= -1193
                |piw inc -984 if re < -1193
                |ln dec 133 if ec >= -211
                |acc dec -218 if fe < -1561
                |tem inc 792 if fe > -1567
                |bb dec 359 if yxr != 541
                |tem inc 273 if h != 2735
                |ec inc -714 if nfo < 401
                |hb dec -555 if bb <= 4206
                |nfo inc -151 if ln > 7294
                |hb dec -635 if yxr <= 550
                |acc inc -603 if u < 1282
                |nev inc 705 if cli >= -1488
                |cli inc 568 if ej < 2435
                |nfo dec 795 if nfo >= 246
                |s dec -996 if ln != 7289
                |u inc -530 if nfo > -550
                |tl dec -431 if qym >= 5171
                |fe inc -998 if ec < -920""".stripMargin

}

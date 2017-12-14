
object Day6 {

  type Bank = Vector[Int]

  // Get the index of the maximum in the Bank
  // If a tie get the first occurence
  def firstMax(bank: Bank, max : Int = 0, maxIndex : Int = Int.MaxValue, curIndex : Int = 0) : Int = {

    if(curIndex == bank.size) maxIndex
    else {
      if(bank(curIndex) > max)
        firstMax(bank, bank(curIndex), curIndex, curIndex + 1)
      else
        firstMax(bank, max, maxIndex, curIndex + 1)
    }
  }

  // Recursively redistribute allocations
  def redist(curIndex : Int, allocs : Int, bank: Bank) : Bank = {

    if(allocs == 0) bank
    else redist((curIndex + 1) % bank.size, allocs - 1, bank.updated(curIndex, bank(curIndex) + 1))

  }

  def reallocate(bank: Bank) : Bank = {

    val reallocateIndex = firstMax(bank)

    redist((reallocateIndex + 1) % bank.size, bank(reallocateIndex), bank.updated(reallocateIndex, 0))

  }

  def countStepsToDuplicate(bank: Bank, previous : Set[Bank] = Set.empty, count : Int = 0) : Int = {

    val reallocated = reallocate(bank)

    if(previous.contains(reallocated)) count + 1
    else {
      countStepsToDuplicate(reallocated, previous + reallocated, count + 1)
    }

  }

  // This time instead of a set we will store a Map of Banks to step counts, then when we hit the duplicate
  // we will immediately know the count ...
  def countStepsToDuplicatePartTwo(bank: Bank, previous : Map[Bank,Int] = Map.empty, count : Int = 0) : Int = {

    val reallocated = reallocate(bank)

    previous.get(reallocated) match {
      case Some(dupe) =>
        (count + 1) - dupe

      case None =>
        countStepsToDuplicatePartTwo(reallocated, previous updated (reallocated, count + 1), count + 1)
    }

  }

  def main(args : Array[String]) : Unit = {

    val stepOneInput = "0\t5\t10\t0\t11\t14\t13\t4\t11\t8\t8\t7\t1\t4\t12\t11"

    val stepOneBank : Bank = stepOneInput.split('\t').map(_.toInt).toVector

    val exampleBank : Bank = Vector(0, 2, 7, 0)

    assert(reallocate(exampleBank) == Vector(2,4,1,2))

    // Now for part one, we want reallocate and save each
    // round then return the count of the first matching one

    val exampleBankCount = countStepsToDuplicate(exampleBank)

    println(s"Example bank duplicate count : $exampleBankCount")

    val stepOneBankCount = countStepsToDuplicate(stepOneBank)

    println(s"Step one bank duplicate count : $stepOneBankCount")

    // Part two is the same but we need to count the steps so we know
    // how many steps it was since the duplicate occurred

    val exampleBankCount2 = countStepsToDuplicatePartTwo(exampleBank)

    println(s"Example bank duplicate count part two : $exampleBankCount2")

    val stepTwoBankCount = countStepsToDuplicatePartTwo(stepOneBank)

    println(s"Step two bank duplicate count : $stepTwoBankCount")

  }


}

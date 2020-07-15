package reductions

import org.scalameter._

object ParallelCountChangeRunner {
  @volatile var seqResult = 0
  @volatile var parResult = 0

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> false
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val amount = 350
    val coins = List(1, 2, 5, 10, 15, 20, 30, 35, 50, 55, 100)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println("#========================================#")
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")
    println("#========================================#")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println("#========================================#")
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
      println("#========================================#")
    } catch {
      case _: NotImplementedError =>
        println("Not implemented.")
    }

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange extends ParallelCountChangeInterface {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else if (coins.isEmpty) 0
    else if (coins.length == 1) countOneChange(money, coins.head)
    else countChangeInner(money, coins)
  }

  def countOneChange(money: Int, coin: Int): Int = {
    if (money % coin == 0) 1 else 0
  }

  def countChangeInner(money: Int, coins: List[Int]): Int = {
    val coinsSrt = coins.sorted
    val matrix: MapTable = MapTable.empty()

    for {
      i <- coinsSrt.indices
    } yield matrix.set(i, 0, countOneChange(0, coinsSrt(i)))

    for {
      j <- 0 to money
    } yield matrix.set(0, j, countOneChange(j, coinsSrt.head))

    for {
      i <- 1 until coinsSrt.length
      j <- 1 to money
    } yield {
      if (j < coinsSrt(i)) matrix.set(i, j, matrix.get(i - 1, j).get)
      else matrix.set(i, j, matrix.get(i - 1, j).get + matrix.get(i, j - coinsSrt(i)).get)
    }

    matrix.get(coinsSrt.length - 1, money).get
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (threshold(money, coins) || money <= 10 || coins.isEmpty || coins.length <= 2) countChange(money, coins)
    else {
      val coinsSrt = coins.sorted.reverse
      val (count1, count2) =
      parallel(
        parCountChange(money - coinsSrt.head, coinsSrt, threshold),
        parCountChange(money, coinsSrt.tail, threshold)
      )
      count1 + count2
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (money: Int, _: List[Int]) => {
      val predicate = 3 * money <= 2 * startingMoney
      // println(s"    In moneyThreshold($startingMoney): (3 * $money <= 2 * $startingMoney) == $predicate")
      if (predicate) true else false
    }

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (_: Int, coins: List[Int]) => {
      val predicate = 3 * coins.length <= 2 * totalCoins
      // println(s"    In totalCoinsThreshold($totalCoins): (3 * ${coins.length} <= 2 * $totalCoins) == $predicate")
      if (predicate) true else false
    }

  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (money: Int, coins: List[Int]) => {
      val predicate = 2 * money * coins.length <= startingMoney * allCoins.length
      // println(s"    In combinedThreshold($startingMoney, $allCoins): " +
        // s"(2 * $money * ${coins.length} <= $startingMoney * ${allCoins.length}) == $predicate")
      if (predicate) true else false
    }
  }
}

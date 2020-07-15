package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> false
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = ParenthesesUtils.createArray(length)
    val threshold = 10000

    println(ParenthesesUtils.showArray(chars, 100))

    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var (leftCount: Int, rightCount: Int) = (0, 0)
    for {
      c <- chars
    } yield {
      if (c == '(') leftCount += 1
      if (c == ')'){
        if (leftCount > 0) leftCount -= 1
        else rightCount += 1
      }
    }
    (leftCount, rightCount) == (0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, leftCount: Int, rightCount: Int): (Int, Int) = {
      if (idx >= until) (leftCount, rightCount)
      else if (chars(idx) == '(') traverse(idx + 1, until, leftCount + 1, rightCount)
      else if (chars(idx) == ')') {
        if (leftCount > 0) traverse(idx + 1, until, leftCount - 1, rightCount)
        else traverse(idx + 1, until, leftCount, rightCount + 1)
      }
      else traverse(idx + 1, until, leftCount, rightCount)
    }

    def moreThen(value: Int, edge: Int): Int = if (value > edge) value else edge
    def moreZero(value: Int): Int = moreThen(value, 0)

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (until + from) / 2
        val ((left1, right1), (left2, right2)) = parallel(reduce(from, mid), reduce(mid, until))
        (left2 + moreZero(left1 - right2), right1 + moreZero(right2 - left1))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}

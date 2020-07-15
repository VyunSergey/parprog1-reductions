package reductions

import org.junit.Test

class ParallelCountChangeSuite extends ReductionsSuite {
  import ParallelCountChange._

  @Test def `countChange my own test`: Unit = {
    def check(money: Int, coins: List[Int], expected: Int): Unit = {
      val count = countChange(money, coins)
      assert(count == expected,
        s"countChange($money, $coins) got $count should be $expected")
    }

    check(2, List(1, 2), 2)
    check(5, List(3, 7), 0)
    check(10, List(1, 2), 6)
    check(10, List(1, 2, 3), 14)
    check(10, List(3, 8, 6), 0)
    check(10, List(1, 2, 3, 5), 20)
  }

  @Test def `countChange should return 0 for money < 0`: Unit = {
    def check(money: Int, coins: List[Int]): Unit =
      assert(countChange(money, coins) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  @Test def `countChange should return 1 when money == 0`: Unit = {
    def check(coins: List[Int]): Unit =
      assert(countChange(0, coins) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  @Test def `countChange should return 0 for money > 0 and coins = List()`: Unit = {
    def check(money: Int): Unit =
      assert(countChange(money, List()) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  @Test def `countChange should work when there is only one coin`: Unit = {
    def check(money: Int, coins: List[Int], expected: Int): Unit =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  @Test def `countChange should work for multi-coins`: Unit = {
    def check(money: Int, coins: List[Int], expected: Int): Unit =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }
}

package reductions

import org.junit.Test

class ParenthesesUtilsSuite extends ReductionsSuite {
  import ParenthesesUtils._

  @Test def `randomNum gets random numbers`: Unit = {
    def check(n: Int): Unit = assert(List.range(0, n).contains(randomNum(n)))

    check(1)
    check(1)
    check(1)

    check(2)
    check(2)
    check(2)

    check(100)
    check(100)
    check(100)
  }

  @Test def `randomParentheses gets random parentheses`: Unit = {
    def check(): Unit = assert(List('(', ')', '.').contains(randomParentheses))

    check()
    check()
    check()
    check()
    check()
  }

  @Test def `createArray gets array of random parentheses`: Unit = {
    def check(n: Int): Unit = createArray(5)
      .foreach(a => assert(List('(', ')', '.').contains(a)))

    check(5)
    check(5)
    check(5)
    check(5)
    check(5)
  }
}

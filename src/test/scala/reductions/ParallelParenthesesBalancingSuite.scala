package reductions

import org.junit.Test

class ParallelParenthesesBalancingSuite extends ReductionsSuite {
  import ParallelParenthesesBalancing._

  @Test def `balance should work for empty string`: Unit = {
    def check(input: String, expected: Boolean): Unit =
      assert(balance(input.toArray) == expected,
        s"balance('$input') should be $expected")

    check("", expected = true)
  }

  @Test def `parBalance should work for empty string`: Unit = {
    def check(input: String, expected: Boolean): Unit =
      assert(parBalance(input.toArray, 0) == expected,
        s"balance('$input') should be $expected")

    check("", expected = true)
  }

  @Test def `balance should work for string of length 1`: Unit = {
    def check(input: String, expected: Boolean): Unit =
      assert(balance(input.toArray) == expected,
        s"balance('$input') should be $expected")

    check("(", expected = false)
    check(")", expected = false)
    check(".", expected = true)
  }

  @Test def `parBalance should work for string of length 1`: Unit = {
    def check(input: String, expected: Boolean): Unit =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance('$input') should be $expected")

    check("(", expected = false)
    check(")", expected = false)
    check(".", expected = true)
  }

  @Test def `balance should work for string of length 2`: Unit = {
    def check(input: String, expected: Boolean): Unit =
      assert(balance(input.toArray) == expected,
        s"balance('$input') should be $expected")

    check("()", expected = true)
    check("(())", expected = true)
    check("()()", expected = true)
    check("()()()()", expected = true)
    check("(())()()(()(()))", expected = true)
    check("(()()()()()((())))", expected = true)
    check("(()()(())()()(())())", expected = true)
    check("()()()((((()))))()(())", expected = true)
    check(")(()()()(())", expected = false)
    check("(((((()())()(", expected = false)
    check("))()()(())(", expected = false)
    check(".)()(()", expected = false)
    check(".(()()(())", expected = false)
    check("(.((())()(()))(", expected = false)
    check(").()((()))()))(())(", expected = false)
  }

  @Test def `parBalance should work for string of length 2`: Unit = {
    def check(input: String, expected: Boolean): Unit =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance('$input') should be $expected")

    check("()", expected = true)
    check("(())", expected = true)
    check("()()", expected = true)
    check("()()()()", expected = true)
    check("(())()()(()(()))", expected = true)
    check("(()()()()()((())))", expected = true)
    check("(()()(())()()(())())", expected = true)
    check("()()()((((()))))()(())", expected = true)
    check(")(()()()(())", expected = false)
    check("(((((()())()(", expected = false)
    check("))()()(())(", expected = false)
    check(".)()(()", expected = false)
    check(".(()()(())", expected = false)
    check("(.((())()(()))(", expected = false)
    check(").()((()))()))(())(", expected = false)
  }
}

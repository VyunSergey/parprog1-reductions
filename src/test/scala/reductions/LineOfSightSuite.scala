package reductions

import org.junit.Test
import org.junit.Assert.assertEquals

class LineOfSightSuite extends ReductionsSuite {
  import LineOfSight._

  @Test def `lineOfSight should correctly handle an arrays`: Unit = {
    def check(input: Array[Float], expected: List[Float]): Unit = {
      val output = new Array[Float](input.length)
      lineOfSight(input, output)
      assertEquals(expected, output.toList)
    }

    check(Array[Float](0f, 1f), List(0f, 1f))
    check(Array[Float](0f, 2f), List(0f, 2f))
    check(Array[Float](0f, 3f), List(0f, 3f))
    check(Array[Float](0f, 4f), List(0f, 4f))

    check(Array[Float](0f, 0f, 0f, 0f), List(0f, 0f, 0f, 0f))
    check(Array[Float](0f, 1f, 1f, 1f), List(0f, 1f, 1f, 1f))
    check(Array[Float](0f, 2f, 6f, 12f), List(0f, 2f, 3f, 4f))
    check(Array[Float](0f, 1f, 8f, 9f), List(0f, 1f, 4f, 4f))
  }

  @Test def `upsweepSequential should correctly handle an arrays`: Unit = {
    def check(input: Array[Float], expected: Float): Unit = {
      val output = upsweepSequential(input, 0, input.length)
      assert(expected == output)
    }

    check(Array[Float](0f, 1f), 1f)
    check(Array[Float](0f, 2f), 2f)
    check(Array[Float](0f, 3f), 3f)
    check(Array[Float](0f, 4f), 4f)

    check(Array[Float](0f, 0f, 0f, 0f), 0f)
    check(Array[Float](0f, 1f, 1f, 1f), 1f)
    check(Array[Float](0f, 2f, 6f, 12f), 4f)
    check(Array[Float](0f, 1f, 8f, 9f), 4f)
  }

  @Test def `upsweep should correctly handle an arrays`: Unit = {
    def check(input: Array[Float], expected: Float): Unit = {
      val output = upsweep(input, 0, input.length, 1).maxPrevious
      assert(expected == output)
    }

    check(Array[Float](0f, 1f), 1f)
    check(Array[Float](0f, 2f), 2f)
    check(Array[Float](0f, 3f), 3f)
    check(Array[Float](0f, 4f), 4f)

    check(Array[Float](0f, 0f, 0f, 0f), 0f)
    check(Array[Float](0f, 1f, 1f, 1f), 1f)
    check(Array[Float](0f, 2f, 6f, 12f), 4f)
    check(Array[Float](0f, 1f, 8f, 9f), 4f)
  }

  @Test def `downsweepSequential should correctly handle an arrays`: Unit = {
    def check(input: Array[Float], expected: List[Float]): Unit = {
      val output = new Array[Float](input.length)
      downsweepSequential(input, output, 0f, 0, input.length)
      assertEquals(expected, output.toList)
    }

    check(Array[Float](0f, 1f), List(0f, 1f))
    check(Array[Float](0f, 2f), List(0f, 2f))
    check(Array[Float](0f, 3f), List(0f, 3f))
    check(Array[Float](0f, 4f), List(0f, 4f))

    check(Array[Float](0f, 0f, 0f, 0f), List(0f, 0f, 0f, 0f))
    check(Array[Float](0f, 1f, 1f, 1f), List(0f, 1f, 1f, 1f))
    check(Array[Float](0f, 2f, 6f, 12f), List(0f, 2f, 3f, 4f))
    check(Array[Float](0f, 1f, 8f, 9f), List(0f, 1f, 4f, 4f))
  }

  @Test def `downsweep should correctly handle an arrays`: Unit = {
    def check(input: Array[Float], expected: List[Float]): Unit = {
      val output = new Array[Float](input.length)
      val tree = upsweep(input, 0, input.length, 1)
      downsweep(input, output, 0f, tree)
      assertEquals(expected, output.toList)
    }

    check(Array[Float](0f, 1f), List(0f, 1f))
    check(Array[Float](0f, 2f), List(0f, 2f))
    check(Array[Float](0f, 3f), List(0f, 3f))
    check(Array[Float](0f, 4f), List(0f, 4f))

    check(Array[Float](0f, 0f, 0f, 0f), List(0f, 0f, 0f, 0f))
    check(Array[Float](0f, 1f, 1f, 1f), List(0f, 1f, 1f, 1f))
    check(Array[Float](0f, 2f, 6f, 12f), List(0f, 2f, 3f, 4f))
    check(Array[Float](0f, 1f, 8f, 9f), List(0f, 1f, 4f, 4f))
  }

  @Test def `parLineOfSight should correctly handle an arrays`: Unit = {
    def check(input: Array[Float], expected: List[Float]): Unit = {
      val output = new Array[Float](input.length)
      parLineOfSight(input, output, 1)
      assertEquals(expected, output.toList)
    }

    check(Array[Float](0f, 1f), List(0f, 1f))
    check(Array[Float](0f, 2f), List(0f, 2f))
    check(Array[Float](0f, 3f), List(0f, 3f))
    check(Array[Float](0f, 4f), List(0f, 4f))

    check(Array[Float](0f, 0f, 0f, 0f), List(0f, 0f, 0f, 0f))
    check(Array[Float](0f, 1f, 1f, 1f), List(0f, 1f, 1f, 1f))
    check(Array[Float](0f, 2f, 6f, 12f), List(0f, 2f, 3f, 4f))
    check(Array[Float](0f, 1f, 8f, 9f), List(0f, 1f, 4f, 4f))
  }
}

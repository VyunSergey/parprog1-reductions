import java.util.concurrent._

import scala.util.{DynamicVariable, Random}
import org.scalameter._

import scala.collection.mutable

package object reductions {
  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute: T = body
      }
      Thread.currentThread match {
        case _: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  type MapTableT[K1, K2, V] = mutable.Map[K1, mutable.Map[K2, V]]
  type MapTableEqT[A] = MapTableT[A, A, A]

  class MapTable(private val table: MapTableEqT[Int]) {
    def get(i: Int, j: Int): Option[Int] = {
      table.get(i).flatMap(_.get(j))
    }

    def set(i: Int, j: Int, value: Int): Unit = {
      val row = table.getOrElse(i, mutable.Map.empty[Int, Int])
      row.update(j, value)
      table.update(i, row)
    }

    override def toString: String =
      table.view.mapValues(_.toString).map({case (k, v) => s"$k -> $v"}).mkString("\n")
  }

  object MapTable {
    def apply(table: MapTableEqT[Int]): MapTable = new MapTable(table)
    def empty(): MapTable = apply(mutable.Map.empty[Int, mutable.Map[Int, Int]])
  }

  object ParenthesesUtils {
    def randomNum(edge: Int): Int = {
      Random.nextInt(edge)
    }

    def randomParentheses: Char = {
      if (randomNum(3) == 0) '('
      else if (randomNum(3) == 1) ')'
      else '.'
    }

    def createArray(length: Int): Array[Char] = {
      val chars = new Array[Char](length)
      chars.map(_ => randomParentheses)
    }

    def showArray(chars: Array[Char], n: Int): String = {
      if (chars.length > n) chars.toList.take(n).mkString("'", "", "'") ++ "..."
      else chars.toList.mkString("'", "", "'")
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

  // Workaround Dotty's handling of the existential type KeyValue
  implicit def keyValueCoerce[T](kv: (Key[T], T)): KeyValue = {
    kv.asInstanceOf[KeyValue]
  }
}

package scalaschool

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.Random

object Concurrent {

  implicit val ec = ExecutionContext.global

  def getResultIndependent = {
    val sum = for {
      a <- Future {
        println("1"); 1
      }
      b <- Future {
        println("2"); 2
      }
      c <- Future {
        println("3"); 3
      }
    } yield {
      a + b + c
    }
    sum.onComplete(x => println("sum is" + x))
  }

  def getResultDependent = {
    val sum = for {
      a <- Future {
        println(1); 1
      }
      b <- Future {
        println(a + 1); a + 1
      }
      c <- Future {
        println(b + 1); b + 1
      }
    } yield {
      a + b + c
    }
    sum.onComplete(x => println("sum is" + x))
  }

  def getResultParallel = {
    val fa = Future {
      println("1"); 1
    }
    val fb = Future {
      println("2"); 2
    }
    val fc = Future {
      println("3"); 3
    }
    val sum = for {
      a <- fa
      b <- fb
      c <- fc
    } yield {
      a + b + c
    }
    sum.onComplete(x => println("sum is" + x))
  }

  def referentTransparent: ((Int, Int), (Int, Int)) = {
    val future1 = {
      val r = new Random(0L)
      val x = Future(r.nextInt)
      for {
        a <- x
        b <- x
      } yield (a, b)
    }

    val future2 = {
      val r = new Random(0L)
      for {
        a <- Future(r.nextInt())
        b <- Future(r.nextInt())
      } yield (a, b)
    }

    val r1 = Await.result(future1, 1.seconds)
    val r2 = Await.result(future2, 1.seconds)
    (r1, r2)
  }
}

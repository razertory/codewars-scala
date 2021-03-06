package scalaschool

object Basic {
  var count = 0

  def currentCount(): Long = {
    count += 1
    count
  }

  // 匿名函数
  var addOne = (x: Int) => {
    x + 1
  }

  // 部分应用
  def adder(m: Int, n: Int): Int = {
    m + n
  }

  var add2 = adder(2, _: Int)

  // 柯里化..
  def multiply(m: Int)(n: Int): Int = m * n

  var timesTwo = multiply(2) _

  // 类
  class Calculator {
    val brand: String = "HP"

    def add(m: Int, n: Int): Int = m + n
  }

  // trait
  trait Car {
    val brand: String
  }

  trait Shiny {
    val shineRefraction: Int
  }

  class BMW extends Car with Shiny {
    val brand = "BMW"
    val shineRefraction = 12
  }

  // 类型, 简单的 hashmap
  trait Cache[K, V] {
    def get(key: K): V

    def put(key: K, value: V)

    def delete(key: K)
  }

  class CacheServer extends Cache[String, String] {
    var arr: Array[String] = new Array[String](100)

    def get(key: String): String = {
      val index: Int = key.hashCode() % 100
      this.arr(index)
    }

    def put(key: String, value: String) = {
      val index: Int = key.hashCode() % 100
      this.arr(index) = value
    }

    def delete(key: String) = {
      val index: Int = key.hashCode() % 100
      this.arr(index) = null
    }
  }

  def parallelSum = {
    var now = System.nanoTime()
    val list = (1 to 1000000).toList
    list.filter(i => i % 2 == 1).sum
    println(System.nanoTime() - now)

    now = System.nanoTime()
    list.par.filter(i => i % 2 == 1).sum
    println(System.nanoTime() - now)
  }

  // 纯 FP 的快速排序
  def quickSort(l: List[Integer]): List[Integer] = {
    l match {
      case Nil => Nil
      case x :: xs => {
        val smaller = quickSort(for (
          n <- xs if n < x
        ) yield n)
        val bigger = quickSort(for (
          n <- xs if n >= x
        ) yield n)
        smaller ++ List(x) ++ bigger
      }
    }
  }
}

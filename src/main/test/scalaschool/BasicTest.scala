package scalaschool

import org.scalatest.FunSuite
import scalaschool.Basic.{BMW, CacheServer, Calculator}

class BasicTest extends FunSuite {
  test("testCurrentCount") {
    assert(Basic.currentCount() == 1)
  }

  test("testAddOne") {
    assert(Basic.addOne(1) == 2)
  }

  test("testAdd2") {
    assert(Basic.add2(3) == 5)
  }

  test("testTimesTwo") {
    assert(Basic.timesTwo(3) == 6)
  }

  test("testBasicClass") {
    val calc = new Calculator
    assert(calc.add(1,1) == 2)
  }

  test("testBasicTrait") {
    val bmw = new BMW
    assert(bmw.brand == "BMW")
    assert(bmw.shineRefraction == 12)
  }

  test("testBasicType") {
    val cacheServer = new CacheServer
    cacheServer.put("Love", "u")
    assert(cacheServer.get("Love") == "u")

    cacheServer.put("hate", "you")
    assert(cacheServer.get("hate") == "you")

    cacheServer.delete("hate")
    assert(cacheServer.get("hate") == null)
  }

}

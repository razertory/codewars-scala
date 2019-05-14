package scalaschool

import org.scalatest.FunSuite

class TimerTest extends FunSuite {

  test("testCurrentCount") {
    assert(Timer.currentCount() == 1)
  }

  test("testAddOne") {
    assert(Timer.addOne(1) == 2)
  }

  test("testAdd2") {
    assert(Timer.add2(3) == 5)
  }

  test("testTimesTwo") {
    assert(Timer.timesTwo(3) == 6)
  }

}

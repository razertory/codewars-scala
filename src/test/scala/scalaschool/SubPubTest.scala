package scalaschool

import org.scalatest.FunSuite

class SubPubTest extends FunSuite {

  test("one subscriber") {
    val publisher = Var(1)
    val subscriber = Signal(publisher() * 20)
    assert(subscriber() == 20)
    publisher() = 2
    assert(subscriber() == 40)
  }

  test("multi subscribers") {
    val publisher = Var(1)
    val subscriber1 = Signal(publisher() * 20)
    val subscriber2 = Signal(publisher() * 30)
    val subscriber3 = Signal(publisher() * 40)
    assert(subscriber1() == 20)
    assert(subscriber2() == 30)
    assert(subscriber3() == 40)
    publisher() = 2
    assert(subscriber1() == 40)
    assert(subscriber2() == 60)
    assert(subscriber3() == 80)
  }

  test("multi publishers & subscribers") {
    val publisher1 = Var(1)
    val publisher2 = Var(2)
    val subscriber1 = Signal(publisher1() + publisher2())
    val subscriber2 = Signal(publisher1() * publisher2())
    assert(subscriber1() == 3)
    assert(subscriber2() == 2)
    publisher1() = 3
    publisher2() = 5
    assert(subscriber1() == 8)
    assert(subscriber2() == 15)
  }
}

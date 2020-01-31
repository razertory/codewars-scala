package fpinscala

import org.scalatest.FunSuite

class AlgebraTest extends FunSuite {
  val algebra = Algebra

  test("testOptionMonoid") {
    assert(algebra.optionMonoid.isMonoid(None, None, None))
    assert(algebra.optionMonoid.isMonoid(Some(1), Some(2), Some(3)))
    assert(algebra.optionMonoid.isMonoid(Some(1), Some(2), None))
    assert(algebra.optionMonoid.isMonoid(Some(1), None, Some(3)))
    assert(algebra.optionMonoid.isMonoid(Some(1), None, None))
  }

  test("testListMonoid") {
    assert(algebra.listMonoid.isMonoid(Nil, Nil, Nil))
    assert(algebra.listMonoid.isMonoid(Nil, List(1), List(2)))
    assert(algebra.listMonoid.isMonoid(List(1), Nil, Nil))
  }

  test("testStringMonoid") {
    assert(algebra.stringMonoid.isMonoid("", "", ""))
    assert(algebra.stringMonoid.isMonoid("", "monoid", ""))
    assert(algebra.stringMonoid.isMonoid("", "", "functor"))
  }

  test("testEndoMonoid") {
    val f1 = (x: String) => x
    val f2 = (y: String) => y
    val f3 = (z: String) => z
    assert(!algebra.endoMonoid.isMonoid(f1, f2, f3))
  }

  test("testProductMonoid") {
  }

  test("testFunctionMonoid") {
  }
}

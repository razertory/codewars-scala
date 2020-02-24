package scalaschool

import org.scalatest.FunSuite

class ConcurrentTest extends FunSuite {

  test("testGetResultDependent") {
    Concurrent.getResultDependent
  }

  test("testGetResultIndependent") {
    Concurrent.getResultIndependent
  }

  test("testGetResultParallel") {
    Concurrent.getResultParallel
  }

  test("testReferentTransparent") {
    val (a, b) = Concurrent.referentTransparent
    assert(a._1 == a._2)
    assert(b._1 != b._2)
  }

}

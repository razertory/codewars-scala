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

}

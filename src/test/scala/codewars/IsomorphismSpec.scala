package codewars

import org.scalatest._

class IsomorphismSpec extends FlatSpec with Matchers {

  import Isomorphism._

  val bISO: ISO[Boolean, Boolean] = (!_, !_)

  def lrl[A, B](iso: ISO[A, B]): A => A =
    a => substR(iso)(substL(iso)(a))

  "substL" should "get A => B" in {
    substL(bISO)(true) shouldBe false
    substL(bISO)(false) shouldBe true
    substL(isoBool)(false) shouldBe false
    substL(isoBool)(true) shouldBe true
  }

  "substR" should "get B => A" in {
    substR(bISO)(true) shouldBe false
    substR(bISO)(false) shouldBe true
    substR(isoBool)(false) shouldBe false
    substR(isoBool)(true) shouldBe true
  }

  "combining isomorphisms" should "work with isoEU" in {
    substL(isoEU)(Right(())).isLeft shouldBe true
    for (i <- 1 to 10) {
      val lst = List.fill(i)(())
      lrl(isoEU)(Left(lst)) shouldBe Left(lst)
    }
  }
}
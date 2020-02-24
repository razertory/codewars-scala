package codewars

import org.scalatest._

class MonadsSpec extends FlatSpec with Matchers {

  import Monads._

  "Maybe" should "be a Functor" in {
    Maybe.maybeFunctor.fmap((x: Int) => x + 1)(None) shouldBe None
    Maybe.maybeFunctor.fmap((x: Int) => x + 1)(Some(2)) shouldBe Some(3)
  }

  "Maybe" should "be an Applicative" in {
    Maybe.maybeApplicative.apply(Some((x: Int) => x + 1))(Some(2)) shouldBe Some(3)
    Maybe.maybeApplicative.apply(None)(Some(2)) shouldBe None
  }

  // "Maybe" should "be a Monad" in {
  //   val m: Maybe[Int] = for {
  //     a <- Some(2): Maybe[Int]
  //     b <- Some(3): Maybe[Int]
  //   } yield a + b

  //   m shouldBe Some(5)

  // val n: Maybe[Int] = for {
  //   a <- None
  //   b <- Some(1)
  // } yield a + b

  // n shouldBe None
}
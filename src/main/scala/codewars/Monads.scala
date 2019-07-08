package codewars

object Monads {
  trait Functor[F[_]] {
    def fmap[A, B]: (A => B) => F[A] => F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A]: A => F[A]
    def apply[A, B]: F[A => B] => F[A] => F[B]
    def fmap[A, B]: (A => B) => F[A] => F[B] = ab => fa => apply(pure(ab))(fa)
  }

  trait Monad[M[_]] extends Applicative[M] {
    def unit[A]: A => M[A]
    def flatMap[A, B]: M[A] => (A => M[B]) => M[B]
    def pure[A]: A => M[A] = unit
    def apply[A, B]: M[A => B] => M[A] => M[B] = mab => ma => flatMap(mab)(ab => fmap(ab)(ma))
  }

  sealed trait Maybe[+A]
  case object None extends Maybe[Nothing]
  case class Some[A](a: A) extends Maybe[A]

  object Maybe {
    var maybeFunctor = new Functor[Maybe] {
      def fmap[A, B]: (A => B) => Maybe[A] => Maybe[B] = f => fa => {
        fa match {
          case Some(a) => Some(f(a)) 
          case None => None
        }
      }
    }

    var maybeApplicative = new Applicative[Maybe] {
      def pure[A]: A => Maybe[A] = a => Some(a)
      def apply[A, B]: Maybe[A => B] => Maybe[A] => Maybe[B] = fab => fa => {
        fab match {
          case Some(ab) => fa match {
            case None => None
            case Some(a) => Some(ab(a))
          }
          case None => None
        }
      }
    }

    var maybeMonad = new Monad[Maybe] {
      def unit[A]: A => Maybe[A] = a => Some(a)
      def flatMap[A, B]: Maybe[A] => (A => Maybe[B]) => Maybe[B] = ma => amb => {
        ma match {
          case Some(a) => amb(a)
          case None => None
        }
      }
    }
  }

}
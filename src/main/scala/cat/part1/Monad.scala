package cat.part1

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  //LAWS
  //  calling pure and transforming the result with func is the same as calling func:
  def leftIdentityLaw[A, B](a: A)(f: A => F[B]): Boolean = {
    flatMap(pure(a))(f) ==  f(a)
  }

  def rightIdentityLaw[A, B](a: A)(f: A => F[B]): Boolean = {
    flatMap(pure(a))(f) == this
  }

  def associativeLaw[A, B, C](fa: F[A])(f: A => F[B], g: A => F[C]): Boolean = ???
}
object Monad {
}

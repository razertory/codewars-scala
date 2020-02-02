package cat.part1

/**
 * So far, functor is a class that encapsulates sequencing computations
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = {
    fa: F[A] => map(fa)(f)
  }

  // laws
  def identityLaw[A](fa: F[A]): Boolean = {
    map(fa)(x => x) == fa
  }

  def compositionLaw[A, B](fa: F[A], f: A => B, g: B => A): Boolean = {
    map(map(fa)(f))(g) == map(fa)(g.compose(f))
  }
}

object ListFunctor extends Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B): List[B] = {
    fa.map(f)
  }
}

object OptionFunctor extends Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B): Option[B] = {
    fa.map(f)
  }
}


package cat.part1

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A

  //monoid laws
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.empty) == x && m.combine(m.empty, x) == x
  }

  // legal
  def isMonoid[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    associativeLaw(x, y, z) && identityLaw(x)
  }
}


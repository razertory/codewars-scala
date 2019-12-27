package fpinscala

object Functional {
  // Monoid 幺半群由三个元素构成
  // 1. 一个类型 A
  // 2. 符合结合律的二元操作 op。例如，对于 x: A, y: A, z: A
  //    op(op(x, y), z) 和 op(x, op(y, z)) 是等价的。意思是在 Scala 中 op(op(x, y), z) == op(x, op(y, z))
  // 3. 单位元 zero: A。
  //    对于整数加法操作，单位元就是 0；
  //    对于整数乘法操作，单位元就是 1；
  //    对于字符串拼接操作，单位元就是空字符串 "";
  //    对于数组拼接操作，单位元就是空数组 Nil
  // 简化一下就是，Monoid 是一个类型，一个该类型满足结合律的二元操作和一个该类型的单位元素
  // 再简化就是，Monoid 是一个类型和一个类型的实现法则
  // 在 Scala 中 Monid 可以用一个 trait 表示
  trait Monoid[A] {
    def op(a1: A, b2: A): A
    def zero: A
  }

  // 组合字符串的 monoid
  def stringMonoid = new Monoid[String] {
    override def op(a1: String, b2: String): String = {
      a1 + b2
    }
    override def zero: String = ""
  }

  // 组合列表的 monoid
  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], b2: List[A]): List[A] = {
      a1 ++ b2
    }

    override def zero: List[A] = Nil
  }

  // 自函数 monoid
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, b2: A => A): A => A = {
      a1 compose b2
    }
    override def zero: A => A = A => A
  }

  // 组合 Option 值的 monoid
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x.orElse(y)
    val zero = None
  }

  // 组合函数的 monoid
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))
      val zero: A => B = a => B.zero
    }
  }

  // 使用 Monoid 做最纯粹的 foldMap 实现
  def foldMap[A, B](l: List[A], m: Monoid[B])(f: A => B): B = {
    l match {
      case Nil => m.zero
      case x :: xs => m.op(f(x), foldMap(xs, m)(f))
    }
  }

  /**
  * Monoid 本身还可以泛化，意思是: a, b 如果都是 monoid，那么 Tuple a, b 也是 monoid
  * 这个 monoid 的泛化行为成为 product。只要包含的元素是 monoid，有些数据结构就能构建成 monoid
  */
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      def op(a1: (A, B), b2: (A, B)) = {
        (A.op(a1._1, b2._1), B.op(a1._2, b2._2))
      }
      val zero = (A.zero, B.zero)
    }

  }


  // 使用类型构造器
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    // 这里的类型构造器 List 本质上就是一个 Functor
    val listFunctor = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
    }

    // F[A, B] 是一个 Functor
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = ???
  }

  def map[A, B](oa: Option[A])(f: A => B): Option[A] = ???

  trait Monad[F[_]] {

   def unit[A](a: _ => A): F[A]

   def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

   def map[A, B](fa: F[A])(f: A => B): F[B] = {
     flatMap(fa)(_ => unit(f))
   }

   def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
     flatMap(fa)(a => map(fb)(b => f(a, b)))
 }

   def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
     ms match {
       case Nil => unit(Nil)
       case h :: t => flatMap(f(h))(b =>
         if (!b) filterM(t)(f)
         else map(filterM(t)(f))(h :: _))
     }
  }
}

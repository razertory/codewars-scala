package fpinscala

import scala.annotation.tailrec

object Basic {
  // 阶乘
  def factorial(x: Int): Int = {
    @tailrec
    def go(x: Int, acc: Int): Int = {
      if (x <= 0) acc
      else go(x - 1, x * acc)
    }

    go(x, 1)
  }

  //  斐波那契
  def fib(x: Int): BigInt = {
    if (x <= 1)
      1
    else fib(x - 1) + fib(x - 2)
  }

  // array 数组
  // ordered 比较大小
  def isSorted[T](array: List[T], ordered: (T, T) => Boolean): Boolean = {
    array match {
      case Nil => true
      case x :: Nil => true
      case x :: xs => ordered(x, xs.head) && isSorted(xs, ordered)
    }
  }

  // 实现 compose 方法
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    x => f(g(x))
  }

  // 实现 drop, 不考虑 n 负数
  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (Nil, _) => l
      case (_, 0) => l
      case (_ :: xs, _) => drop(xs, n - 1)
    }
  }

  // 实现库函数 dropwhile
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case x :: xs if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  // foldLeft 求 List 长度
  def length[A](l: List[A]): Int = {
    l.foldLeft(0)((x, _) => x + 1)
  }

  // 尾递归优化版 foldLeft
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(x, z))(f)
    }
  }

  // 用 foldLeft 反转 list
  def reverse[A](l: List[A]): List[A] = {
    l.foldLeft(List[A]())((xs, x) => x :: xs)
  }

  // 泛化的 map
  def map[A, B](l: List[A], f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case x :: xs => f(x) :: map(xs, f)
    }
  }

  // filter
  def filter[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case x :: xs if f(x) => x :: filter(xs, f)
      case _ :: xs => filter(xs, f)
    }
  }

  // l 是否包含一个子序列为 m
  def hasSubSequence[A](l: List[A], m: List[A]): Boolean = {
    (l, m) match {
      case (Nil, ys) => Nil == ys
      case (_, Nil) => true
      case (x :: xs, y :: ys) => {
        if (x == y && hasSubSequence(xs, ys)) true
        else hasSubSequence(xs, y :: ys)
      }
    }
  }

  // 使用一个二元组来组合两个 Option 值。如果两个 Option 都为 None，也返回 None
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }
  }

  // 实现方差
  def vaiance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // 使用 Option 处理异常
  // 每一个有效的输出都包装在 Some 类型里，无效的输入映射为 None，
  // 编译器可以强制调用者显式处理失败的可能性
  def mean(l: Seq[Double]): Option[Double] = {
    if (l.isEmpty) None
    else Some(l.sum / l.length)
  }

  // 通过 lazy 缓存执行结果
  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }

  case class TreeNode[T](value: T, left: TreeNode[T], right: TreeNode[T])

  // 二叉树的深度
  def treeDepth[T](root: TreeNode[T]): Int = {
    def depth[_](root: TreeNode[T], d: Int): Int = {
      if (root == null) d
      else depth(root.left, d + 1).max(depth(root.right, d + 1))
    }

    depth(root, 0)
  }

  // Option 是一个最多只包含一个元素的 List
  // Option 本身自带 flatMap 方法
  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    l match {
      case Nil => None
      case x :: xs => x.flatMap(h => sequence(xs).map(h :: _))
    }
  }

  // 相较于 Option，Either 可以让使用者更清楚知道错误发生的信息
  case class Left[+E](get: E) extends Either[E, Nothing]

  case class Right[+A](get: A) extends Either[Nothing, A]

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }

    def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
      this match {
        case Left(_) => b
        case Right(a) => Right(a)
      }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
    Either[EE, C] = for {a <- this; b1 <- b} yield f(a, b1)
  }

}

package codewars

// https://www.codewars.com/kata/555624b601231dc7a400017a/train/scala
object JosephusSurvivor {
  def josephusSurvivor(n: Int, k: Int): Int = {
    help(n, k) + 1
  }

  def help(n: Int, m: Int): Int = {
    n match {
      case 0 => -1
      case 1 => 0
      case _ => (help(n - 1, m) + m) % n
    }
  }
}










object Timer {
  var count = 0

  def currentCount(): Long = {
    count += 1
    count
  }

  // 匿名函数
  var addOne = (x: Int) => {
    x + 1
  }

  // 部分应用
  def adder(m: Int, n: Int): Int = {
    m + n
  }
  var add2 = adder(2, _:Int)

  // 柯里化
  def multiply(m: Int)(n: Int): Int = m * n
  var timesTwo = multiply(2) _
}

package scalaschool

class Signal[T](expr: => T) {

  import Signal._

  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = (() => expr) // take the expr provided, and assign it to MyExpr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if (myValue != newValue) {
      myValue = newValue // assign the newValue to myValue
      val obs = observers // take the observers into a local value obs
      observers = Set() // clear the set of observers
      obs.foreach(_.computeValue()) // do a compute value for each observer
    }
  }

  def apply() = {
    observers += caller.value // add the current caller to the set of observers
    assert(!caller.value.observers.contains(this), "cyclic signal definition") // S() = S() + 1 i.e. signal which depends on itself cannot be observed.
    myValue
  }
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}

object Signal {
  val caller = new StackableVariable[Signal[_]](NoSignal)

  def apply[T](expr: => T) = new Signal(expr)
}

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr) // re use the same implementation, and expose it publicly
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}

class StackableVariable[T](init: T) {
  private var values: List[T] = List(init)

  def value: T = values.head

  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op finally values = values.tail
  }
}


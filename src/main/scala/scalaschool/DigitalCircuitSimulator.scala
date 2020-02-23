package scalaschool

trait Simulation {
  type Action = (() => Unit)
  case class Event(time: Int, action: Action)
  type Agenda = List[Event]
  var agenda: Agenda = List[Event]()
  var currentTime = 0
}

trait Parameters {
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}

object DigitalCircuitSimulator extends Simulation with Parameters {

  class Wire {
    private var sigVal = false // value of the current signal, returned using getSignal()
    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal

    def setSignal(s: Boolean): Unit =
      if (s != sigVal) {
        sigVal = s
        actions foreach (_ ())
      }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  val a, b, c = new Wire

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSig
      }
    }

    input addAction invertAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2sig = in2.getSignal
      afterDelay(AndGateDelay) { output setSignal (in1Sig & in2sig) }
    }
    in1 addAction andAction
    in2 addAction andAction
  }

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2sig = in2.getSignal
      afterDelay(OrGateDelay) { output setSignal (in1Sig | in2sig) }
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  // this half-adder can in turn be used to define a full-adder:
  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      currentTime = first.time
      first.action()
      loop()
    case Nil =>
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** Simulation started, time = " +currentTime+" ***")
    }
    loop()
  }

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time => // '<=' is 'less than or equal to'
      first :: insert(rest, item)
    case _ =>
      item :: ag
  }
}


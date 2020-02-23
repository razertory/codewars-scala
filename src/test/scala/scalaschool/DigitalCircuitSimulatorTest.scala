package scalaschool

import org.scalatest.FunSuite
import scalaschool.DigitalCircuitSimulator.Wire

class DigitalCircuitSimulatorTest extends FunSuite {

  test("run simulation") {
    val sim = DigitalCircuitSimulator
    val in1, in2, sum, carry = new Wire
    sim.halfAdder(in1, in2, sum, carry)
    sim.probe("sum", sum)
    sim.probe("carry", carry)
    in1.setSignal(true)
    sim.run()
    in1.setSignal(true)
    sim.run()
    in1.setSignal(false)
    sim.run()
  }
}

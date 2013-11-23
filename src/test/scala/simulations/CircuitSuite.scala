package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  def wires(xs: List[Boolean]):List[Wire] = xs map {
    x => {
      val w = new Wire()
      w.setSignal(x)
      w
    }
  }
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate test") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false)
    
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true)
    
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true)
  }
  
  test("orGate2 test") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false)
    
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true)
    
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true)
  }
  
  test("demux") {
    val in = new Wire
    val ctrlWires = List(new Wire, new Wire)
    val outWires = List(new Wire, new Wire, new Wire, new Wire)
    demux(in, ctrlWires, outWires)
    
    in.setSignal(false)
    ctrlWires(0).setSignal(false)
    ctrlWires(1).setSignal(false)
    run
    assert(outWires === wires(List(false, false, false, false)))
    
    ctrlWires(1).setSignal(true)
    run
    assert(outWires === wires(List(false, false, false, false)))
    
    ctrlWires(0).setSignal(true)
    ctrlWires(1).setSignal(false)
    run
    assert(outWires === wires(List(false, false, false, false)))
    
    ctrlWires(1).setSignal(true)
    run
    assert(outWires === wires(List(false, false, false, false)))
    
    in.setSignal(true)
    ctrlWires(0).setSignal(false)
    ctrlWires(1).setSignal(false)
    run
    assert(outWires === wires(List(true, false, false, false)))
    
    ctrlWires(1).setSignal(true)
    run
    assert(outWires === wires(List(false, true, false, false)))
    
    ctrlWires(0).setSignal(true)
    ctrlWires(1).setSignal(false)
    run
    assert(outWires === wires(List(false, false, true, false)))
    
    ctrlWires(1).setSignal(true)
    run
    assert(outWires === wires(List(false, false, false, true)))
  }

}

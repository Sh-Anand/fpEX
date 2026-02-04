package fpex

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Top"

  it should "pass a simple sanity check" in {
    test(new Top) { dut =>
      dut.io.in.poke(3.U)
      dut.clock.step()
      dut.io.out.expect(3.U)
    }
  }
}

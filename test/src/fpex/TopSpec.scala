package fpex

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FPEXSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FPEX"

  it should "elaborate with default parameters" in {
    test(new FPEX()) { _ => }
  }
}

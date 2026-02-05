package fpex

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FPEXSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FPEX"

  private def fp32ExpAllOnes = 0x7f800000L

  private def isNaN(bits: BigInt, expWidth: Int, fracWidth: Int): Boolean = {
    val expMask = (BigInt(1) << expWidth) - 1
    val fracMask = (BigInt(1) << fracWidth) - 1
    val exp = (bits >> fracWidth) & expMask
    val frac = bits & fracMask
    exp == expMask && frac != 0
  }

  private def driveAndAwait(dut: FPEX, value: Long, lanes: Int, neg: Boolean = false, maxCycles: Int = 10): Seq[BigInt] = {
    dut.io.resp.ready.poke(true.B)
    dut.io.req.valid.poke(true.B)
    dut.io.req.bits.neg.poke(neg.B)
    dut.io.req.bits.laneMask.poke(((1 << lanes) - 1).U)
    for (i <- 0 until lanes) {
      dut.io.req.bits.xVec(i).poke(value.U)
    }
    var cycles = 0
    while (cycles < maxCycles && !dut.io.resp.valid.peek().litToBoolean) {
      dut.clock.step()
      cycles += 1
    }
    assert(dut.io.resp.valid.peek().litToBoolean, "response did not become valid")
    val out = for (i <- 0 until lanes) yield dut.io.resp.bits.result(i).peek().litValue
    dut.clock.step() // consume response and return to READY
    dut.io.req.valid.poke(false.B)
    out
  }

  it should "handle special cases (FP32)" in {
    test(new FPEX(FPType.FP32T, numLanes = 4)) { dut =>
      val lanes = 4

      // NaN -> NaN
      val nanIn = 0x7fc00000L
      val nanOut = driveAndAwait(dut, nanIn, lanes)
      nanOut.foreach(v => assert(isNaN(v, 8, 23), s"expected NaN, got 0x${v.toString(16)}"))

      // +Inf -> +Inf
      val posInf = 0x7f800000L
      val posInfOut = driveAndAwait(dut, posInf, lanes)
      posInfOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      // -Inf -> +0
      val negInf = 0xff800000L
      val negInfOut = driveAndAwait(dut, negInf, lanes)
      negInfOut.foreach(v => assert(v == 0x00000000L, s"expected +0, got 0x${v.toString(16)}"))

      // +0 -> 1.0
      val posZero = 0x00000000L
      val one = 0x3f800000L
      val posZeroOut = driveAndAwait(dut, posZero, lanes)
      posZeroOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      // subnormal -> 1.0 (flush-to-zero)
      val subnormal = 0x00000001L
      val subnormalOut = driveAndAwait(dut, subnormal, lanes)
      subnormalOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      // overflow -> +Inf
      val overMaxX = 0x42b17219L
      val overMaxXOut = driveAndAwait(dut, overMaxX, lanes)
      overMaxXOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      // negation special cases
      val nanNegOut = driveAndAwait(dut, nanIn, lanes, neg = true)
      nanNegOut.foreach(v => assert(isNaN(v, 8, 23), s"expected NaN, got 0x${v.toString(16)}"))

      val posInfNegOut = driveAndAwait(dut, posInf, lanes, neg = true)
      posInfNegOut.foreach(v => assert(v == 0x00000000L, s"expected +0, got 0x${v.toString(16)}"))

      val negInfNegOut = driveAndAwait(dut, negInf, lanes, neg = true)
      negInfNegOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      val posZeroNegOut = driveAndAwait(dut, posZero, lanes, neg = true)
      posZeroNegOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      val subnormalNegOut = driveAndAwait(dut, subnormal, lanes, neg = true)
      subnormalNegOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      val negOverflowIn = 0xc2b17219L
      val negOverflowOut = driveAndAwait(dut, negOverflowIn, lanes, neg = true)
      negOverflowOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))
    }
  }

  it should "handle special cases (FP16)" in {
    test(new FPEX(FPType.FP16T, numLanes = 4)) { dut =>
      val lanes = 4

      // NaN -> NaN
      val nanIn = 0x7e00L
      val nanOut = driveAndAwait(dut, nanIn, lanes)
      nanOut.foreach(v => assert(isNaN(v, 5, 10), s"expected NaN, got 0x${v.toString(16)}"))

      // +Inf -> +Inf
      val posInf = 0x7c00L
      val posInfOut = driveAndAwait(dut, posInf, lanes)
      posInfOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      // -Inf -> +0
      val negInf = 0xfc00L
      val negInfOut = driveAndAwait(dut, negInf, lanes)
      negInfOut.foreach(v => assert(v == 0x0000L, s"expected +0, got 0x${v.toString(16)}"))

      // +0 -> 1.0
      val posZero = 0x0000L
      val one = 0x3c00L
      val posZeroOut = driveAndAwait(dut, posZero, lanes)
      posZeroOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      // subnormal -> 1.0 (flush-to-zero)
      val subnormal = 0x0001L
      val subnormalOut = driveAndAwait(dut, subnormal, lanes)
      subnormalOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      // overflow -> +Inf
      val overMaxX = 0x498dL
      val overMaxXOut = driveAndAwait(dut, overMaxX, lanes)
      overMaxXOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      // negation special cases
      val nanNegOut = driveAndAwait(dut, nanIn, lanes, neg = true)
      nanNegOut.foreach(v => assert(isNaN(v, 5, 10), s"expected NaN, got 0x${v.toString(16)}"))

      val posInfNegOut = driveAndAwait(dut, posInf, lanes, neg = true)
      posInfNegOut.foreach(v => assert(v == 0x0000L, s"expected +0, got 0x${v.toString(16)}"))

      val negInfNegOut = driveAndAwait(dut, negInf, lanes, neg = true)
      negInfNegOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      val posZeroNegOut = driveAndAwait(dut, posZero, lanes, neg = true)
      posZeroNegOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      val subnormalNegOut = driveAndAwait(dut, subnormal, lanes, neg = true)
      subnormalNegOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      val negOverflowIn = 0xc98dL
      val negOverflowOut = driveAndAwait(dut, negOverflowIn, lanes, neg = true)
      negOverflowOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))
    }
  }

  it should "handle special cases (BF16)" in {
    test(new FPEX(FPType.BF16T, numLanes = 4)) { dut =>
      val lanes = 4

      // NaN -> NaN
      val nanIn = 0x7fc0L
      val nanOut = driveAndAwait(dut, nanIn, lanes)
      nanOut.foreach(v => assert(isNaN(v, 8, 7), s"expected NaN, got 0x${v.toString(16)}"))

      // +Inf -> +Inf
      val posInf = 0x7f80L
      val posInfOut = driveAndAwait(dut, posInf, lanes)
      posInfOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      // -Inf -> +0
      val negInf = 0xff80L
      val negInfOut = driveAndAwait(dut, negInf, lanes)
      negInfOut.foreach(v => assert(v == 0x0000L, s"expected +0, got 0x${v.toString(16)}"))

      // +0 -> 1.0
      val posZero = 0x0000L
      val one = 0x3f80L
      val posZeroOut = driveAndAwait(dut, posZero, lanes)
      posZeroOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      // subnormal -> 1.0 (flush-to-zero)
      val subnormal = 0x0001L
      val subnormalOut = driveAndAwait(dut, subnormal, lanes)
      subnormalOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      // overflow -> +Inf
      val overMaxX = 0x42b2L
      val overMaxXOut = driveAndAwait(dut, overMaxX, lanes)
      overMaxXOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      // negation special cases
      val nanNegOut = driveAndAwait(dut, nanIn, lanes, neg = true)
      nanNegOut.foreach(v => assert(isNaN(v, 8, 7), s"expected NaN, got 0x${v.toString(16)}"))

      val posInfNegOut = driveAndAwait(dut, posInf, lanes, neg = true)
      posInfNegOut.foreach(v => assert(v == 0x0000L, s"expected +0, got 0x${v.toString(16)}"))

      val negInfNegOut = driveAndAwait(dut, negInf, lanes, neg = true)
      negInfNegOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))

      val posZeroNegOut = driveAndAwait(dut, posZero, lanes, neg = true)
      posZeroNegOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      val subnormalNegOut = driveAndAwait(dut, subnormal, lanes, neg = true)
      subnormalNegOut.foreach(v => assert(v == one, s"expected 1.0, got 0x${v.toString(16)}"))

      val negOverflowIn = 0xc2b2L
      val negOverflowOut = driveAndAwait(dut, negOverflowIn, lanes, neg = true)
      negOverflowOut.foreach(v => assert(v == posInf, s"expected +Inf, got 0x${v.toString(16)}"))
    }
  }
}

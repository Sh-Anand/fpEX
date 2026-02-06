package fpex

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

abstract class FPEXSpecBase extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FPEX"

  protected def profileName: String
  protected def randomCount: Int
  protected def gridCount: Int
  protected def approxLanes: Int

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

  private def fp32BitsToFloat(bits: BigInt): Float =
    java.lang.Float.intBitsToFloat(bits.toInt)

  private def fp32FloatToBits(f: Float): Long =
    java.lang.Float.floatToIntBits(f).toLong & 0xffffffffL

  private def ulpDiff32(aBits: Long, bBits: Long): Long = {
    def ordered(x: Long): Long = {
      val sign = (x >>> 31) & 1L
      if (sign == 1L) (~x) & 0xffffffffL else x | 0x80000000L
    }
    val oa = ordered(aBits)
    val ob = ordered(bBits)
    (oa - ob).abs
  }


  private def bf16BitsToFloat(bits: BigInt): Float =
    java.lang.Float.intBitsToFloat((bits.toInt & 0xffff) << 16)

  private def floatToBf16Bits(f: Float): Long =
    ((java.lang.Float.floatToIntBits(f) >>> 16) & 0xffff).toLong

  private def fp16BitsToFloat(bits: BigInt): Float = {
    val h = bits.toInt & 0xffff
    val sign = (h >>> 15) & 0x1
    val exp = (h >>> 10) & 0x1f
    val frac = h & 0x3ff
    val fexp =
      if (exp == 0) 0
      else if (exp == 0x1f) 0xff
      else exp - 15 + 127
    val ffrac =
      if (exp == 0 || exp == 0x1f) frac << 13
      else frac << 13
    val fbits = (sign << 31) | (fexp << 23) | ffrac
    java.lang.Float.intBitsToFloat(fbits)
  }

  private def floatToFp16Bits(f: Float): Long = {
    val bits = java.lang.Float.floatToIntBits(f)
    val sign = (bits >>> 31) & 0x1
    val exp = (bits >>> 23) & 0xff
    val frac = bits & 0x7fffff
    val hexp =
      if (exp == 0) 0
      else if (exp == 0xff) 0x1f
      else exp - 127 + 15
    val hfrac = frac >>> 13
    ((sign << 15) | ((hexp & 0x1f) << 10) | (hfrac & 0x3ff)).toLong
  }

  private def ulpDiff16(aBits: Long, bBits: Long): Long = {
    def ordered(x: Long): Long = {
      val sign = (x >>> 15) & 1L
      val masked = x & 0xffffL
      if (sign == 1L) (~masked) & 0xffffL else masked | 0x8000L
    }
    val oa = ordered(aBits)
    val ob = ordered(bBits)
    (oa - ob).abs
  }

  private def wideInputSet(
      seed: Int,
      min: Float,
      max: Float,
      randomCountOverride: Int = randomCount,
      gridCountOverride: Int = gridCount
  ): Seq[Float] = {
    val rng = new Random(seed)
    val anchors = Seq(0.0f, 0.5f, -1.0f, 2.0f, -2.0f, 3.0f, -3.0f, 5.0f, -5.0f)
    val grid =
      if (gridCountOverride <= 1) Seq(min)
      else (0 until gridCountOverride).map(i => min + (max - min) * (i.toFloat / (gridCountOverride - 1).toFloat))
    val random = Seq.fill(randomCountOverride)(min + rng.nextFloat() * (max - min))
    (anchors ++ grid ++ random).distinct
  }

  private def printUlpStats(testName: String, diffs: Seq[Long]): Unit = {
    require(diffs.nonEmpty, s"$testName had no samples")
    val avg = diffs.map(_.toDouble).sum / diffs.size.toDouble
    val worst = diffs.max
    val best = diffs.min
    println(f"[$testName/$profileName] samples=${diffs.size} avgULP=$avg%.4f worstULP=$worst bestULP=$best")
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

  it should "approximate exp for normal FP32 values" in {
    test(new FPEX(FPType.FP32T, numLanes = approxLanes)) { dut =>
      val lanes = approxLanes
      val inputs = wideInputSet(seed = 1, min = -10.0f, max = 10.0f)
      val diffs = scala.collection.mutable.ArrayBuffer.empty[Long]
      inputs.foreach { in =>
        val inBits = fp32FloatToBits(in)
        val out = driveAndAwait(dut, inBits, lanes)
        val quantIn = fp32BitsToFloat(inBits).toDouble
        val expectedBits = fp32FloatToBits(math.exp(quantIn).toFloat)
        out.foreach { v =>
          val gotBits = v.toLong & 0xffffffffL
          val diff = ulpDiff32(gotBits, expectedBits)
          diffs += diff
        }
      }
      printUlpStats("FP32", diffs.toSeq)
      val avg = diffs.map(_.toDouble).sum / diffs.size.toDouble
      assert(avg < 2.0, f"average FP32 ULP $avg%.4f is not < 2.0")
    }
  }


  it should "approximate exp for normal FP16 values" in {
    test(new FPEX(FPType.FP16T, numLanes = approxLanes)) { dut =>
      val lanes = approxLanes
      val inputs = wideInputSet(seed = 2, min = -8.0f, max = 8.0f)
      val diffs = scala.collection.mutable.ArrayBuffer.empty[Long]
      inputs.foreach { in =>
        val inBits = floatToFp16Bits(in)
        val out = driveAndAwait(dut, inBits, lanes)
        val quantIn = fp16BitsToFloat(inBits).toDouble
        val expectedBits = floatToFp16Bits(math.exp(quantIn).toFloat)
        out.foreach { v =>
          val gotBits = v.toLong & 0xffffL
          val diff = ulpDiff16(gotBits, expectedBits)
          diffs += diff
        }
      }
      printUlpStats("FP16", diffs.toSeq)
      val avg = diffs.map(_.toDouble).sum / diffs.size.toDouble
      assert(avg < 2.0, f"average FP16 ULP $avg%.4f is not < 2.0")
    }
  }

  it should "approximate exp for normal BF16 values" in {
    test(new FPEX(FPType.BF16T, numLanes = approxLanes)) { dut =>
      val lanes = approxLanes
      val inputs = wideInputSet(seed = 3, min = -10.0f, max = 10.0f)
      val diffs = scala.collection.mutable.ArrayBuffer.empty[Long]
      inputs.foreach { in =>
        val inBits = floatToBf16Bits(in)
        val out = driveAndAwait(dut, inBits, lanes)
        val quantIn = bf16BitsToFloat(inBits).toDouble
        val expectedBits = floatToBf16Bits(math.exp(quantIn).toFloat)
        out.foreach { v =>
          val gotBits = v.toLong & 0xffffL
          val diff = ulpDiff16(gotBits, expectedBits)
          diffs += diff
        }
      }
      printUlpStats("BF16", diffs.toSeq)
      val avg = diffs.map(_.toDouble).sum / diffs.size.toDouble
      assert(avg < 2.0, f"average BF16 ULP $avg%.4f is not < 2.0")
    }
  }
}

class FPEXShortSpec extends FPEXSpecBase {
  override protected def profileName: String = "short"
  override protected def randomCount: Int = 24
  override protected def gridCount: Int = 11
  override protected def approxLanes: Int = 1
}

class FPEXMediumSpec extends FPEXSpecBase {
  override protected def profileName: String = "medium"
  override protected def randomCount: Int = 96
  override protected def gridCount: Int = 25
  override protected def approxLanes: Int = 1
}

class FPEXLongSpec extends FPEXSpecBase {
  override protected def profileName: String = "long"
  override protected def randomCount: Int = 384
  override protected def gridCount: Int = 65
  override protected def approxLanes: Int = 1
}

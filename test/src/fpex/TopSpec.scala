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
  protected def enableFP32Tests: Boolean = true
  protected def enableFP16Tests: Boolean = true
  protected def enableBF16Tests: Boolean = true

  private def fp32ExpAllOnes = 0x7f800000L

  private def isNaN(bits: BigInt, expWidth: Int, fracWidth: Int): Boolean = {
    val expMask = (BigInt(1) << expWidth) - 1
    val fracMask = (BigInt(1) << fracWidth) - 1
    val exp = (bits >> fracWidth) & expMask
    val frac = bits & fracMask
    exp == expMask && frac != 0
  }

  private def initializeInterface(dut: FPEX, lanes: Int, drainCycles: Int = 8): Unit = {
    dut.io.req.valid.poke(false.B)
    dut.io.req.bits.roundingMode.poke(0.U)
    dut.io.req.bits.tag.poke(0.U)
    dut.io.req.bits.neg.poke(false.B)
    dut.io.req.bits.laneMask.poke(((1 << lanes) - 1).U)
    for (i <- 0 until lanes) {
      dut.io.req.bits.xVec(i).poke(0.U)
    }
    dut.io.resp.ready.poke(true.B)
    if (drainCycles > 0) dut.clock.step(drainCycles)
  }

  private def driveAndAwait(dut: FPEX, value: Long, lanes: Int, neg: Boolean = false, maxCycles: Int = 64): Seq[BigInt] = {
    dut.io.resp.ready.poke(true.B)
    dut.io.req.bits.roundingMode.poke(0.U)
    dut.io.req.bits.tag.poke(0.U)
    dut.io.req.bits.neg.poke(neg.B)
    dut.io.req.bits.laneMask.poke(((1 << lanes) - 1).U)
    for (i <- 0 until lanes) {
      dut.io.req.bits.xVec(i).poke(value.U)
    }
    dut.io.req.valid.poke(true.B)
    var reqCycles = 0
    while (reqCycles < maxCycles && !dut.io.req.ready.peek().litToBoolean) {
      dut.clock.step()
      reqCycles += 1
    }
    assert(dut.io.req.ready.peek().litToBoolean, "request was not accepted (req.ready stayed low)")
    dut.clock.step() // issue request on req.fire
    dut.io.req.valid.poke(false.B)

    var respCycles = 0
    while (respCycles < maxCycles && !dut.io.resp.valid.peek().litToBoolean) {
      dut.clock.step()
      respCycles += 1
    }
    assert(dut.io.resp.valid.peek().litToBoolean, "response did not become valid")
    val out = for (i <- 0 until lanes) yield dut.io.resp.bits.result(i).peek().litValue
    dut.clock.step() // consume response and return to READY
    out
  }

  private def driveAndAwaitVec(dut: FPEX, values: Seq[Long], neg: Boolean = false, maxCycles: Int = 64): Seq[BigInt] = {
    val lanes = values.size
    dut.io.resp.ready.poke(true.B)
    dut.io.req.bits.roundingMode.poke(0.U)
    dut.io.req.bits.tag.poke(0.U)
    dut.io.req.bits.neg.poke(neg.B)
    dut.io.req.bits.laneMask.poke(((1 << lanes) - 1).U)
    values.zipWithIndex.foreach { case (value, i) =>
      dut.io.req.bits.xVec(i).poke(value.U)
    }
    dut.io.req.valid.poke(true.B)
    var reqCycles = 0
    while (reqCycles < maxCycles && !dut.io.req.ready.peek().litToBoolean) {
      dut.clock.step()
      reqCycles += 1
    }
    assert(dut.io.req.ready.peek().litToBoolean, "request was not accepted (req.ready stayed low)")
    dut.clock.step() // issue request on req.fire
    dut.io.req.valid.poke(false.B)

    var respCycles = 0
    while (respCycles < maxCycles && !dut.io.resp.valid.peek().litToBoolean) {
      dut.clock.step()
      respCycles += 1
    }
    assert(dut.io.resp.valid.peek().litToBoolean, "response did not become valid")
    val out = for (i <- 0 until lanes) yield dut.io.resp.bits.result(i).peek().litValue
    dut.clock.step()
    out
  }

  private def driveAndAwaitVecMasked(
      dut: FPEX,
      values: Seq[Long],
      laneMask: Int,
      neg: Boolean = false,
      maxCycles: Int = 64
  ): (BigInt, Seq[BigInt]) = {
    val lanes = values.size
    dut.io.resp.ready.poke(true.B)
    dut.io.req.bits.roundingMode.poke(0.U)
    dut.io.req.bits.tag.poke(0.U)
    dut.io.req.bits.neg.poke(neg.B)
    dut.io.req.bits.laneMask.poke(laneMask.U)
    values.zipWithIndex.foreach { case (value, i) =>
      dut.io.req.bits.xVec(i).poke(value.U)
    }
    dut.io.req.valid.poke(true.B)
    var reqCycles = 0
    while (reqCycles < maxCycles && !dut.io.req.ready.peek().litToBoolean) {
      dut.clock.step()
      reqCycles += 1
    }
    assert(dut.io.req.ready.peek().litToBoolean, "request was not accepted (req.ready stayed low)")
    dut.clock.step()
    dut.io.req.valid.poke(false.B)

    var respCycles = 0
    while (respCycles < maxCycles && !dut.io.resp.valid.peek().litToBoolean) {
      dut.clock.step()
      respCycles += 1
    }
    assert(dut.io.resp.valid.peek().litToBoolean, "response did not become valid")
    val outMask = dut.io.resp.bits.laneMask.peek().litValue
    val out = for (i <- 0 until lanes) yield dut.io.resp.bits.result(i).peek().litValue
    dut.clock.step()
    (outMask, out)
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
    val fbits =
      if (exp == 0) {
        if (frac == 0) {
          sign << 31
        } else {
          var mant = frac
          var e = -14
          while ((mant & 0x400) == 0) {
            mant <<= 1
            e -= 1
          }
          mant &= 0x3ff
          val fexp = e + 127
          val ffrac = mant << 13
          (sign << 31) | (fexp << 23) | ffrac
        }
      } else if (exp == 0x1f) {
        val fexp = 0xff
        val ffrac = frac << 13
        (sign << 31) | (fexp << 23) | ffrac
      } else {
        val fexp = exp - 15 + 127
        val ffrac = frac << 13
        (sign << 31) | (fexp << 23) | ffrac
      }
    java.lang.Float.intBitsToFloat(fbits)
  }

  private def floatToFp16Bits(f: Float): Long = {
    val bits = java.lang.Float.floatToIntBits(f)
    val sign = (bits >>> 16) & 0x8000
    val exp = (bits >>> 23) & 0xff
    val frac = bits & 0x7fffff
    val out =
      if (exp == 0xff) {
        if (frac == 0) sign | 0x7c00
        else sign | 0x7e00
      } else {
        val unbiased = exp - 127
        var hexp = unbiased + 15
        if (hexp >= 0x1f) {
          sign | 0x7c00
        } else if (hexp <= 0) {
          if (hexp < -10) {
            sign
          } else {
            val mant = frac | 0x800000
            val shift = 14 - hexp
            var hfrac = mant >>> shift
            val remMask = (1 << shift) - 1
            val rem = mant & remMask
            val halfway = 1 << (shift - 1)
            if (rem > halfway || (rem == halfway && (hfrac & 1) == 1)) {
              hfrac += 1
            }
            sign | (hfrac & 0x3ff)
          }
        } else {
          var hfrac = frac >>> 13
          val rem = frac & 0x1fff
          if (rem > 0x1000 || (rem == 0x1000 && (hfrac & 1) == 1)) {
            hfrac += 1
            if (hfrac == 0x400) {
              hfrac = 0
              hexp += 1
            }
          }
          if (hexp >= 0x1f) sign | 0x7c00
          else sign | ((hexp & 0x1f) << 10) | (hfrac & 0x3ff)
        }
      }
    out.toLong
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

  private def hex(x: Long): String = java.lang.Long.toHexString(x)

  private def mixedRequestsPerProfile: Int = profileName match {
    case "short" => 50
    case "medium" => 100
    case "wide" => 4000
    case _ => 400
  }

  private def fp32NormalAbsRange: Float =
    if (profileName == "wide") 88.72283f - 0.01f else 6.0f

  private def fp16NormalAbsRange: Float =
    if (profileName == "wide") 11.089866f - 0.01f else 5.0f

  private def bf16NormalAbsRange: Float =
    if (profileName == "wide") 88.72283f - 0.01f else 6.0f

  private def nonTrivialLaneMasks(lanes: Int): Seq[Int] = {
    val all = (1 << lanes) - 1
    (1 until all).toSeq
  }

  private case class StreamReq(tag: Int, neg: Boolean, inVec: Seq[Long], laneChecks: Seq[Long => Unit])

  private def runMixedThroughput(
      dut: FPEX,
      lanes: Int,
      requests: Int,
      tagWidth: Int,
      valueMask: Long,
      backpressurePercent: Int,
      seed: Int,
      makeReq: (Random, Boolean, Int) => (Seq[Long], Seq[Long => Unit])
  ): Unit = {
    require(backpressurePercent >= 0 && backpressurePercent < 100, s"invalid backpressurePercent=$backpressurePercent")
    require(requests > 0, "requests must be > 0")
    require(requests <= (1 << tagWidth), s"requests=$requests exceed unique tag space for tagWidth=$tagWidth")
    val rng = new Random(seed)
    val allLanesMask = (1 << lanes) - 1
    val pending = scala.collection.mutable.Map.empty[Int, StreamReq]
    val maxCycles = requests * (if (backpressurePercent == 0) 16 else 64) + 256
    var heldReq: Option[StreamReq] = None
    var sent = 0
    var received = 0
    var cycles = 0

    while ((sent < requests || received < requests) && cycles < maxCycles) {
      if (heldReq.isEmpty && sent < requests) {
        val neg = rng.nextBoolean()
        val (inVec, laneChecks) = makeReq(rng, neg, lanes)
        heldReq = Some(StreamReq(tag = sent, neg = neg, inVec = inVec, laneChecks = laneChecks))
      }

      val respReady = backpressurePercent == 0 || (rng.nextInt(100) >= backpressurePercent)
      dut.io.resp.ready.poke(respReady.B)

      heldReq match {
        case Some(req) =>
          dut.io.req.valid.poke(true.B)
          dut.io.req.bits.roundingMode.poke(0.U)
          dut.io.req.bits.tag.poke(req.tag.U)
          dut.io.req.bits.neg.poke(req.neg.B)
          dut.io.req.bits.laneMask.poke(allLanesMask.U)
          req.inVec.zipWithIndex.foreach { case (v, i) =>
            dut.io.req.bits.xVec(i).poke(v.U)
          }
        case None =>
          dut.io.req.valid.poke(false.B)
      }

      val reqFire = heldReq.nonEmpty && dut.io.req.ready.peek().litToBoolean
      val respFire = dut.io.resp.valid.peek().litToBoolean && respReady

      if (reqFire) {
        val req = heldReq.get
        assert(!pending.contains(req.tag), s"tag ${req.tag} was reissued while still in flight")
        pending(req.tag) = req
        sent += 1
        heldReq = None
      }

      if (respFire) {
        val respTag = dut.io.resp.bits.tag.peek().litValue.toInt
        val respLaneMask = dut.io.resp.bits.laneMask.peek().litValue.toInt & allLanesMask
        assert(respLaneMask == allLanesMask, s"response laneMask mismatch: got=0x${hex(respLaneMask)} exp=0x${hex(allLanesMask)}")
        assert(pending.contains(respTag), s"received unknown response tag $respTag")
        val req = pending.remove(respTag).get
        val outVec = (0 until lanes).map(i => dut.io.resp.bits.result(i).peek().litValue.toLong & valueMask)
        outVec.zipWithIndex.foreach { case (out, i) => req.laneChecks(i)(out) }
        received += 1
      }

      dut.clock.step()
      cycles += 1
    }

    assert(sent == requests, s"sent only $sent/$requests requests within $maxCycles cycles")
    assert(received == requests, s"received only $received/$requests responses within $maxCycles cycles")
    assert(pending.isEmpty, s"${pending.size} requests still in flight at end of run")
  }

  private def makeMixedFP32Req(rng: Random, neg: Boolean, lanes: Int): (Seq[Long], Seq[Long => Unit]) = {
    val nan = 0x7fc00000L
    val posInf = 0x7f800000L
    val negInf = 0xff800000L
    val posZero = 0x00000000L
    val subnormal = 0x00000001L
    val posOverflow = 0x42b17219L
    val negOverflow = 0xc2b17219L
    val one = 0x3f800000L
    val zero = 0x00000000L
    val checks = scala.collection.mutable.ArrayBuffer.empty[Long => Unit]
    val inVec = (0 until lanes).map { _ =>
      if (rng.nextDouble() < 0.5) {
        rng.nextInt(6) match {
          case 0 =>
            checks += ((out: Long) => assert(isNaN(out, 8, 23), s"expected NaN, got 0x${hex(out)}"))
            nan
          case 1 =>
            val expected = if (neg) zero else posInf
            checks += ((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
            posInf
          case 2 =>
            val expected = if (neg) posInf else zero
            checks += ((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
            negInf
          case 3 =>
            checks += ((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
            posZero
          case 4 =>
            checks += ((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
            subnormal
          case _ =>
            checks += ((out: Long) => assert(out == posInf, s"expected +Inf, got 0x${hex(out)}"))
            if (neg) negOverflow else posOverflow
        }
      } else {
        val absRange = fp32NormalAbsRange
        val in = (rng.nextDouble() * (2.0 * absRange) - absRange).toFloat
        val inBits = fp32FloatToBits(in)
        val quantIn = fp32BitsToFloat(inBits).toDouble
        val expectedBits = fp32FloatToBits(math.exp(if (neg) -quantIn else quantIn).toFloat)
        checks += ((out: Long) => {
          val diff = ulpDiff32(out, expectedBits)
          assert(diff <= 6, s"normal-lane FP32 ULP diff $diff > 6 for input $in (neg=$neg)")
        })
        inBits
      }
    }
    (inVec, checks.toSeq)
  }

  private def makeMixedFP16Req(rng: Random, neg: Boolean, lanes: Int): (Seq[Long], Seq[Long => Unit]) = {
    val nan = 0x7e00L
    val posInf = 0x7c00L
    val negInf = 0xfc00L
    val posZero = 0x0000L
    val subnormal = 0x0001L
    val posOverflow = 0x498dL
    val negOverflow = 0xc98dL
    val one = 0x3c00L
    val zero = 0x0000L
    val checks = scala.collection.mutable.ArrayBuffer.empty[Long => Unit]
    val inVec = (0 until lanes).map { _ =>
      if (rng.nextDouble() < 0.5) {
        rng.nextInt(6) match {
          case 0 =>
            checks += ((out: Long) => assert(isNaN(out, 5, 10), s"expected NaN, got 0x${hex(out)}"))
            nan
          case 1 =>
            val expected = if (neg) zero else posInf
            checks += ((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
            posInf
          case 2 =>
            val expected = if (neg) posInf else zero
            checks += ((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
            negInf
          case 3 =>
            checks += ((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
            posZero
          case 4 =>
            checks += ((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
            subnormal
          case _ =>
            checks += ((out: Long) => assert(out == posInf, s"expected +Inf, got 0x${hex(out)}"))
            if (neg) negOverflow else posOverflow
        }
      } else {
        val absRange = fp16NormalAbsRange
        val in = (rng.nextDouble() * (2.0 * absRange) - absRange).toFloat
        val inBits = floatToFp16Bits(in)
        val quantIn = fp16BitsToFloat(inBits).toDouble
        val expectedBits = floatToFp16Bits(math.exp(if (neg) -quantIn else quantIn).toFloat)
        checks += ((out: Long) => {
          val diff = ulpDiff16(out, expectedBits)
          assert(diff <= 2, s"normal-lane FP16 ULP diff $diff > 2 for input $in (neg=$neg)")
        })
        inBits
      }
    }
    (inVec, checks.toSeq)
  }

  private def makeMixedBF16Req(rng: Random, neg: Boolean, lanes: Int): (Seq[Long], Seq[Long => Unit]) = {
    val nan = 0x7fc0L
    val posInf = 0x7f80L
    val negInf = 0xff80L
    val posZero = 0x0000L
    val subnormal = 0x0001L
    val posOverflow = 0x42b2L
    val negOverflow = 0xc2b2L
    val one = 0x3f80L
    val zero = 0x0000L
    val checks = scala.collection.mutable.ArrayBuffer.empty[Long => Unit]
    val inVec = (0 until lanes).map { _ =>
      if (rng.nextDouble() < 0.5) {
        rng.nextInt(6) match {
          case 0 =>
            checks += ((out: Long) => assert(isNaN(out, 8, 7), s"expected NaN, got 0x${hex(out)}"))
            nan
          case 1 =>
            val expected = if (neg) zero else posInf
            checks += ((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
            posInf
          case 2 =>
            val expected = if (neg) posInf else zero
            checks += ((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
            negInf
          case 3 =>
            checks += ((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
            posZero
          case 4 =>
            checks += ((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
            subnormal
          case _ =>
            checks += ((out: Long) => assert(out == posInf, s"expected +Inf, got 0x${hex(out)}"))
            if (neg) negOverflow else posOverflow
        }
      } else {
        val absRange = bf16NormalAbsRange
        val in = (rng.nextDouble() * (2.0 * absRange) - absRange).toFloat
        val inBits = floatToBf16Bits(in)
        val quantIn = bf16BitsToFloat(inBits).toDouble
        val expectedBits = floatToBf16Bits(math.exp(if (neg) -quantIn else quantIn).toFloat)
        checks += ((out: Long) => {
          val diff = ulpDiff16(out, expectedBits)
          assert(diff <= 2, s"normal-lane BF16 ULP diff $diff > 2 for input $in (neg=$neg)")
        })
        inBits
      }
    }
    (inVec, checks.toSeq)
  }


  if (enableFP32Tests) {
    it should "keep response invalid until the first pipelined result is due after reset" in {
      test(new FPEX(FPType.FP32T, numLanes = 1)) { dut =>
      val lanes = 1
      val expectedLatency = 6
      initializeInterface(dut, lanes, drainCycles = 0)

      for (i <- 0 until 2) {
        assert(!dut.io.resp.valid.peek().litToBoolean, s"resp.valid should be low while idle (cycle $i)")
        dut.clock.step()
      }

      val inputs = Seq(0x3f800000L, 0xbf800000L, 0x40000000L)
      val expected = inputs.map { inBits =>
        val quantIn = fp32BitsToFloat(inBits).toDouble
        fp32FloatToBits(math.exp(quantIn).toFloat)
      }

      dut.io.resp.ready.poke(true.B)
      dut.io.req.bits.neg.poke(false.B)
      dut.io.req.bits.roundingMode.poke(0.U)
      dut.io.req.bits.tag.poke(0.U)
      dut.io.req.bits.laneMask.poke(1.U)

      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.xVec(0).poke(inputs.head.U)
      assert(dut.io.req.ready.peek().litToBoolean, "req.ready should be high for the first request")
      dut.clock.step()

      for (fill <- 0 until (expectedLatency - 1)) {
        if (fill + 1 < inputs.length) {
          dut.io.req.valid.poke(true.B)
          dut.io.req.bits.xVec(0).poke(inputs(fill + 1).U)
          assert(dut.io.req.ready.peek().litToBoolean, s"req.ready dropped during fill cycle $fill")
        } else {
          dut.io.req.valid.poke(false.B)
        }
        assert(!dut.io.resp.valid.peek().litToBoolean, s"resp.valid asserted too early at fill cycle $fill")
        dut.clock.step()
      }
      dut.io.req.valid.poke(false.B)

      expected.zipWithIndex.foreach { case (expBits, i) =>
        assert(dut.io.resp.valid.peek().litToBoolean, s"missing response $i when expected")
        val gotBits = dut.io.resp.bits.result(0).peek().litValue.toLong & 0xffffffffL
        val diff = ulpDiff32(gotBits, expBits)
        assert(diff <= 6, s"response $i mismatch: got=0x${hex(gotBits)} expected=0x${hex(expBits)} diff=$diff")
        dut.clock.step()
      }

      assert(!dut.io.resp.valid.peek().litToBoolean, "resp.valid remained high after draining all queued responses")
      }
    }
  }

  if (enableFP32Tests) {
    it should "handle special cases (FP32)" in {
      test(new FPEX(FPType.FP32T, numLanes = 4)) { dut =>
      val lanes = 4
      initializeInterface(dut, lanes)

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
  }

  if (enableFP16Tests) {
    it should "handle special cases (FP16)" in {
      test(new FPEX(FPType.FP16T, numLanes = 4)) { dut =>
      val lanes = 4
      initializeInterface(dut, lanes)

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
  }

  if (enableBF16Tests) {
    it should "handle special cases (BF16)" in {
      test(new FPEX(FPType.BF16T, numLanes = 4)) { dut =>
      val lanes = 4
      initializeInterface(dut, lanes)

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

  if (enableFP32Tests) {
    it should "approximate exp for normal FP32 values" in {
      test(new FPEX(FPType.FP32T, numLanes = approxLanes)) { dut =>
      val lanes = approxLanes
      initializeInterface(dut, lanes)
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
  }


  if (enableFP16Tests) {
    it should "approximate exp for normal FP16 values" in {
      test(new FPEX(FPType.FP16T, numLanes = approxLanes)) { dut =>
      val lanes = approxLanes
      initializeInterface(dut, lanes)
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
  }

  if (enableBF16Tests) {
    it should "approximate exp for normal BF16 values" in {
      test(new FPEX(FPType.BF16T, numLanes = approxLanes)) { dut =>
      val lanes = approxLanes
      initializeInterface(dut, lanes)
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

  if (enableFP32Tests) {
    it should "handle mixed special and normal lane inputs under random bombardment (FP32, full throughput, random backpressure)" in {
      test(new FPEX(FPType.FP32T, numLanes = 4, tagWidth = 16)) { dut =>
        val lanes = 4
        initializeInterface(dut, lanes)
        runMixedThroughput(
          dut = dut,
          lanes = lanes,
          requests = mixedRequestsPerProfile,
          tagWidth = 16,
          valueMask = 0xffffffffL,
          backpressurePercent = 35,
          seed = 101,
          makeReq = makeMixedFP32Req
        )
      }
    }

    it should "handle mixed special and normal lane inputs under random bombardment (FP32, full throughput, no backpressure)" in {
      test(new FPEX(FPType.FP32T, numLanes = 4, tagWidth = 16)) { dut =>
        val lanes = 4
        initializeInterface(dut, lanes)
        runMixedThroughput(
          dut = dut,
          lanes = lanes,
          requests = mixedRequestsPerProfile,
          tagWidth = 16,
          valueMask = 0xffffffffL,
          backpressurePercent = 0,
          seed = 1101,
          makeReq = makeMixedFP32Req
        )
      }
    }
  }

  if (enableFP16Tests) {
    it should "handle mixed special and normal lane inputs under random bombardment (FP16, full throughput, random backpressure)" in {
      test(new FPEX(FPType.FP16T, numLanes = 4, tagWidth = 16)) { dut =>
        val lanes = 4
        initializeInterface(dut, lanes)
        runMixedThroughput(
          dut = dut,
          lanes = lanes,
          requests = mixedRequestsPerProfile,
          tagWidth = 16,
          valueMask = 0xffffL,
          backpressurePercent = 35,
          seed = 102,
          makeReq = makeMixedFP16Req
        )
      }
    }

    it should "handle mixed special and normal lane inputs under random bombardment (FP16, full throughput, no backpressure)" in {
      test(new FPEX(FPType.FP16T, numLanes = 4, tagWidth = 16)) { dut =>
        val lanes = 4
        initializeInterface(dut, lanes)
        runMixedThroughput(
          dut = dut,
          lanes = lanes,
          requests = mixedRequestsPerProfile,
          tagWidth = 16,
          valueMask = 0xffffL,
          backpressurePercent = 0,
          seed = 1102,
          makeReq = makeMixedFP16Req
        )
      }
    }
  }

  if (enableBF16Tests) {
    it should "handle mixed special and normal lane inputs under random bombardment (BF16, full throughput, random backpressure)" in {
      test(new FPEX(FPType.BF16T, numLanes = 4, tagWidth = 16)) { dut =>
        val lanes = 4
        initializeInterface(dut, lanes)
        runMixedThroughput(
          dut = dut,
          lanes = lanes,
          requests = mixedRequestsPerProfile,
          tagWidth = 16,
          valueMask = 0xffffL,
          backpressurePercent = 35,
          seed = 103,
          makeReq = makeMixedBF16Req
        )
      }
    }

    it should "handle mixed special and normal lane inputs under random bombardment (BF16, full throughput, no backpressure)" in {
      test(new FPEX(FPType.BF16T, numLanes = 4, tagWidth = 16)) { dut =>
        val lanes = 4
        initializeInterface(dut, lanes)
        runMixedThroughput(
          dut = dut,
          lanes = lanes,
          requests = mixedRequestsPerProfile,
          tagWidth = 16,
          valueMask = 0xffffL,
          backpressurePercent = 0,
          seed = 1103,
          makeReq = makeMixedBF16Req
        )
      }
    }
  }

  if (enableFP32Tests) {
    it should "handle partial lane masks under random mixed inputs (FP32)" in {
      test(new FPEX(FPType.FP32T, numLanes = 4)) { dut =>
      val lanes = 4
      initializeInterface(dut, lanes)
      val rng = new Random(104)
      val masks = nonTrivialLaneMasks(lanes)
      val nan = 0x7fc00000L
      val posInf = 0x7f800000L
      val negInf = 0xff800000L
      val posZero = 0x00000000L
      val subnormal = 0x00000001L
      val posOverflow = 0x42b17219L
      val negOverflow = 0xc2b17219L
      val one = 0x3f800000L
      val zero = 0x00000000L

      for (_ <- 0 until mixedRequestsPerProfile) {
        val neg = rng.nextBoolean()
        val laneMask = masks(rng.nextInt(masks.size))
        val laneChecks = Array.fill[Option[Long => Unit]](lanes)(None)
        val inVec = (0 until lanes).map { lane =>
          val active = ((laneMask >> lane) & 1) == 1
          if (rng.nextDouble() < 0.5) {
            rng.nextInt(6) match {
              case 0 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(isNaN(out, 8, 23), s"expected NaN, got 0x${hex(out)}"))
                nan
              case 1 =>
                val expected = if (neg) zero else posInf
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
                posInf
              case 2 =>
                val expected = if (neg) posInf else zero
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
                negInf
              case 3 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
                posZero
              case 4 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
                subnormal
              case _ =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == posInf, s"expected +Inf, got 0x${hex(out)}"))
                if (neg) negOverflow else posOverflow
            }
          } else {
            val in = (rng.nextDouble() * 12.0 - 6.0).toFloat
            val inBits = fp32FloatToBits(in)
            if (active) {
              val quantIn = fp32BitsToFloat(inBits).toDouble
              val expectedBits = fp32FloatToBits(math.exp(if (neg) -quantIn else quantIn).toFloat)
              laneChecks(lane) = Some((out: Long) => {
                val diff = ulpDiff32(out, expectedBits)
                assert(diff <= 6, s"normal-lane FP32 ULP diff $diff > 6 for input $in (neg=$neg, laneMask=0x${hex(laneMask)})")
              })
            }
            inBits
          }
        }
        val (outMask, out) = driveAndAwaitVecMasked(dut, inVec, laneMask, neg = neg)
        assert((outMask.toInt & ((1 << lanes) - 1)) == laneMask, s"laneMask mismatch: got=0x${outMask.toString(16)} exp=0x${hex(laneMask)}")
        out.zipWithIndex.foreach { case (v, lane) =>
          laneChecks(lane).foreach(check => check(v.toLong & 0xffffffffL))
        }
      }
      }
    }
  }

  if (enableFP16Tests) {
    it should "handle partial lane masks under random mixed inputs (FP16)" in {
      test(new FPEX(FPType.FP16T, numLanes = 4)) { dut =>
      val lanes = 4
      initializeInterface(dut, lanes)
      val rng = new Random(105)
      val masks = nonTrivialLaneMasks(lanes)
      val nan = 0x7e00L
      val posInf = 0x7c00L
      val negInf = 0xfc00L
      val posZero = 0x0000L
      val subnormal = 0x0001L
      val posOverflow = 0x498dL
      val negOverflow = 0xc98dL
      val one = 0x3c00L
      val zero = 0x0000L

      for (_ <- 0 until mixedRequestsPerProfile) {
        val neg = rng.nextBoolean()
        val laneMask = masks(rng.nextInt(masks.size))
        val laneChecks = Array.fill[Option[Long => Unit]](lanes)(None)
        val inVec = (0 until lanes).map { lane =>
          val active = ((laneMask >> lane) & 1) == 1
          if (rng.nextDouble() < 0.5) {
            rng.nextInt(6) match {
              case 0 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(isNaN(out, 5, 10), s"expected NaN, got 0x${hex(out)}"))
                nan
              case 1 =>
                val expected = if (neg) zero else posInf
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
                posInf
              case 2 =>
                val expected = if (neg) posInf else zero
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
                negInf
              case 3 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
                posZero
              case 4 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
                subnormal
              case _ =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == posInf, s"expected +Inf, got 0x${hex(out)}"))
                if (neg) negOverflow else posOverflow
            }
          } else {
            val in = (rng.nextDouble() * 10.0 - 5.0).toFloat
            val inBits = floatToFp16Bits(in)
            if (active) {
              val quantIn = fp16BitsToFloat(inBits).toDouble
              val expectedBits = floatToFp16Bits(math.exp(if (neg) -quantIn else quantIn).toFloat)
              laneChecks(lane) = Some((out: Long) => {
                val diff = ulpDiff16(out, expectedBits)
                assert(diff <= 2, s"normal-lane FP16 ULP diff $diff > 2 for input $in (neg=$neg, laneMask=0x${hex(laneMask)})")
              })
            }
            inBits
          }
        }
        val (outMask, out) = driveAndAwaitVecMasked(dut, inVec, laneMask, neg = neg)
        assert((outMask.toInt & ((1 << lanes) - 1)) == laneMask, s"laneMask mismatch: got=0x${outMask.toString(16)} exp=0x${hex(laneMask)}")
        out.zipWithIndex.foreach { case (v, lane) =>
          laneChecks(lane).foreach(check => check(v.toLong & 0xffffL))
        }
      }
      }
    }
  }

  if (enableBF16Tests) {
    it should "handle partial lane masks under random mixed inputs (BF16)" in {
      test(new FPEX(FPType.BF16T, numLanes = 4)) { dut =>
      val lanes = 4
      initializeInterface(dut, lanes)
      val rng = new Random(106)
      val masks = nonTrivialLaneMasks(lanes)
      val nan = 0x7fc0L
      val posInf = 0x7f80L
      val negInf = 0xff80L
      val posZero = 0x0000L
      val subnormal = 0x0001L
      val posOverflow = 0x42b2L
      val negOverflow = 0xc2b2L
      val one = 0x3f80L
      val zero = 0x0000L

      for (_ <- 0 until mixedRequestsPerProfile) {
        val neg = rng.nextBoolean()
        val laneMask = masks(rng.nextInt(masks.size))
        val laneChecks = Array.fill[Option[Long => Unit]](lanes)(None)
        val inVec = (0 until lanes).map { lane =>
          val active = ((laneMask >> lane) & 1) == 1
          if (rng.nextDouble() < 0.5) {
            rng.nextInt(6) match {
              case 0 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(isNaN(out, 8, 7), s"expected NaN, got 0x${hex(out)}"))
                nan
              case 1 =>
                val expected = if (neg) zero else posInf
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
                posInf
              case 2 =>
                val expected = if (neg) posInf else zero
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == expected, s"expected 0x${hex(expected)}, got 0x${hex(out)}"))
                negInf
              case 3 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
                posZero
              case 4 =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == one, s"expected 1.0, got 0x${hex(out)}"))
                subnormal
              case _ =>
                if (active) laneChecks(lane) = Some((out: Long) => assert(out == posInf, s"expected +Inf, got 0x${hex(out)}"))
                if (neg) negOverflow else posOverflow
            }
          } else {
            val in = (rng.nextDouble() * 12.0 - 6.0).toFloat
            val inBits = floatToBf16Bits(in)
            if (active) {
              val quantIn = bf16BitsToFloat(inBits).toDouble
              val expectedBits = floatToBf16Bits(math.exp(if (neg) -quantIn else quantIn).toFloat)
              laneChecks(lane) = Some((out: Long) => {
                val diff = ulpDiff16(out, expectedBits)
                assert(diff <= 2, s"normal-lane BF16 ULP diff $diff > 2 for input $in (neg=$neg, laneMask=0x${hex(laneMask)})")
              })
            }
            inBits
          }
        }
        val (outMask, out) = driveAndAwaitVecMasked(dut, inVec, laneMask, neg = neg)
        assert((outMask.toInt & ((1 << lanes) - 1)) == laneMask, s"laneMask mismatch: got=0x${outMask.toString(16)} exp=0x${hex(laneMask)}")
        out.zipWithIndex.foreach { case (v, lane) =>
          laneChecks(lane).foreach(check => check(v.toLong & 0xffffL))
        }
      }
      }
    }
  }
}

class FPEXShortSpec extends FPEXSpecBase {
  override protected def profileName: String = "short"
  override protected def randomCount: Int = 50
  override protected def gridCount: Int = 11
  override protected def approxLanes: Int = 1
}

class FPEXShortSpecFP32 extends FPEXShortSpec {
  override protected def enableFP16Tests: Boolean = false
  override protected def enableBF16Tests: Boolean = false
}

class FPEXShortSpecFP16 extends FPEXShortSpec {
  override protected def enableFP32Tests: Boolean = false
  override protected def enableBF16Tests: Boolean = false
}

class FPEXShortSpecBF16 extends FPEXShortSpec {
  override protected def enableFP32Tests: Boolean = false
  override protected def enableFP16Tests: Boolean = false
}

class FPEXMediumSpec extends FPEXSpecBase {
  override protected def profileName: String = "medium"
  override protected def randomCount: Int = 100
  override protected def gridCount: Int = 25
  override protected def approxLanes: Int = 1
}

class FPEXMediumSpecFP32 extends FPEXMediumSpec {
  override protected def enableFP16Tests: Boolean = false
  override protected def enableBF16Tests: Boolean = false
}

class FPEXMediumSpecFP16 extends FPEXMediumSpec {
  override protected def enableFP32Tests: Boolean = false
  override protected def enableBF16Tests: Boolean = false
}

class FPEXMediumSpecBF16 extends FPEXMediumSpec {
  override protected def enableFP32Tests: Boolean = false
  override protected def enableFP16Tests: Boolean = false
}

class FPEXLongSpec extends FPEXSpecBase {
  override protected def profileName: String = "long"
  override protected def randomCount: Int = 400
  override protected def gridCount: Int = 65
  override protected def approxLanes: Int = 1
}

class FPEXWideSpec extends FPEXSpecBase {
  override protected def profileName: String = "wide"
  override protected def randomCount: Int = 4000
  override protected def gridCount: Int = 651
  override protected def approxLanes: Int = 1
}

class FPEXLongSpecFP32 extends FPEXLongSpec {
  override protected def enableFP16Tests: Boolean = false
  override protected def enableBF16Tests: Boolean = false
}

class FPEXLongSpecFP16 extends FPEXLongSpec {
  override protected def enableFP32Tests: Boolean = false
  override protected def enableBF16Tests: Boolean = false
}

class FPEXLongSpecBF16 extends FPEXLongSpec {
  override protected def enableFP32Tests: Boolean = false
  override protected def enableFP16Tests: Boolean = false
}

class FPEXWideSpecFP32 extends FPEXWideSpec {
  override protected def enableFP16Tests: Boolean = false
  override protected def enableBF16Tests: Boolean = false
}

class FPEXWideSpecFP16 extends FPEXWideSpec {
  override protected def enableFP32Tests: Boolean = false
  override protected def enableBF16Tests: Boolean = false
}

class FPEXWideSpecBF16 extends FPEXWideSpec {
  override protected def enableFP32Tests: Boolean = false
  override protected def enableFP16Tests: Boolean = false
}

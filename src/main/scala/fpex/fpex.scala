package fpex

import chisel3._
import chisel3.util._
import hardfloat._

trait HasFPEXParams {
  def numFP16Lanes = 4
  def tagWidth = 8
}

object FPEXState extends ChiselEnum {
  val READY = 0.U
  val BUSY = 1.U
  val DONE = 2.U
}

class FPEXReq(wordWidth: Int, numLanes: Int, tagWidth: Int) extends Bundle {
  val roundingMode = UInt(3.W)
  val tag = UInt(tagWidth.W)
  val neg = Bool() // 0 = e^x, 1 = e^-x
  val laneMask = UInt(numLanes.W)
  val xVec = Vec(numLanes, UInt(wordWidth.W))
}

class FPEXResp(wordWidth: Int, numLanes: Int, tagWidth: Int) extends Bundle {
  val tag = UInt(tagWidth.W)
  val laneMask = UInt(numLanes.W)
  val result = Vec(numLanes, UInt(wordWidth.W))
}

class FPEX(fpT: FPType, numLanes: Int = 4, tagWidth: Int = 1)
  extends Module
  with HasFPEXParams {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new FPEXReq(fpT.wordWidth, numLanes, tagWidth)))
    val resp = Decoupled(new FPEXResp(fpT.wordWidth, numLanes, tagWidth))
  })

  def maskLaneNext[T <: Data](bitsVec: Vec[T], laneMask: Vec[Bool]): Vec[T] = {
    require(laneMask.length == bitsVec.length)
    VecInit(bitsVec.zip(laneMask).map{case (bits, laneEnable) => RegEnable(bits, laneEnable)})
  }

  val state = RegInit(FPEXState.READY)
  val res = Reg(Vec(numLanes, UInt(fpT.wordWidth.W)))
  val lut = Module(new ExLUT(numLanes, fpT.lutAddrBits, fpT.lutValM, fpT.lutValN))
  val roundToRecFn = Seq.fill(numLanes)(Module(new RoundRawFNToRecFN(fpT.expWidth, fpT.sigWidth, 0)))

  val laneEnable = VecInit((io.req.bits.laneMask & VecInit.fill(numLanes)(io.req.fire).asUInt).asBools)
  val maskedXVec = io.req.bits.xVec.zip(laneEnable).map {
    case (x, en) => Mux (en, x, 0.U(fpT.wordWidth.W))
  }
  val maskedNeg = io.req.bits.neg && io.req.fire

  //stage 0: special case check and raw float decomposition
  //flush subnormals to zero, ignore x for which e^x = inf
  val rawFloatVec = VecInit(maskedXVec.map(
    x => rawFloatFromFN(fpT.expWidth, fpT.sigWidth, x).negate(maskedNeg)))
  val expFPOverflow = VecInit(maskedXVec.map(x => fpT.expFPIsInf(x, maskedNeg)))
  val rawFloatOf = rawFloatVec.zip(expFPOverflow)
  val earlyRes = VecInit(rawFloatOf.map { case (x, of) =>
    MuxCase(
      0.U(fpT.wordWidth.W),
      Seq(
        x.isNaN -> Cat(x.sign, fpT.nanExp, isSigNaNRawFloat(x), fpT.nanSig),
        (x.isZero || x.isSubNorm) -> fpT.one,
        (x.isInf && x.sign) -> fpT.zero,
        ((x.isInf && !x.sign) || of) -> Cat(0.U(1.W), fpT.infinity)
      )
    )
  })
  val earlyTerminate = VecInit(rawFloatOf.map { case (x, of) =>
    x.isInf || x.isZero || x.isSubNorm || x.isNaN || of
  })

  //stage 1: convert to Qmn
  val stage1Req = RegEnable(io.req.bits, io.req.fire)
  val stage1Valid = RegNext(io.req.fire)
  val stage1LaneEnable = RegNext(laneEnable)
  val stage1EarlyTerminate = maskLaneNext(earlyTerminate, laneEnable)
  val stage1EarlyRes = maskLaneNext(earlyRes, laneEnable)
  val stage1RawFloatVec = maskLaneNext(rawFloatVec, laneEnable)
  val stage1Qmn = VecInit(stage1RawFloatVec.map(x => fpT.qmnFromRawFloat(x)))

  //stage 2: multiply x/ln2, extract k and r, init lut read r[top]
  val stage2Req = RegEnable(stage1Req, stage1Valid)
  val stage2Valid = RegNext(stage1Valid)
  val stage2LaneEnable = RegNext(stage1LaneEnable)
  val stage2EarlyTerminate = maskLaneNext(stage1EarlyTerminate, stage1LaneEnable)
  val stage2EarlyRes = maskLaneNext(stage1EarlyRes, stage1LaneEnable)
  val stage2Qmn = maskLaneNext(stage1Qmn, stage1LaneEnable)
  val xrln2KRVec = stage2Qmn.map(_.mul(fpT.rln2).getKR).unzip
  val stage2kVec = VecInit(xrln2KRVec._1)
  val stage2rVec = VecInit(xrln2KRVec._2)

  //stage 3: lut ready r[top], init lut read r[top] + 1
  val stage3Req = RegEnable(stage2Req, stage2Valid)
  val rLowBits = fpT.qmnN - fpT.lutAddrBits
  val stage3Valid = RegNext(stage2Valid)
  val stage3LaneEnable = RegNext(stage2LaneEnable)
  val stage3EarlyTerminate = maskLaneNext(stage2EarlyTerminate, stage2LaneEnable)
  val stage3EarlyRes = maskLaneNext(stage2EarlyRes, stage2LaneEnable)
  val stage3kVec = maskLaneNext(stage2kVec, stage2LaneEnable)
  val stage3rVec = maskLaneNext(stage2rVec, stage2LaneEnable)
  val stage3AddrVec = VecInit(stage3rVec.map(r => r(fpT.qmnN - 1, rLowBits)))
  val rLowerVec = VecInit(stage3rVec.map(r => r(rLowBits - 1, 0)))

  //stage 4: lut ready r[top] + 1, interpolate
  val stage4Req = RegEnable(stage3Req, stage3Valid)
  val stage4Valid = RegNext(stage3Valid)
  val stage4LaneEnable = RegNext(stage3LaneEnable)
  val stage4EarlyTerminate = maskLaneNext(stage3EarlyTerminate, stage3LaneEnable)
  val stage4EarlyRes = maskLaneNext(stage3EarlyRes, stage3LaneEnable)
  val stage4kVec = maskLaneNext(stage3kVec, stage3LaneEnable)
  val stage4rLowerVec = maskLaneNext(rLowerVec, stage3LaneEnable)
  val stage4AddrVec = maskLaneNext(stage3AddrVec, stage3LaneEnable)
  val y0 = RegNext(lut.io.rdata(0))
  val y1 = lut.io.rdata(1)
  val lutTopEndpoint = ((BigInt(1) << (fpT.lutValM + fpT.lutValN)) - 1).U((fpT.lutValM + fpT.lutValN).W)
  val pow2r = VecInit(y0.zip(y1).zip(stage4rLowerVec).zip(stage4AddrVec).map {
    case (((y0, y1), frac), addr) =>
      val y1Interp = Mux(addr === ((1 << fpT.lutAddrBits) - 1).U, lutTopEndpoint, y1)
      val delta = y1Interp - y0
      val interp = y0 + ((delta * frac) >> rLowBits)
      interp
  })

  //stage 5: convert and return result
  val stage5Req = RegEnable(stage4Req, stage4Valid)
  val stage5Valid = RegNext(stage4Valid)
  val stage5LaneEnable = RegNext(stage4LaneEnable)
  val stage5EarlyTerminate = maskLaneNext(stage4EarlyTerminate, stage4LaneEnable)
  val stage5EarlyRes = maskLaneNext(stage4EarlyRes, stage4LaneEnable)
  val stage5pow2rVec = maskLaneNext(pow2r, stage4LaneEnable)
  val stage5kVec = maskLaneNext(stage4kVec, stage4LaneEnable)
  val resRawFloat = stage5pow2rVec.zip(stage5kVec).map{ case (qmn, k) => fpT.rawFloatFromQmnK(qmn, k) }
  roundToRecFn.zip(resRawFloat).zip(stage5LaneEnable).foreach {
    case ((round, rawFloat), en) => {
      round.io.invalidExc := false.B
      round.io.infiniteExc := false.B
      round.io.in := rawFloat
      round.io.roundingMode := Mux(en, stage5Req.roundingMode, 0.U)
      round.io.detectTininess := 0.U
    }
  }
  val resReq = RegNext(stage5Req)
  val resFN = roundToRecFn.map(round => fNFromRecFN(fpT.expWidth, fpT.sigWidth, round.io.out))
  val resFinal = VecInit(resFN.zip(stage5EarlyTerminate.zip(stage5EarlyRes)).map {
    case (res, (earlyTerminate, earlyRes)) => Mux(earlyTerminate, earlyRes, res)
  })
  val resNext = VecInit(res.zip(resFinal).zip(stage5LaneEnable).map {
    case ((cur, nxt), en) => Mux(en && stage5Valid, nxt, cur)
  })
  res := resNext

  lut.io.raddrs(0) := stage2rVec.map(r => r(fpT.qmnN - 1, rLowBits))
  lut.io.raddrs(1) := stage3rVec.map { r =>
    val maxAddr = ((1 << fpT.lutAddrBits) - 1).U
    val addr = r(fpT.qmnN - 1, rLowBits)
    Mux(addr === maxAddr, addr, addr + 1.U)
  }

  io.req.ready := state === FPEXState.READY
  io.resp.valid := state === FPEXState.DONE
  io.resp.bits.tag := resReq.tag
  io.resp.bits.result := res
  io.resp.bits.laneMask := resReq.laneMask

  when (io.req.fire) {
    state := FPEXState.BUSY
  }.elsewhen (io.resp.fire) {
    state := FPEXState.READY
  }.elsewhen(stage5Valid) {
    state := FPEXState.DONE
  }
}

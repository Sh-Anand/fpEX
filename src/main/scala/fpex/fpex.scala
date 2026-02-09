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

  def maskLane[T <: Data](bitsVec: Vec[T], laneMask: UInt): Vec[T] = {
    require(laneMask.getWidth == bitsVec.length)
    VecInit(bitsVec.zip(laneMask.asBools).map{case (bits, laneEnable) => RegEnable(bits, laneEnable)})
  }

  val state = RegInit(FPEXState.READY)
  val res = Reg(Vec(numLanes, UInt(fpT.wordWidth.W)))
  val lut = Module(new ExLUT(numLanes, fpT.lutAddrBits, fpT.lutValM, fpT.lutValN))
  val roundToRecFn = Seq.fill(numLanes)(Module(new RoundRawFNToRecFN(fpT.expWidth, fpT.sigWidth, 0)))
  val busy = state === FPEXState.BUSY

  //stage 0: special case check and raw float decomposition
  //flush subnormals to zero, ignore x for which e^x = inf
  val laneEnable = io.req.bits.laneMask & VecInit.fill(numLanes)(io.req.fire).asUInt
  val rawFloatVec = VecInit(io.req.bits.xVec.map(
    x => rawFloatFromFN(fpT.expWidth, fpT.sigWidth, x).negate(io.req.bits.neg)))
  val expFPOverflow = VecInit(io.req.bits.xVec.map(x => fpT.expFPIsInf(x, io.req.bits.neg)))
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
  val stage1Req = RegNext(io.req.bits)
  val stage1Valid = RegNext(io.req.fire)
  val stage1EarlyTerminate = RegNext(earlyTerminate)
  val stage1EarlyRes = RegNext(earlyRes)
  val stage1RawFloatVec = RegEnable(rawFloatVec, io.req.fire)
  val stage1Qmn = VecInit(stage1RawFloatVec.map(x => fpT.qmnFromRawFloat(x)))

  //stage 2: multiply x/ln2, extract k and r, init lut read r[top]
  val stage2Req = RegNext(stage1Req)
  val stage2Valid = RegNext(stage1Valid)
  val stage2EarlyTerminate = RegNext(stage1EarlyTerminate)
  val stage2EarlyRes = RegNext(stage1EarlyRes)
  val stage2Qmn = RegEnable(stage1Qmn, stage1Valid)
  val xrln2KRVec = stage2Qmn.map(_.mul(fpT.rln2).getKR).unzip
  val stage2kVec = VecInit(xrln2KRVec._1)
  val stage2rVec = VecInit(xrln2KRVec._2)

  //stage 3: lut ready r[top], init lut read r[top] + 1
  val stage3Req = RegNext(stage2Req)
  val rLowBits = fpT.qmnN - fpT.lutAddrBits
  val stage3Valid = RegNext(stage2Valid)
  val stage3EarlyTerminate = RegNext(stage2EarlyTerminate)
  val stage3EarlyRes = RegNext(stage2EarlyRes)
  val stage3kVec = RegNext(stage2kVec)
  val stage3rVec = RegNext(stage2rVec)
  val stage3AddrVec = VecInit(stage3rVec.map(r => r(fpT.qmnN - 1, rLowBits)))
  val rLowerVec = VecInit(stage3rVec.map(r => r(rLowBits - 1, 0)))

  //stage 4: lut ready r[top] + 1, interpolate
  val stage4Req = RegNext(stage3Req)
  val stage4Valid = RegNext(stage3Valid)
  val stage4EarlyTerminate = RegNext(stage3EarlyTerminate)
  val stage4EarlyRes = RegNext(stage3EarlyRes)
  val stage4kVec = RegNext(stage3kVec)
  val stage4rLowerVec = RegNext(rLowerVec)
  val stage4AddrVec = RegNext(stage3AddrVec)
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
  val stage5Req = RegNext(stage4Req)
  val stage5Valid = RegNext(stage4Valid)
  val stage5EarlyTerminate = RegNext(stage4EarlyTerminate)
  val stage5EarlyRes = RegNext(stage4EarlyRes)
  val stage5pow2rVec = RegNext(pow2r)
  val stage5kVec = RegNext(stage4kVec)
  val resRawFloat = stage5pow2rVec.zip(stage5kVec).map{ case (qmn, k) => fpT.rawFloatFromQmnK(qmn, k) }
  roundToRecFn.zip(resRawFloat).foreach {
    case (round, rawFloat) => {
      round.io.invalidExc := false.B
      round.io.infiniteExc := false.B
      round.io.in := rawFloat
      round.io.roundingMode := stage5Req.roundingMode
      round.io.detectTininess := 0.U
    }
  }
  val resReq = RegNext(stage5Req)
  val resFN = roundToRecFn.map(round => fNFromRecFN(fpT.expWidth, fpT.sigWidth, round.io.out))
  val resFinal = VecInit(resFN.zip(stage5EarlyTerminate.zip(stage5EarlyRes)).map { case (res, (earlyTerminate, earlyRes)) =>
    Mux(earlyTerminate, earlyRes, res)
  })
  res := Mux(stage5Valid, resFinal, res)

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

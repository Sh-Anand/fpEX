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

  val state = RegInit(FPEXState.READY)
  val req = Reg(new FPEXReq(fpT.wordWidth, numLanes, tagWidth))
  val res = Reg(Vec(numLanes, UInt(fpT.wordWidth.W)))
  val lut = Module(new ExLUT(numLanes, fpT.lutAddrBits, fpT.lutValM, fpT.lutValN))
  val roundToRecFn = Seq.fill(numLanes)(Module(new RoundRawFNToRecFN(fpT.expWidth, fpT.sigWidth, 0)))
  val busy = state === FPEXState.BUSY

  //stage 0: special case check and raw float decomposition
  //flush subnormals to zero, ignore x for which e^x = inf
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
  val earlyValid = VecInit(rawFloatOf.map { case (x, of) =>
    x.isInf || x.isZero || x.isSubNorm || x.isNaN || of
  })
  val earlyTerminate = earlyValid.asUInt.andR

  //stage 1: convert to Qmn
  val stage1Valid = RegNext(io.req.fire && !earlyTerminate)
  val stage1RawFloatVec = RegEnable(rawFloatVec, io.req.fire && !earlyTerminate)
  val stage1Qmn = VecInit(stage1RawFloatVec.map(x => fpT.qmnFromRawFloat(x)))

  //stage 2: multiply x/ln2, extract k and r, init lut read r[top]
  val stage2Valid = RegNext(stage1Valid)
  val stage2Qmn = RegEnable(stage1Qmn, stage1Valid)
  val xrln2KRVec = stage2Qmn.map(_.mul(fpT.rln2).getKR).unzip
  val stage2kVec = VecInit(xrln2KRVec._1)
  val stage2rVec = VecInit(xrln2KRVec._2)

  //stage 3: lut ready r[top], init lut read r[top] + 1
  val rLowBits = fpT.qmnN - fpT.lutAddrBits
  val stage3Valid = RegNext(stage2Valid)
  val stage3kVec = RegNext(stage2kVec)
  val stage3rVec = RegNext(stage2rVec)
  val rLowerVec = VecInit(stage3rVec.map(r => r(rLowBits - 1, 0)))

  //stage 4: lut ready r[top] + 1, interpolate
  val stage4Valid = RegNext(stage3Valid)
  val stage4kVec = RegNext(stage3kVec)
  val stage4rLowerVec = RegNext(rLowerVec)
  val y0 = RegNext(lut.io.rdata(0))
  val y1 = lut.io.rdata(1)
  val pow2r = VecInit(y0.zip(y1).zip(stage4rLowerVec).map {
    case ((y0, y1), frac) =>
      val delta = y1 - y0
      val interp = y0 + ((delta * frac) >> rLowBits)
      interp
  })

  //stage 5: convert and return result
  val stage5Valid = RegNext(stage4Valid)
  val stage5pow2rVec = RegNext(pow2r)
  val stage5kVec = RegNext(stage4kVec)
  val resRawFloat = stage5pow2rVec.zip(stage5kVec).map{ case (qmn, k) => fpT.rawFloatFromQmnK(qmn, k) }
  roundToRecFn.zip(resRawFloat).foreach {
    case (round, rawFloat) => {
      round.io.invalidExc := false.B
      round.io.infiniteExc := false.B
      round.io.in := rawFloat
      round.io.roundingMode := req.roundingMode
      round.io.detectTininess := 0.U
    }
  }
  val resFN = VecInit(roundToRecFn.map(round => fNFromRecFN(fpT.expWidth, fpT.sigWidth, round.io.out)))
  res := Mux(stage5Valid, resFN, res)

  lut.io.raddrs(0) := stage2rVec.map(r => r(fpT.qmnN - 1, rLowBits))
  lut.io.raddrs(1) := stage3rVec.map { r =>
    val maxAddr = ((1 << fpT.lutAddrBits) - 1).U
    val addr = r(fpT.qmnN - 1, rLowBits)
    Mux(addr === maxAddr, addr, addr + 1.U)
  }

  io.req.ready := state === FPEXState.READY
  io.resp.valid := state === FPEXState.DONE
  io.resp.bits.tag := req.tag
  io.resp.bits.result := res
  io.resp.bits.laneMask := req.laneMask

  when (io.req.fire) {
    req := io.req.bits
    state := FPEXState.BUSY
    when (earlyTerminate) {
      res := earlyRes
      state := FPEXState.DONE
    }
  }.elsewhen (io.resp.fire) {
    state := FPEXState.READY
  }.elsewhen(stage5Valid) {
    state := FPEXState.DONE
  }
}

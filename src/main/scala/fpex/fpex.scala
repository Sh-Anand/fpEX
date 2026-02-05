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
  val roundingMode = FPRoundingMode()
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
  val busy = state === FPEXState.BUSY

  //stage 0: special case check and raw float decomposition
  //flush subnormals to zero, ignore x for which e^x = inf
  val rawFloatVec = VecInit(io.req.bits.xVec.map(x => rawFloatFromFN(fpT.expWidth, fpT.sigWidth, x)))
  val expFPOverflow = VecInit(io.req.bits.xVec.map(x => FPUtil.expFPIsInf(x, fpT)))
  val rawFloatOf = rawFloatVec.zip(expFPOverflow)
  val earlyRes = VecInit(rawFloatOf.map { case (x, of) =>
    MuxCase(
      0.U(fpT.wordWidth.W),
      Seq(
        x.isNaN -> Cat(x.sign, fpT.nanExp, isSigNaNRawFloat(x), fpT.nanSig),
        (x.isZero || x.isSubNorm) -> fpT.one,
        (x.isInf && x.sign) -> fpT.zero,
        ((x.isInf && !x.sign) || of) -> Cat(x.sign, fpT.infinity)
      )
    )
  })
  val earlyValid = VecInit(rawFloatOf.map { case (x, of) =>
    x.isInf || x.isZero || x.isSubNorm || x.isNaN || of
  })
  val earlyTerminate = earlyValid.asUInt.andR

  //stage 1
  val stage1Valid = RegNext(io.req.fire && !earlyTerminate)
  val stage1RawFloatVec = RegEnable(VecInit(rawFloatVec.map(_.negate(io.req.bits.neg))),
    io.req.fire && !earlyTerminate)

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
  }
}

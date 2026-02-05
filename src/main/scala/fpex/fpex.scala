package fpex

import chisel3._
import chisel3.util._
import hardfloat._

trait HasFPEXParams {
  def numFP16Lanes = 4
  def tagWidth = 8

  def getWordExpSigWidth(fmt: FPFormat.Type): (Int, Int, Int) = {
    fmt match {
      case FPFormat.FP32 => (32, 8, 24)
      case FPFormat.FP16 => (16, 5, 11)
      case FPFormat.BF16 => (16, 8, 8)
      case _ => (32, 8, 8)
    }
  }

  // get signless exp,sig bits. NOTE: returns 1 fewer sig bit, prefix quiet/signalling bit
  def constructNaNExpSig(fmt: FPFormat.Type): (UInt, UInt) = {
    fmt match {
      case FPFormat.FP32 => (Fill(8, 1.U(1.W)), Fill(22, 1.U(1.W)))
      case FPFormat.FP16 => (Fill(5, 1.U(1.W)), Fill(9, 1.U(1.W)))
      case FPFormat.BF16 => (Fill(8, 1.U(1.W)), Fill(6, 1.U(1.W)))
      case _ => (Fill(8, 1.U(1.W)), Fill(22, 1.U(1.W)))
    }
  }

  // get signless zero
  def constructZero(fmt: FPFormat.Type): UInt = {
    fmt match {
      case FPFormat.FP32 => Cat(Fill(8, 0.U(1.W)), Fill(23, 0.U(1.W)))
      case FPFormat.FP16 => Cat(Fill(5, 0.U(1.W)), Fill(10, 0.U(1.W)))
      case FPFormat.BF16 => Cat(Fill(8, 0.U(1.W)), Fill(7, 0.U(1.W)))
      case _ => Cat(Fill(8, 0.U(1.W)), Fill(23, 0.U(1.W)))
    }
  }

  // get signless one
  def constructOne(fmt: FPFormat.Type): UInt = {
    fmt match {
      case FPFormat.FP32 => Cat(127.U(8.W), 0.U(23.W))
      case FPFormat.FP16 => Cat(15.U(5.W), 0.U(10.W))
      case FPFormat.BF16 => Cat(127.U(8.W), 0.U(7.W))
      case _ => Cat(127.U(8.W), 0.U(23.W))
    }
  }

  // get signless infinity
  def constructInf(fmt: FPFormat.Type): UInt = {
    fmt match {
      case FPFormat.FP32 => Cat(Fill(8, 1.U(1.W)), 0.U(23.W))
      case FPFormat.FP16 => Cat(Fill(5, 1.U(1.W)), 0.U(10.W))
      case FPFormat.BF16 => Cat(Fill(8, 1.U(1.W)), 0.U(7.W))
      case _ => Cat(Fill(8, 1.U(1.W)), 0.U(23.W))
    }
  }
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

class FPEX(fmt: FPFormat.Type, numLanes: Int = 4, tagWidth: Int = 1)
  extends Module
  with HasFPEXParams {
  val (wordWidth, expWidth, sigWidth) = getWordExpSigWidth(fmt)
  val (naNExp, naNSig) = constructNaNExpSig(fmt)
  val zero = constructZero(fmt)
  val one = constructOne(fmt)
  val infinity = constructInf(fmt)

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new FPEXReq(wordWidth, numLanes, tagWidth)))
    val resp = Decoupled(new FPEXResp(wordWidth, numLanes, tagWidth))
  })

  val state = RegInit(FPEXState.READY)
  val req = Reg(new FPEXReq(wordWidth, numLanes, tagWidth))
  val res = Reg(Vec(numLanes, UInt(wordWidth.W)))
  val recFnVec = VecInit(io.req.bits.xVec.map(x => recFNFromFN(expWidth, sigWidth, x)))
  val busy = state === FPEXState.BUSY

  //stage 1: special case check and raw float decomposition
  val rawFloatVec = VecInit(recFnVec.map(x => rawFloatFromRecFN(expWidth, sigWidth, x)))
  val earlyRes = VecInit(rawFloatVec.zipWithIndex.map { case (x, i) =>
    MuxCase(
      0.U(wordWidth.W),
      Seq(
        x.isNaN -> Cat(x.sign, naNExp, isSigNaNRawFloat(x), naNSig),
        x.isZero -> Cat(x.sign, one),
        (x.isInf && x.sign) -> Cat(x.sign, zero),
        (x.isInf && !x.sign) -> Cat(x.sign, infinity)
      )
    )
  })
  val earlyValid = VecInit(rawFloatVec.map(x => x.isInf || x.isZero || x.isInf))
  val earlyTerminate = earlyValid.asUInt.andR

  //stage 2
  val stage2Valid = RegNext(busy && !earlyTerminate)

  state := MuxCase(state, Seq(
      (earlyTerminate && busy) -> FPEXState.DONE,
      io.req.fire -> FPEXState.BUSY,
      io.resp.fire -> FPEXState.READY
    )
  )
  res := Mux(busy && earlyTerminate, earlyRes, res)

  io.req.ready := state === FPEXState.READY
  io.resp.valid := state === FPEXState.DONE
  io.resp.bits.tag := req.tag
  io.resp.bits.result := res
  io.resp.bits.laneMask := req.laneMask

  when (io.req.fire) {
    req := io.req.bits
  }
}

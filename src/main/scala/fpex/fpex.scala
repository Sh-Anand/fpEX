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
}

object FPEXState extends ChiselEnum {
  val READY = 0.U
  val BUSY = 1.U
  val DONE = 2.U
}

class FPEXReq(wordWidth: Int, numLanes: Int, tagWidth: Int) extends Bundle {
  val roundingMode = FPRoundingMode()
  val tag = UInt(tagWidth.W)
  val laneMask = UInt(numLanes.W)
  val x = Vec(numLanes, UInt(wordWidth.W))
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
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new FPEXReq(wordWidth, numLanes, tagWidth)))
    val resp = Decoupled(new FPEXResp(wordWidth, numLanes, tagWidth))
  })

  val state = RegInit(FPEXState.READY)
  val recFnX = VecInit(io.req.bits.x.map(x => recFNFromFN(expWidth, sigWidth, x)))

  io.req.ready := state === FPEXState.READY
  io.resp.valid := state === FPEXState.DONE
  io.resp.bits.tag := 0.U
  io.resp.bits.result := recFnX
  io.resp.bits.laneMask := 0.U

  when (io.req.fire) {
    state := FPEXState.BUSY
  }.elsewhen(io.resp.fire) {
    state := FPEXState.READY
  }
}
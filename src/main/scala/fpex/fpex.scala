package fpex

import chisel3._
import chisel3.util._
import hardfloat._

class FPEXReq(numFP16Lanes: Int = 4, tagWidth: Int = 1) extends Bundle {
  val fmt = FPFormat()
  val roundingMode = FPRoundingMode()
  val tag = UInt(tagWidth.W)
  val laneMask = UInt(numFP16Lanes.W)
  val x = UInt((numFP16Lanes * 16).W)
}

class FPEXResp(numFP16Lanes: Int = 4, tagWidth: Int = 1) extends Bundle {
  val tag = UInt(tagWidth.W)
  val laneMask = UInt(numFP16Lanes.W)
  val result = UInt((numFP16Lanes * 16).W)
}

class FPEX(numFP16Lanes: Int = 4, tagWidth: Int = 1) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new FPEXReq(numFP16Lanes, tagWidth)))
    val resp = Decoupled(new FPEXResp(numFP16Lanes, tagWidth))
  })

  val recFNx = Wire(UInt((numFP16Lanes * 16).W))
  recFNx := recFNFromFN(5, 11, io.req.bits.x)

  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits.tag := 0.U
  io.resp.bits.result := recFNx
  io.resp.bits.laneMask := 0.U
}
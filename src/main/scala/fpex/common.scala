package fpex

import chisel3._

object FPRoundingMode extends ChiselEnum {
  val RNE = Value("b000".U)
  val RTZ = Value("b001".U)
  val RDN = Value("b010".U)
  val RUP = Value("b011".U)
  val RMM = Value("b100".U)
  val ROD = Value("b101".U)
  val DYN = Value("b111".U)
}

object FPFormat extends ChiselEnum {
  val FP32 = Value("b000".U)
  val FP64 = Value("b001".U) // unsupported
  val FP16 = Value("b010".U)
  val E5M2 = Value("b011".U) // unsupported
  val BF16 = Value("b100".U)
  val _w   = Value("b111".U)
}

trait HasFPEXParams {
  def numFP16Lanes = 4
  def tagWidth = 8

  def getExpSigWidth(fmt: FPFormat.Type): (Int, Int) = {
    fmt match {
      case FPFormat.FP32 => (8, 24)
      case FPFormat.FP16 => (5, 11)
      case FPFormat.BF16 => (8, 8)
      case _ => (8, 8)
    }
  }
}
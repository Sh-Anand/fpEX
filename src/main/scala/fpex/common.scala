package fpex

import chisel3._
import chisel3.util._

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

object FPConst {
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

  // get pos zero
  def constructZero(fmt: FPFormat.Type): UInt = {
    fmt match {
      case FPFormat.FP32 => Cat(0.U(1.W), Fill(8, 0.U(1.W)), Fill(23, 0.U(1.W)))
      case FPFormat.FP16 => Cat(0.U(1.W), Fill(5, 0.U(1.W)), Fill(10, 0.U(1.W)))
      case FPFormat.BF16 => Cat(0.U(1.W), Fill(8, 0.U(1.W)), Fill(7, 0.U(1.W)))
      case _ => constructZero(FPFormat.FP32)
    }
  }

  def constructOne(fmt: FPFormat.Type): UInt = {
    fmt match {
      case FPFormat.FP32 => Cat(0.U(1.W), 127.U(8.W), 0.U(23.W))
      case FPFormat.FP16 => Cat(0.U(1.W), 15.U(5.W), 0.U(10.W))
      case FPFormat.BF16 => Cat(0.U(1.W), 127.U(8.W), 0.U(7.W))
      case _ => constructOne(FPFormat.FP32)
    }
  }

  // get signless infinity
  def constructInf(fmt: FPFormat.Type): UInt = {
    fmt match {
      case FPFormat.FP32 => Cat(Fill(8, 1.U(1.W)), 0.U(23.W))
      case FPFormat.FP16 => Cat(Fill(5, 1.U(1.W)), 0.U(10.W))
      case FPFormat.BF16 => Cat(Fill(8, 1.U(1.W)), 0.U(7.W))
      case _ => constructInf(FPFormat.FP32)
    }
  }

  def getrln2(fmt: FPFormat.Type): UInt = {
    fmt match {
      case FPFormat.FP32 => "h3fb8aa3b".U(32.W) // 1/ln2
      case FPFormat.FP16 => "h3dc5".U(16.W)     // 1/ln2
      case FPFormat.BF16 => "h3fb9".U(16.W)     // 1/ln2 (bf16, RNE)
      case _ => "h3fb8aa3b".U(32.W)
    }
  }
}
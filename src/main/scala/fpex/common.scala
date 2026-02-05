package fpex

import chisel3._
import chisel3.util._
import hardfloat.RawFloat

object FPRoundingMode extends ChiselEnum {
  val RNE = Value("b000".U)
  val RTZ = Value("b001".U)
  val RDN = Value("b010".U)
  val RUP = Value("b011".U)
  val RMM = Value("b100".U)
  val ROD = Value("b101".U)
  val DYN = Value("b111".U)
}

sealed trait FPType {
  def wordWidth: Int
  def expWidth: Int
  def sigWidth: Int // includes hidden bit
  def rln2: UInt
  def qmnM: Int
  def qmnN: Int
  def maxXExp: UInt // ln(max finite) exponent
  def maxXFrac: UInt // ln(max finite) fraction
  def bias: Int = (1 << (expWidth - 1)) - 1
  def nanExp = Fill(expWidth, 1.U(1.W))
  def nanSig = Fill(sigWidth - 2, 1.U(1.W))
  def zero = Cat(0.U(1.W), Fill(expWidth, 0.U(1.W)), Fill(sigWidth - 1, 0.U(1.W)))
  def one = Cat(0.U(1.W), bias.U(expWidth.W), 0.U((sigWidth - 1).W))
  def infinity = Cat(Fill(expWidth, 1.U(1.W)), 0.U((sigWidth - 1).W))
  def qmnCtor = () => new Qmn(qmnM, qmnN)
}

object FPType {
  case object FP32T extends FPType {
    val wordWidth = 32
    val expWidth = 8
    val sigWidth = 24
    val rln2 = "h3fb8aa3b".U(32.W) // 1/ln2
    val qmnM = 10
    val qmnN = 18
    val maxXExp = "h85".U(8.W)
    val maxXFrac = "h317218".U(23.W)
  }

  case object FP16T extends FPType {
    val wordWidth = 16
    val expWidth = 5
    val sigWidth = 11
    val rln2 = "h3dc5".U(16.W) // 1/ln2
    val qmnM = 6
    val qmnN = 12
    val maxXExp = "h12".U(5.W)
    val maxXFrac = "h18c".U(10.W)
  }

  case object BF16T extends FPType {
    val wordWidth = 16
    val expWidth = 8
    val sigWidth = 8
    val rln2 = "h3fb9".U(16.W) // 1/ln2 (bf16, RNE)
    val qmnM = 9
    val qmnN = 12
    val maxXExp = "h85".U(8.W)
    val maxXFrac = "h31".U(7.W)
  }
}

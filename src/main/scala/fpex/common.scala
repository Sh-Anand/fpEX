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
  def rln2: Qmn
  def qmnM: Int
  def qmnN: Int
  def maxXExp: UInt // ln(max finite) exponent
  def maxXSig: UInt // ln(max finite) fraction
  def bias: Int = (1 << (expWidth - 1)) - 1
  def nanExp = Fill(expWidth, 1.U(1.W))
  def nanSig = Fill(sigWidth - 2, 1.U(1.W))
  def zero = Cat(0.U(1.W), Fill(expWidth, 0.U(1.W)), Fill(sigWidth - 1, 0.U(1.W)))
  def one = Cat(0.U(1.W), bias.U(expWidth.W), 0.U((sigWidth - 1).W))
  def infinity = Cat(Fill(expWidth, 1.U(1.W)), 0.U((sigWidth - 1).W))
  def qmnCtor = () => new Qmn(qmnM, qmnN)
  def lutAddrBits: Int
  def lutValM: Int = 1
  def lutValN: Int

  def expFPIsInf(in: UInt, neg: Bool): Bool = {
    val sign = in(wordWidth - 1) ^ neg
    val exp = in(wordWidth - 2, sigWidth - 1)
    val frac = in(sigWidth - 2, 0)
    !sign && (exp > maxXExp || (exp === maxXExp && frac > maxXSig))
  }

  // assumes no special cases and no overflow. no rounding
  def qmnFromRawFloat(rawFloat: RawFloat): Qmn = {
    val shift = qmnN.asSInt + rawFloat.sExp - (sigWidth - 1).asSInt
    val qmn = Wire(qmnCtor())
    val mag = Mux(shift < 0.S, rawFloat.sig >> (-shift).asUInt, rawFloat.sig << shift.asUInt).asSInt
    qmn.value := Mux(rawFloat.sign, -mag, mag)
    qmn
  }
}

object FPType {
  case object FP32T extends FPType {
    val wordWidth = 32
    val expWidth = 8
    val sigWidth = 24
    val qmnM = 10
    val qmnN = 24
    def rln2 = new Qmn(2, qmnN)(378194.S((2 + qmnN).W)) // 1/ln2 in Q2.18
    val maxXExp = "h85".U(expWidth.W)
    val maxXSig = "h317218".U((sigWidth - 1).W)
    val lutAddrBits = 6
    val lutValN = 30
  }

  case object FP16T extends FPType {
    val wordWidth = 16
    val expWidth = 5
    val sigWidth = 11
    val qmnM = 6
    val qmnN = 12
    def rln2 = new Qmn(2, qmnN)(5909.S((2 + qmnN).W)) // 1/ln2 in Q2.12
    val maxXExp = "h12".U(expWidth.W)
    val maxXSig = "h18c".U((sigWidth - 1).W)
    val lutAddrBits = 6
    val lutValN = 16
  }

  case object BF16T extends FPType {
    val wordWidth = 16
    val expWidth = 8
    val sigWidth = 8
    val qmnM = 9
    val qmnN = 12
    def rln2 = new Qmn(2, qmnN)(5909.S((2 + qmnN).W)) // 1/ln2 in Q2.12
    val maxXExp = "h85".U(8.W)
    val maxXSig = "h31".U((sigWidth - 1).W)
    val lutAddrBits = 5
    val lutValN = 16
  }
}

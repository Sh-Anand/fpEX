package fpex

import chisel3._
import chisel3.util._
import hardfloat._

class Qmn(val m: Int, val n: Int) extends Bundle {
  def apply(value: SInt): Qmn = {
    val q = Wire(new Qmn(m, n))
    q.value := value
    q
  }

  // round to nearest even
  def mul(qkn: Qmn): Qmn = {
    val prod = Wire(SInt((m + qkn.m + n + n).W))
    prod := value * qkn.value
    val neg = prod < 0.S
    val mag = Mux(neg, (-prod).asUInt, prod.asUInt)
    val roundedMag = (mag + (1.U << (n - 1))) >> n
    val roundedMagTrunc = roundedMag(m + qkn.m + n - 1, 0)
    val rounded = Mux(neg, -roundedMagTrunc.asSInt, roundedMagTrunc.asSInt)
    val res = new Qmn(m + qkn.m, n)(rounded)
    res
  }

  // get integer and fractional parts
  def getKR = {
    val k = value >> n
    val r = value(n - 1, 0).asUInt
    (k, r)
  }

  val value = SInt((m + n).W) // integer
}

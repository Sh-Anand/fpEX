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
    val guard = mag(n - 1)
    val sticky = if (n > 1) mag(n - 2, 0).orR else false.B
    val lsb = mag(n)
    val inc = (guard & (sticky | lsb)).asUInt
    val roundedMag = (mag >> n) + inc
    val outWidth = m + qkn.m + n
    val roundedMagTrunc = roundedMag(outWidth - 1, 0)
    val rounded = Mux(neg, -roundedMagTrunc.asSInt, roundedMagTrunc.asSInt)
    val res = new Qmn(m + qkn.m, n)(rounded)
    res
  }

  val value = SInt((m + n).W) // integer
}

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

  // truncate toward zero. assumes multiplicand n is same as us
  def mul(qkn: Qmn): Qmn = {
    assert(n == qkn.n)
    val prod = ((value * qkn.value) >> n).asSInt
    new Qmn(m + qkn.m, n)(prod)
  }

  // get integer and fractional parts
  def getKR = {
    val k = (value >> n).asSInt
    val r = value(n - 1, 0).asUInt
    (k, r)
  }

  val value = SInt((m + n).W) // integer
}

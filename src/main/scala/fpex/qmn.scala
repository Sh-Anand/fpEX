package fpex

import chisel3._
import chisel3.util._
import hardfloat._

class Qmn(val m: Int, val n: Int) extends Bundle {
  def apply(value: SInt) = {
    this.value := value
    this
  }
  val value = SInt((m + n).W) // integer
}
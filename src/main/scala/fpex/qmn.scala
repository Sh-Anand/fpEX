package fpex

import chisel3._
import chisel3.util._
import hardfloat._

class Qmn(val m: Int, val n: Int) extends Bundle {
  val sign: Bool = Bool()
  val i = UInt(m.W) // integer
  val f = UInt(n.W) // fraction
}
package fpex

import chisel3._
import chisel3.util._

class ExLUT(ports: Int, addrBits: Int, m: Int, n: Int,
            min: Double = 0.0, max: Double = 1.0)
  extends Module {
  val io = IO(new Bundle {
    val raddrs = Input(Vec(ports, UInt(addrBits.W)))
    val rdata = Output(Vec(ports, UInt((m + n).W)))
  })

  val entries = 1 << addrBits
  val lut = VecInit.tabulate(entries) { i =>
    val r = min + i.toDouble * (max - min) / entries
    val v = math.pow(2.0, r)
    val scaled = BigInt(math.round(v * (1 << n)))
    scaled.U((m + n).W)
  }

  val results = Reg(Vec(ports, UInt((m + n).W)))
  results.zip(io.raddrs).foreach { case (res, raddr) => res := lut(raddr) }
  io.rdata := results
}
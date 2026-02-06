package fpex

import chisel3._
import chisel3.util._

class ExLUT(ports: Int, addrBits: Int, m: Int, n: Int,
            min: Double = 0.0, max: Double = 1.0)
  extends Module {
  val io = IO(new Bundle {
    val raddrs = Input(Vec(2, Vec(ports, UInt(addrBits.W))))
    val rdata = Output(Vec(2, Vec(ports, UInt((m + n).W))))
  })

  val entries = 1 << addrBits
  val lut = VecInit.tabulate(entries) { i =>
    val r = min + i.toDouble * (max - min) / entries
    val v = math.pow(2.0, r)
    val scaled = BigInt(math.round(v * (1 << n)))
    scaled.U((m + n).W)
  }

  val res = Reg(Vec(2, Vec(ports, UInt((m + n).W))))
  res.zip(io.raddrs).foreach {
    case (results, raddrs) => results.zip(raddrs).foreach {
      case (result, raddr) => result := lut(raddr)
    }
  }
  io.rdata := res
}
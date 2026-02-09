package fpex

import chisel3._
import chisel3.util._

class ExLUT(ports: Int, addrBits: Int, m: Int, n: Int,
            min: Double = 0.0, max: Double = 1.0)
  extends Module {
  val io = IO(new Bundle {
    val raddr = Input(Vec(ports, UInt(addrBits.W)))
    val ren = Input(Vec(ports, Bool()))
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

  val maxAddr = ((1 << addrBits) - 1).U
  val raddrNxt = io.raddr.map(raddr => Mux(raddr === maxAddr, raddr, raddr + 1.U))
  res.zip(Seq(io.raddr, raddrNxt)).foreach {
    case (results, raddrs) => results.zip(raddrs).zip(io.ren).foreach {
      case ((result, raddr), en) => when (en) { result := lut(raddr) }
    }
  }
  io.rdata := res
}
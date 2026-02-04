package fpex

import chisel3._

class TopIO extends Bundle {
  val in  = Input(UInt(8.W))
  val out = Output(UInt(8.W))
}

class Top extends Module {
  val io = IO(new TopIO)
  io.out := io.in
}

object Main extends App {
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new Top,
    Array("--target-dir", "generated")
  )
}

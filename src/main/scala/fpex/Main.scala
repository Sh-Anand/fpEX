package fpex

object Main extends App {
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new FPEX,
    Array("--target-dir", "generated")
  )
}

package fpex

object Main extends App {
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new FPEX(FPType.FP32T),
    Array("--target-dir", "generated")
  )
}

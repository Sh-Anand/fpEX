package fpex

object Main extends App {
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new FPEX(FPFormat.FP32),
    Array("--target-dir", "generated")
  )
}

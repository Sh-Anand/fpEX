package fpex

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class BF16ModelBitExactSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FPEX BF16 model equivalence"

  private case class VecCase(x: Int, rm: Int, expected: Int)

  private def loadCases(path: String): Seq[VecCase] = {
    val src = Source.fromFile(path)
    try {
      src.getLines()
        .filter(_.nonEmpty)
        .filterNot(_.startsWith("#"))
        .zipWithIndex
        .map { case (line, idx) =>
          val parts = line.trim.split("\\s+")
          require(parts.length == 3, s"bad vector format at line ${idx + 1}: '$line'")
          VecCase(
            x = Integer.parseInt(parts(0), 16) & 0xffff,
            rm = parts(1).toInt & 0x7,
            expected = Integer.parseInt(parts(2), 16) & 0xffff
          )
        }
        .toSeq
    } finally {
      src.close()
    }
  }

  it should "match python model output exactly for all provided BF16 vectors" in {
    val vectorPath = sys.env.getOrElse("BF16_MODEL_VECTORS", "/tmp/bf16_model_vectors.tsv")
    val cases = loadCases(vectorPath)
    require(cases.nonEmpty, s"no vectors loaded from $vectorPath")

    test(new FPEX(FPType.BF16T, numLanes = 1)) { dut =>
      dut.io.req.valid.poke(false.B)
      dut.io.req.bits.roundingMode.poke(0.U)
      dut.io.req.bits.tag.poke(0.U)
      dut.io.req.bits.neg.poke(false.B)
      dut.io.req.bits.laneMask.poke(1.U)
      dut.io.req.bits.xVec(0).poke(0.U)
      dut.io.resp.ready.poke(true.B)
      dut.clock.step(8)

      var checked = 0
      cases.foreach { c =>
        dut.io.req.bits.roundingMode.poke(c.rm.U)
        dut.io.req.bits.xVec(0).poke(c.x.U)
        dut.io.req.valid.poke(true.B)

        while (!dut.io.req.ready.peek().litToBoolean) {
          dut.clock.step()
        }
        dut.clock.step()
        dut.io.req.valid.poke(false.B)

        while (!dut.io.resp.valid.peek().litToBoolean) {
          dut.clock.step()
        }
        val got = (dut.io.resp.bits.result(0).peek().litValue & 0xffff).toInt
        assert(
          got == c.expected,
          f"mismatch at case#$checked x=0x${c.x}%04x rm=${c.rm}: got=0x$got%04x exp=0x${c.expected}%04x"
        )
        dut.clock.step()
        checked += 1
      }

      println(s"[BF16ModelBitExactSpec] checked $checked vectors from $vectorPath")
    }
  }
}

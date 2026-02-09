package fpex

import chisel3._
import chisel3.util._
import hardfloat._

trait HasFPEXParams {
  def numFP16Lanes = 4
  def tagWidth = 8

  def maskLane[T <: Data](currVec: Vec[T], nxtVec: Vec[T], laneMask: Vec[Bool], bp: Bool): Vec[T] = {
    require(laneMask.length == currVec.length && currVec.length == nxtVec.length)
    VecInit(currVec.zip(nxtVec).zip(laneMask).map{case ((curr, nxt), en) => Mux(en && !bp, nxt, curr)})
  }
  def maskLaneNext[T <: Data](bitsVec: Vec[T], laneMask: Vec[Bool], bp: Bool): Vec[T] = {
    require(laneMask.length == bitsVec.length)
    VecInit(bitsVec.zip(laneMask).map{case (bits, en) => RegEnable(bits, en && !bp)})
  }
}

class FPEXReq(wordWidth: Int, numLanes: Int, tagWidth: Int) extends Bundle {
  val roundingMode = UInt(3.W)
  val tag = UInt(tagWidth.W)
  val neg = Bool() // 0 = e^x, 1 = e^-x
  val laneMask = UInt(numLanes.W)
  val xVec = Vec(numLanes, UInt(wordWidth.W))
}

class FPEXResp(wordWidth: Int, numLanes: Int, tagWidth: Int) extends Bundle {
  val tag = UInt(tagWidth.W)
  val laneMask = UInt(numLanes.W)
  val result = Vec(numLanes, UInt(wordWidth.W))
}

class FPEX(fpT: FPType, numLanes: Int = 4, tagWidth: Int = 1)
  extends Module
  with HasFPEXParams {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new FPEXReq(fpT.wordWidth, numLanes, tagWidth)))
    val resp = Decoupled(new FPEXResp(fpT.wordWidth, numLanes, tagWidth))
  })

  val lut = Module(new ExLUT(numLanes, fpT.lutAddrBits, fpT.lutValM, fpT.lutValN))
  val roundToRecFn = Seq.fill(numLanes)(Module(new RoundRawFNToRecFN(fpT.expWidth, fpT.sigWidth, 0)))
  val rLowBits = fpT.qmnN - fpT.lutAddrBits
  val laneEnable = VecInit((io.req.bits.laneMask & VecInit.fill(numLanes)(io.req.fire).asUInt).asBools)
  val maskedXVec = io.req.bits.xVec.zip(laneEnable).map {
    case (x, en) => Mux (en, x, 0.U(fpT.wordWidth.W))
  }
  val maskedNeg = io.req.bits.neg && io.req.fire

  //stage 0: special case check and raw float decomposition
  //flush subnormals to zero, ignore x for which e^x = inf
  val rawFloatVec = VecInit(maskedXVec.map(
    x => rawFloatFromFN(fpT.expWidth, fpT.sigWidth, x).negate(maskedNeg)))
  val expFPOverflow = VecInit(maskedXVec.map(x => fpT.expFPIsInf(x, maskedNeg)))
  val rawFloatOf = rawFloatVec.zip(expFPOverflow)
  val earlyResult = VecInit(rawFloatOf.map { case (x, of) =>
    MuxCase(
      0.U(fpT.wordWidth.W),
      Seq(
        x.isNaN -> Cat(x.sign, fpT.nanExp, isSigNaNRawFloat(x), fpT.nanSig),
        (x.isZero || x.isSubNorm) -> fpT.one,
        (x.isInf && x.sign) -> fpT.zero,
        ((x.isInf && !x.sign) || of) -> Cat(0.U(1.W), fpT.infinity)
      )
    )
  })
  val earlyTerminate = VecInit(rawFloatOf.map { case (x, of) =>
    x.isInf || x.isZero || x.isSubNorm || x.isNaN || of
  })

  class CommonStageState extends Bundle {
    val valid = Bool()
    val req = chiselTypeOf(io.req.bits)
    val laneEn = chiselTypeOf(laneEnable)
    val earlyTerm = chiselTypeOf(earlyTerminate)
    val earlyRes = chiselTypeOf(earlyResult)
  }

  def numIntermediateStages = 6
  val commonState = RegInit(VecInit(Seq.fill(numIntermediateStages)(0.U.asTypeOf(new CommonStageState))))
  val backPressure = Wire(Vec(numIntermediateStages, Bool()))
  val stateWithBp = commonState.zip(backPressure)
  def st(i: Int) = commonState(i-1)
  def bp(i: Int) = backPressure(i-1)
  stateWithBp.take(1).foreach { case (state, back) =>
      state.valid := Mux(!back, io.req.fire, state.valid)
      state.req := Mux(io.req.fire, io.req.bits, state.req)
      state.laneEn := Mux(!back, laneEnable, state.laneEn)
      state.earlyTerm := maskLane(state.earlyTerm, earlyTerminate, laneEnable, back)
      state.earlyRes := maskLane(state.earlyRes, earlyResult, laneEnable, back)
  }

  stateWithBp.zipWithIndex.takeRight(numIntermediateStages - 1).foreach {
    case ((state, back), i) =>
      state.valid := Mux(!back, st(i).valid, state.valid)
      state.req := Mux(st(i).valid && !back, st(i).req, state.req)
      state.laneEn := Mux(!back, st(i).laneEn, state.laneEn)
      state.earlyTerm := maskLane(state.earlyTerm, st(i).earlyTerm, st(i).laneEn, back)
      state.earlyRes := maskLane(state.earlyRes, st(i).earlyRes, st(i).laneEn, back)
  }

  backPressure(numIntermediateStages-1) := !io.resp.ready
  backPressure.zip(commonState).zipWithIndex.take(numIntermediateStages-1).foreach { case ((bp, st), i) =>
      bp := st.valid && backPressure(i+1)
  }

  //stage 1: convert to Qmn
  val stage1RawFloatVec = maskLaneNext(rawFloatVec, laneEnable, bp(1))
  val stage1Qmn = VecInit(stage1RawFloatVec.map(x => fpT.qmnFromRawFloat(x)))

  //stage 2: multiply x/ln2, extract k and r, init lut read r[top]
  val stage2Qmn = maskLaneNext(stage1Qmn, st(1).laneEn, bp(2))
  val xrln2KRVec = stage2Qmn.map(_.mul(fpT.rln2).getKR).unzip
  val stage2kVec = VecInit(xrln2KRVec._1)
  val stage2rVec = VecInit(xrln2KRVec._2)

  //stage 3: lut ready r[top], init lut read r[top] + 1
  val stage3kVec = maskLaneNext(stage2kVec, st(2).laneEn, bp(3))
  val stage3rVec = maskLaneNext(stage2rVec, st(2).laneEn, bp(3))
  val stage3AddrVec = VecInit(stage3rVec.map(r => r(fpT.qmnN - 1, rLowBits)))
  val rLowerVec = VecInit(stage3rVec.map(r => r(rLowBits - 1, 0)))

  //stage 4: lut ready r[top] + 1
  val stage4kVec = maskLaneNext(stage3kVec, st(3).laneEn, bp(4))
  val stage4rLowerVec = maskLaneNext(rLowerVec, st(3).laneEn, bp(4))
  val stage4AddrVec = maskLaneNext(stage3AddrVec, st(3).laneEn, bp(4))
  val stage4y0 = maskLaneNext(lut.io.rdata(0), st(3).laneEn, bp(4))

  //stage 5: interpolate
  val stage5y0 = maskLaneNext(stage4y0, st(4).laneEn, bp(5))
  val stage5y1 = maskLaneNext(lut.io.rdata(1), st(4).laneEn, bp(5))
  val stage5rLowerVec = maskLaneNext(stage4rLowerVec, st(4).laneEn, bp(5))
  val stage5AddrVec = maskLaneNext(stage4AddrVec, st(4).laneEn, bp(5))
  val stage5kVec = maskLaneNext(stage4kVec, st(4).laneEn, bp(5))
  val lutTopEndpoint = ((BigInt(1) << (fpT.lutValM + fpT.lutValN)) - 1).U((fpT.lutValM + fpT.lutValN).W)
  val stage5pow2rVec = VecInit(stage5y0.zip(stage5y1).zip(stage5rLowerVec).zip(stage5AddrVec).map {
    case (((y0, y1), frac), addr) =>
      val y1Interp = Mux(addr === ((1 << fpT.lutAddrBits) - 1).U, lutTopEndpoint, y1)
      val delta = y1Interp - y0
      val interp = y0 + ((delta * frac) >> rLowBits)
      interp
  })

  //stage 6: convert and return result
  val stage6pow2rVec = maskLaneNext(stage5pow2rVec, st(5).laneEn, bp(6))
  val stage6kVec = maskLaneNext(stage5kVec, st(5).laneEn, bp(6))
  val resRawFloat = stage6pow2rVec.zip(stage6kVec).map{ case (qmn, k) => fpT.rawFloatFromQmnK(qmn, k) }
  roundToRecFn.zip(resRawFloat).zip(st(6).laneEn).foreach {
    case ((round, rawFloat), en) => {
      round.io.invalidExc := false.B
      round.io.infiniteExc := false.B
      round.io.in := rawFloat
      round.io.roundingMode := Mux(en, st(6).req.roundingMode, 0.U)
      round.io.detectTininess := 0.U
    }
  }
  val resValid = st(6).valid
  val resReq = st(6).req
  val resFN = roundToRecFn.map(round => fNFromRecFN(fpT.expWidth, fpT.sigWidth, round.io.out))
  val resFinal = VecInit(resFN.zip(st(6).earlyTerm.zip(st(6).earlyRes)).map {
    case (res, (earlyTerminate, earlyRes)) => Mux(earlyTerminate, earlyRes, res)
  })

  lut.io.raddrs(0) := stage2rVec.map(r => r(fpT.qmnN - 1, rLowBits))
  lut.io.raddrs(1) := stage3rVec.map { r =>
    val maxAddr = ((1 << fpT.lutAddrBits) - 1).U
    val addr = r(fpT.qmnN - 1, rLowBits)
    Mux(addr === maxAddr, addr, addr + 1.U)
  }

  io.req.ready := !backPressure(0)
  io.resp.valid := resValid
  io.resp.bits.tag := resReq.tag
  io.resp.bits.result := resFinal
  io.resp.bits.laneMask := resReq.laneMask
}

package nutcore.backend.fu.fpu.fudian

import chisel3._
import chisel3.util._
import nutcore.backend.fu.fpu.fudian.utils._
import nutcore.backend.fu.fpu.fudian.utils.lza_utils._

class FMULToFADD_fflags extends Bundle {
  val isNaN = Bool()
  val isInf = Bool()
  val isInv = Bool()
  val overflow = Bool()
}
class FMULToFADD(val expWidth: Int, val sigWidth: Int) extends Bundle {
  val fp_prod = UInt((expWidth + 2 * sigWidth).W)
  val inter_flags = new FMULToFADD_fflags
}

class FMUL(val expWidth: Int, val sigWidth: Int) extends Module {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new Bundle() {
      val a, b = Output(UInt((expWidth + sigWidth).W))
      val roundingMode = Output(UInt(3.W))
    }))
    val out = Decoupled(new Bundle() {
      val result = Output(UInt((expWidth + sigWidth).W))
      val fflags = Output(UInt(5.W))
      val tofadd = Output(new FMULToFADD(expWidth, sigWidth))
    })
  })
  val fp_a = FloatPoint.fromUInt(io.in.bits.a, expWidth, sigWidth)
  val fp_b = FloatPoint.fromUInt(io.in.bits.b, expWidth, sigWidth)
  val (decode_a, decode_b) = (fp_a.decode, fp_b.decode)
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))
  val raw_b = RawFloat.fromFP(fp_b, Some(decode_b.expNotZero))
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val hasZero = decode_a.isZero | decode_b.isZero
  val resultSign = fp_a.sign ^ fp_b.sign
  val expSum = raw_a.exp +& raw_b.exp
  val biasInt = FloatPoint.expBias(expWidth)
  val expSumMinusBias = Cat(0.U(1.W), expSum) - biasInt.U
  val s1_exp_underflow = expSumMinusBias.head(1).asBool
  val s1_exp_diff_abs = Mux(
    s1_exp_underflow,
    (~expSumMinusBias.tail(1)).asUInt + 1.U,
    expSumMinusBias.tail(1)
  )
  val s1_exp_overflow = s1_exp_diff_abs > ((1 << expWidth) - 2).U

  val s1_exp_no_shift = PriorityMux(
    Seq(
      (s1_exp_underflow | hasZero) -> 0.U,
      s1_exp_overflow -> ((1 << expWidth) - 1).U,
      true.B -> s1_exp_diff_abs
    )
  )
  val s1_sig_no_shift = (raw_a.sig * raw_b.sig)(sigWidth * 2 - 1, 0)

  val aLZC = LZC(raw_a.sig)
  val bLZC = LZC(raw_b.sig)
  val s1_lzc_raw = aLZC + bLZC
  val s1_error_mask = Cat(1.U(1.W), 0.U((sigWidth * 2 - 1).W)) >> s1_lzc_raw

  class FMUL_S1_S2 extends Bundle{
    val sig_no_shift = Output(UInt((sigWidth * 2).W))
    val exp_no_shift = Output(UInt((expWidth + 1).W))
    val exp_diff_abs = Output(UInt((expWidth + 1).W))
    val exp_underflow = Output(Bool())
    val exp_overflow = Output(Bool())
    val lzc_raw = Output(UInt(s1_lzc_raw.getWidth.W))
    val error_mask = Output(UInt((sigWidth * 2).W))
  }
  val pipeline_s1_s2 = Module(new PipelineReg(new FMUL_S1_S2))
  pipeline_s1_s2.io.in.bits.exp_overflow := s1_exp_overflow
  pipeline_s1_s2.io.in.bits.exp_underflow := s1_exp_underflow
  pipeline_s1_s2.io.in.bits.exp_no_shift := s1_exp_no_shift
  pipeline_s1_s2.io.in.bits.sig_no_shift := s1_sig_no_shift
  pipeline_s1_s2.io.in.bits.exp_diff_abs := s1_exp_diff_abs
  pipeline_s1_s2.io.in.bits.lzc_raw := s1_lzc_raw
  pipeline_s1_s2.io.in.bits.error_mask := s1_error_mask
  pipeline_s1_s2.io.flush := io.flush
  pipeline_s1_s2.io.in.valid := io.in.valid
  pipeline_s1_s2.io.out.ready := io.out.ready
  val s2_sig_no_shift = pipeline_s1_s2.io.out.bits.sig_no_shift
  val s2_exp_no_shift = pipeline_s1_s2.io.out.bits.exp_no_shift
  val s2_exp_diff_abs = pipeline_s1_s2.io.out.bits.exp_diff_abs
  val s2_exp_underflow = pipeline_s1_s2.io.out.bits.exp_underflow
  val s2_exp_overflow = pipeline_s1_s2.io.out.bits.exp_overflow
  val s2_lzc_raw = pipeline_s1_s2.io.out.bits.lzc_raw
  val s2_error_mask = pipeline_s1_s2.io.out.bits.error_mask
  /*------------------------------------------------------------------------
  Shift
  *------------------------------------------------------------------------*/
  val lzcError = !(s2_sig_no_shift & s2_error_mask).orR
  val lzc = Mux(lzcError, s2_lzc_raw + 1.U, s2_lzc_raw)

  val (resultSigRightShifted, rightShiftSticky) = ShiftRightJam(s2_sig_no_shift, s2_exp_diff_abs)
  val leftShiftLimit = (s2_exp_no_shift + 1.U) <= lzc
  val resultSigLeftShifted = (s2_sig_no_shift << Mux(leftShiftLimit, s2_exp_no_shift, lzc))(sigWidth * 2 - 1, 0)

  val resultExpShifted = Mux(leftShiftLimit || s2_exp_underflow, 0.U, s2_exp_no_shift - lzc + 1.U)
  val resultSigShifted = Mux(s2_exp_overflow,
    Cat(resultSigRightShifted, rightShiftSticky),
    Cat(resultSigLeftShifted, 0.U)
  )
  /*------------------------------------------------------------------------
  Rounding
  *------------------------------------------------------------------------*/
  val rm = io.in.bits.roundingMode
  val resultShifted = Wire(new RawFloat(expWidth, sigWidth + 3))
  resultShifted.sign := resultSign
  resultShifted.exp := resultExpShifted.tail(1)
  resultShifted.sig := resultSigShifted.head(sigWidth + 2) ## resultSigShifted.tail(sigWidth + 2).orR
  val tininess_rounder = Module(new TininessRounder(expWidth, sigWidth))
  tininess_rounder.io.in := resultShifted
  tininess_rounder.io.rm := rm
  val tininess = tininess_rounder.io.tininess
  val rounder = RoundingUnit(
    resultShifted.sig.tail(1), // hidden bit is not needed
    rm,
    resultShifted.sign,
    sigWidth - 1
  )
  val exp_rounded = rounder.io.cout + resultShifted.exp
  val sig_rounded = rounder.io.out
  /*------------------------------------------------------------------------
  Exception
  *------------------------------------------------------------------------*/
  val common_of = Mux(
    rounder.io.cout,
    resultShifted.exp === ((BigInt(1) << expWidth) - 2).U,
    resultShifted.exp === ((BigInt(1) << expWidth) - 1).U
  ) || s2_exp_overflow
  val common_ix = rounder.io.inexact | common_of
  val common_uf = tininess & common_ix

  val rmin = RoundingUnit.is_rmin(rm, resultShifted.sign)

  val of_exp = Mux(rmin,
    ((BigInt(1) << expWidth) - 2).U(expWidth.W),
    ((BigInt(1) << expWidth) - 1).U(expWidth.W)
  )
  /*------------------------------------------------------------------------
  Special Case
  *------------------------------------------------------------------------*/
  val hasNaN = decode_a.isNaN || decode_b.isNaN
  val hasSNaN = decode_a.isSNaN || decode_b.isSNaN
  val hasInf = decode_a.isInf || decode_b.isInf
  val special_case_happen = hasZero || hasNaN || hasInf

  val zero_mul_inf = hasZero && hasInf
  val nan_result = hasNaN || zero_mul_inf
  val special_iv = hasSNaN || zero_mul_inf
  val special_result = Mux(nan_result,
    Cat(0.U(1.W), Fill(expWidth + 1, 1.U(1.W)), 0.U((sigWidth - 2).W)), // default NaN
    Mux(hasInf,
      Cat(resultSign,
        ((BigInt(1) << expWidth) - 1).U(expWidth.W),
        0.U((sigWidth - 1).W)), // inf
      Cat(resultSign, 0.U((expWidth + sigWidth - 1).W)) // zero
    )
  )
  val special_fflags = Cat(special_iv, false.B, false.B, false.B, false.B)
  /*------------------------------------------------------------------------
  Result
  *------------------------------------------------------------------------*/
  val common_exp = Mux(
    common_of,
    of_exp,
    exp_rounded(expWidth - 1, 0)
  )
  val common_sig = Mux(
    common_of,
    Mux(rmin, Fill(sigWidth - 1, 1.U(1.W)), 0.U((sigWidth - 1).W)),
    sig_rounded
  )
  val common_result = Cat(resultShifted.sign, common_exp, common_sig)

  val common_fflags = Cat(false.B, false.B, common_of, common_uf, common_ix)

  io.out.valid := pipeline_s1_s2.io.out.valid
  io.in.ready := pipeline_s1_s2.io.in.ready
  io.out.bits.result := Mux(special_case_happen, special_result, common_result)
  io.out.bits.fflags := Mux(special_case_happen, special_fflags, common_fflags)

  io.out.bits.tofadd.fp_prod := resultSign ##
    resultExpShifted.tail(1) ##
    resultSigShifted.tail(1).head(2 * sigWidth - 2) ##
    resultSigShifted.tail(2 * sigWidth - 1).orR
  io.out.bits.tofadd.inter_flags.isInv := special_iv
  io.out.bits.tofadd.inter_flags.isInf := hasInf
  io.out.bits.tofadd.inter_flags.isNaN := nan_result
  io.out.bits.tofadd.inter_flags.overflow := resultExpShifted > Fill(expWidth, 1.U(1.W))
}

object FMUL {
  def apply(a: UInt, b: UInt, rm: UInt,
            expWidth: Int, sigWidth: Int,
            pre_valid: Bool, post_ready: Bool, flush: Bool) = {
    val fmul = Module(new FMUL(expWidth, sigWidth))

    //    val prehandshaked = pre_valid && fmul.io.in.ready
    //    val valid = RegInit(false.B)
    //    when(fmul.io.toFADD.valid && post_ready) {
    //      valid := false.B
    //    }
    //    when(prehandshaked) {
    //      valid := true.B
    //    }

    fmul.io.flush := flush
    fmul.io.in.valid := pre_valid
    //    fmul.io.in.bits.a := RegEnable(a, prehandshaked)
    //    fmul.io.in.bits.b := RegEnable(b, prehandshaked)
    fmul.io.in.bits.a := a
    fmul.io.in.bits.b := b
    fmul.io.in.bits.roundingMode := rm
    fmul.io.out.ready := post_ready

    (fmul.io.out.bits, fmul.io.out.valid, fmul.io.in.ready)
  }
}

object FMUL32 extends App {
  emitVerilog(new FMUL(8, 24), Array("--target-dir", "generated"))
}
package nutcore

import chisel3._
import chisel3.util._
import nutcore.backend.fu.fpu.fudian._
import float._

trait FCOMPOpType {
  def fmin_s = "b100_0001".U
  def fmax_s = "b100_0010".U
  def feq_s = "b100_0100".U
  def flt_s = "b100_1000".U
  def fle_s = "b100_1100".U

  def fmin_d = "b000_0001".U
  def fmax_d = "b000_0010".U
  def feq_d = "b000_0100".U
  def flt_d = "b000_1000".U
  def fle_d = "b000_1100".U

}

class FCOMPIO(expWidth: Int, sigWidth: Int) extends Bundle {
  val in = Flipped(DecoupledIO(new FPDataIn(expWidth, sigWidth, 2)))
  val out = DecoupledIO(new IntDataOut(64))
  val func = Input(UInt(7.W))
  val flush = Input(Bool())
}

class FCOMP extends Module with FCOMPOpType {
  val S = fp32
  val D = fp64
  val io = IO(new FCOMPIO(D.expWidth, D.sigWidth))

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src(0), io.in.bits.src(1), io.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): IntDataOut = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val min = func(0).asBool
  val max = func(1).asBool
  val eq = func(2).asBool
  val lt = func(3).asBool
  val le = eq && lt
  val single = func(6).asBool

  val unboxed_src = VecInit((0 until 2).map(i => unbox(io.in.bits.src(i), S)))
  val sfcmp = Module(new FCMP(S.expWidth, S.sigWidth))
  sfcmp.io.in.src := unboxed_src
  sfcmp.io.in.rm := io.in.bits.rm
  val sfcmp_data = Mux1H(
    Seq(
      eq -> sfcmp.io.out.eq,
      lt -> sfcmp.io.out.lt,
      le -> sfcmp.io.out.le
    )
  )
  val dfcmp = Module(new FCMP(D.expWidth, D.sigWidth))
  dfcmp.io.in := io.in.bits
  val dfcmp_data = Mux1H(
    Seq(
      eq -> dfcmp.io.out.eq,
      lt -> dfcmp.io.out.lt,
      le -> dfcmp.io.out.le
    )
  )
  val fcmp_data = Mux(single, sfcmp_data, dfcmp_data)
  val fcmp_fflags = Mux(single, sfcmp.io.out.fflags, dfcmp.io.out.fflags)

  def minmax(src: Vec[UInt], ftype: FType, max: Bool) = {
    val fp_a = FloatPoint.fromUInt(src(0), ftype.expWidth, ftype.sigWidth).decode
    val fp_b = FloatPoint.fromUInt(src(1), ftype.expWidth, ftype.sigWidth).decode
    val isnan1 = fp_a.isNaN
    val isnan2 = fp_b.isNaN
    val isInv = fp_a.isSNaN || fp_b.isSNaN
    val isNaNOut = isnan1 && isnan2
    val fcmp = if(ftype == S) sfcmp else dfcmp
    val isLHS = isnan2 || io.in.bits.rm(0) =/= fcmp.io.out.lt && !isnan1
    val cond = max ^ isLHS
    val minmax_data = Mux(isNaNOut,
      FloatPoint.defaultNaNUInt(ftype.expWidth, ftype.sigWidth),
      Mux(cond, src(0), src(1))
    )
    val minmax_fflags = Cat(isInv, 0.U(4.W))
    (minmax_data, minmax_fflags)
  }

  val sfminmax = minmax(unboxed_src, S, max)
  val dfminmax = minmax(io.in.bits.src, D, max)
  val fminmax_data = Mux(single, box(sfminmax._1, D), dfminmax._1)
  val fminmax_fflags = Mux(single, sfminmax._2, dfminmax._2)

  io.in.ready := io.out.ready || io.flush
  io.out.valid := valid
  io.out.bits.result := Mux(io.func(0) || io.func(1), fminmax_data, Cat(0.U(63.W), fcmp_data))
  io.out.bits.fflags := Mux(io.func(0) || io.func(1), fminmax_fflags, fcmp_fflags)
}

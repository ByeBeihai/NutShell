package nutcore

import chisel3._
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}
import chisel3.util._
import nutcore.backend.fu.fpu.fudian._
import float._

import scala.collection.mutable

trait FDivSqrtOpType {
  def fdiv_s = "b100_0010".U
  def fsqrt_s = "b100_0001".U

  def fdiv_d = "b000_0010".U
  def fsqrt_d = "b000_0001".U

  def isDiv(op: UInt) = op(1)
  def isSqrt(op: UInt) = op(0)
  def single(op: UInt) = op(6)
}

class FDivSqrtIO(expWidth: Int, sigWidth: Int) extends Bundle {
  val in = Flipped(DecoupledIO(new FPDataIn(expWidth, sigWidth, 2)))
  val func = Input(UInt(7.W))
  val flush = Input(Bool())
  val out = DecoupledIO(new FPDataOut(expWidth, sigWidth))
}

class FDivSqrt extends Module with FDivSqrtOpType {
  val S = fp32
  val D = fp64
  val io = IO(new FDivSqrtIO(D.expWidth, D.sigWidth))
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src(0), io.in.bits.src(1), io.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): FPDataOut = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val sfdivsqrt = Module(new FDIV(S.expWidth, S.sigWidth))
  sfdivsqrt.io.in.bits.rm := io.in.bits.rm
  for(i <- 0 until 2){
    sfdivsqrt.io.in.bits.src(i) := unbox(io.in.bits.src(i), S)
  }
  sfdivsqrt.io.in.valid := valid && !io.flush && single(func)
  sfdivsqrt.io.specialIO.kill := io.flush
  sfdivsqrt.io.specialIO.isSqrt := isSqrt(func)
  sfdivsqrt.io.out.ready := io.out.ready

  val dfdivsqrt = Module(new FDIV(D.expWidth, D.sigWidth))
  dfdivsqrt.io.in.bits := io.in.bits
  dfdivsqrt.io.in.valid := valid && !io.flush && !single(func)
  dfdivsqrt.io.specialIO.kill := io.flush
  dfdivsqrt.io.specialIO.isSqrt := isSqrt(func)
  dfdivsqrt.io.out.ready := io.out.ready

  io.in.ready := sfdivsqrt.io.in.ready && dfdivsqrt.io.in.ready
  io.out.valid := Mux(single(func), sfdivsqrt.io.out.valid, dfdivsqrt.io.out.valid)
  io.out.bits.result := Mux(single(func), box(sfdivsqrt.io.out.bits.result, D), dfdivsqrt.io.out.bits.result)
  io.out.bits.fflags := Mux(single(func), sfdivsqrt.io.out.bits.fflags, dfdivsqrt.io.out.bits.fflags)

  }

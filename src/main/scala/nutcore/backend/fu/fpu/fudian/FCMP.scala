package nutcore.backend.fu.fpu.fudian

import chisel3._
import chisel3.util._

class FCMPOut extends Bundle {
  val eq, le, lt = Output(Bool())
  val fflags = Output(UInt(5.W))
}

class FCMP(val expWidth: Int, val sigWidth: Int) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(new FPDataIn(expWidth, sigWidth, nelem = 2))
    val out = new FCMPOut
  })

  val (a, b) = (io.in.src(0), io.in.src(1))
  val fp_a = FloatPoint.fromUInt(a, expWidth, sigWidth)
  val fp_b = FloatPoint.fromUInt(b, expWidth, sigWidth)
  val decode_a = fp_a.decode
  val decode_b = fp_b.decode

  val hasNaN = decode_a.isNaN || decode_b.isNaN
  val hasSNaN = decode_a.isSNaN || decode_b.isSNaN
  val bothZero = decode_a.isZero && decode_b.isZero

  val same_sign = fp_a.sign === fp_b.sign
  val a_minus_b = Cat(0.U(1.W), a) - Cat(0.U(1.W), b)
  val uint_eq = a_minus_b.tail(1) === 0.U
  val uint_less = fp_a.sign ^ a_minus_b.head(1).asBool

  val invalid = hasSNaN || (io.in.rm(1).asBool && hasNaN)

  io.out.eq := !hasNaN && (uint_eq || bothZero)
  io.out.le := !hasNaN && Mux(
    same_sign,
    uint_less || uint_eq,
    fp_a.sign || bothZero
  )
  io.out.lt := !hasNaN && Mux(
    same_sign,
    uint_less && !uint_eq,
    fp_a.sign && !bothZero
  )
  io.out.fflags := Cat(invalid, 0.U(4.W))
}

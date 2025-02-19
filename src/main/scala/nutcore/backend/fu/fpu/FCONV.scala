package nutcore

import chisel3._
import chisel3.util._
import nutcore.backend.fu.fpu.fudian._
import float._

trait FCONVOpType {
  def fcvt_w_s = "b0000_001".U
  def fcvt_wu_s = "b0001_001".U
  def fcvt_s_w = "b0000_010".U
  def fcvt_s_wu = "b0001_010".U
  def fcvt_l_s = "b0010_001".U
  def fcvt_lu_s = "b0011_001".U
  def fcvt_s_l = "b0010_010".U
  def fcvt_s_lu = "b0011_010".U
  def fmv_x_w = "b0100_001".U
  def fmv_w_x = "b0100_010".U
  def fclass_s = "b0110_001".U
  def fsgnj_s = "b0100_100".U
  def fsgnjn_s = "b0101_100".U
  def fsgnjx_s = "b0110_100".U

  def fcvt_s_d = "b0000_100".U
  def fcvt_d_s = "b1000_100".U
  def fcvt_w_d = "b1000_001".U
  def fcvt_wu_d = "b1001_001".U
  def fcvt_d_w  = "b1000_010".U
  def fcvt_d_wu = "b1001_010".U
  def fcvt_l_d  = "b1010_001".U
  def fcvt_lu_d = "b1011_001".U
  def fcvt_d_l  = "b1010_010".U
  def fcvt_d_lu = "b1011_010".U
  def fsgnj_d  = "b1100_100".U
  def fsgnjn_d = "b1101_100".U
  def fsgnjx_d = "b1110_100".U
  def fmv_x_d = "b1100_001".U
  def fmv_d_x = "b1100_010".U
  def fclass_d = "b1110_001".U

  def FpToInt(op: UInt): Bool = op(0)
  def IntToFp(op: UInt): Bool = op(1)
  def FpToFp(op: UInt): Bool = op(2)
  def isUnsigned(op: UInt): Bool = op(3)
  def isLong(op: UInt): Bool = op(4)
  def isOther(op: UInt): Bool = op(5)
  def isDouble(op: UInt):Bool = op(6)

  def isSgnj(op: UInt): Bool = isOther(op) && FpToFp(op) //privilege sgnjn sgnjx
  def isSgnjn(op: UInt): Bool = isSgnj(op) && op(3)
  def isSgnjx(op: UInt): Bool = isSgnj(op) && op(4)
  def isClass(op: UInt): Bool = isOther(op) && FpToInt(op) && op(4)
  def mvIntToFp(op: UInt): Bool = isOther(op) && IntToFp(op)
  def mvFpToInt(op: UInt): Bool = isOther(op) && FpToInt(op) && !op(4)
  def SToD(op: UInt): Bool = isDouble(op) && FpToFp(op) && !isOther(op)
  def DToS(op: UInt): Bool = !isDouble(op) && FpToFp(op) && !isOther(op)
  def cvtFpToInt(op: UInt): Bool = !isOther(op) && FpToInt(op)
  def cvtIntToFp(op: UInt): Bool = !isOther(op) && IntToFp(op)
}

class FCONVIO(expWidth: Int, sigWidth: Int) extends Bundle {
  val in = Flipped(DecoupledIO(new FPDataIn(expWidth, sigWidth, 2)))
  val out = DecoupledIO(new FPDataOut(expWidth, sigWidth))
  val func = Input(UInt(7.W))
  val flush = Input(Bool())
  //  val decode = new DecodeIO
}

class FCONV extends Module with FCONVOpType {
  val S = fp32
  val D = fp64
  val io = IO(new FCONVIO(D.expWidth, D.sigWidth))

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src(0), io.in.bits.src(1), io.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): FPDataOut = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val double = isDouble(io.func)
  val isOthers = isOther(io.func)
  val s2d = SToD(io.func)
  val d2s = DToS(io.func)
  val unsigned = isUnsigned(io.func)
  val long = isLong(io.func)

  val sgnj = isSgnj(io.func)
  val sgnjn = isSgnjn(io.func)
  val sgnjx = isSgnjx(io.func)
  val isClassify = isClass(io.func)
  val mvToX = mvFpToInt(io.func)
  val mvFromX = mvIntToFp(io.func)
  val cvtToX = cvtFpToInt(io.func)
  val cvtFromX = cvtIntToFp(io.func)

  val unboxed_src1 = unbox(src1, S)
  val unboxed_src2 = unbox(src2, S)
  /*
   * op: 00 => f -> wu
   * 01 => f -> w
   * 10 => f -> lu
   * 11 => f -> l
   */
  val in_sign = !unsigned && Mux(long, src1.head(1).asBool, src1(S.len - 1).asBool)
  val sToInt = Module(new FPToInt(S.expWidth, S.sigWidth))
  sToInt.io.in.src := VecInit(unboxed_src1)
  sToInt.io.in.rm := io.in.bits.rm
  sToInt.io.op := Cat(long, !unsigned)

  val intToS = Module(new IntToFP(S.expWidth, S.sigWidth))
  intToS.io.in.int := src1
  intToS.io.in.sign := in_sign
  intToS.io.in.long := long
  intToS.io.rm := io.in.bits.rm

  val dToInt = Module(new FPToInt(D.expWidth, D.sigWidth))
  dToInt.io.in.src := VecInit(src1)
  dToInt.io.in.rm := io.in.bits.rm
  dToInt.io.op := Cat(long, !unsigned)

  val intToD = Module(new IntToFP(D.expWidth, D.sigWidth))
  intToD.io.in.int := src1
  intToD.io.in.sign := in_sign
  intToD.io.in.long := long
  intToD.io.rm := io.in.bits.rm

  val sToD = Module(new FPToFP(S.expWidth, S.sigWidth, D.expWidth, D.sigWidth))
  sToD.io.in := unboxed_src1
  sToD.io.roundingMode := io.in.bits.rm
  val dToS = Module(new FPToFP(D.expWidth, D.sigWidth, S.expWidth, S.sigWidth))
  dToS.io.in := src1
  dToS.io.roundingMode := io.in.bits.rm
  /*------------------------------------------------------------------------
  Others(000)
  *------------------------------------------------------------------------*/
  def classify(ftype: FType): UInt = {
    val a = if(ftype == S) unboxed_src1 else src1
    val f = FloatPoint.fromUInt(a, ftype.expWidth, ftype.sigWidth)
    val decode = f.decode
    val isNormal = !decode.expIsOnes && !decode.expIsZero
    Cat(
      decode.isQNaN,
      decode.isSNaN,
      decode.isInf && !f.sign,
      isNormal && !f.sign,
      decode.isSubnormal && !f.sign,
      decode.isZero && !f.sign,
      decode.isZero && f.sign,
      decode.isSubnormal && f.sign,
      isNormal && f.sign,
      decode.isInf && f.sign
    )
  }

  def sgnj_result(ftype: FType) = {
    val a = if(ftype == S) unboxed_src1 else src1
    val b = if(ftype == S) unboxed_src2 else src2
    val result = PriorityMux(
      Seq(
        sgnjx -> Cat(a.head(1) ^ b.head(1), a(ftype.len - 2, 0)),
        sgnjn -> Cat(!b.head(1), a(ftype.len - 2, 0)),
        sgnj -> Cat(b.head(1), a(ftype.len - 2, 0)),
      )
    )
    result
  }

  //  io.decode := DontCare
  //  io.decode.ctrl.fReg.wen := !fromDS || fp2fp
  io.in.ready := io.out.ready || io.flush
  io.out.valid := valid
  io.out.bits.result := Mux1H(
    Seq(
      isClassify -> Mux(double, classify(D), classify(S)),
      sgnj -> Mux(double, sgnj_result(D), box(sgnj_result(S), D)),
      mvToX -> Mux(double, src1, Cat(Fill(S.len, unboxed_src1.head(1)), unboxed_src1)),
      mvFromX -> Mux(double, src1, box(src1(S.len - 1, 0), D)),
      d2s -> box(dToS.io.result, D),
      s2d -> sToD.io.result,
      cvtToX -> Mux(double, dToInt.io.out.result, sToInt.io.out.result),
      cvtFromX -> Mux(double, intToD.io.out.result, box(intToS.io.out.result, D))
    )
  )
  io.out.bits.fflags := Mux1H(
    Seq(
      isOthers -> 0.U(5.W),
      d2s -> dToS.io.fflags,
      s2d -> sToD.io.fflags,
      cvtToX -> Mux(double, dToInt.io.out.fflags, sToInt.io.out.fflags),
      cvtFromX -> Mux(double, intToD.io.out.fflags, intToS.io.out.fflags)
    )
  )
}

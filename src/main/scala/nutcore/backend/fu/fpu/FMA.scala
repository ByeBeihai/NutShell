package nutcore

import chisel3._
import chisel3.util._
import nutcore.backend.fu.fpu.fudian._
import float._
import chisel3.util.experimental.BoringUtils

trait FMAOpType {
  def fadd_s = "b100_0010".U
  def fsub_s = "b100_0011".U
  def fmul_s = "b100_0100".U
  def fmadd_s = "b100_0110".U
  def fmsub_s = "b100_0111".U
  def fnmsub_s = "b100_1111".U
  def fnmadd_s = "b100_1110".U

  def fadd_d = "b000_0010".U
  def fsub_d = "b000_0011".U
  def fmul_d = "b000_0100".U
  def fmadd_d = "b000_0110".U
  def fmsub_d = "b000_0111".U
  def fnmsub_d = "b000_1111".U
  def fnmadd_d = "b000_1110".U

  def add(op: UInt): Bool = op(1)
  def mul(op: UInt): Bool = op(2)
  def sub(op: UInt): Bool = op(0)
  def inv(op: UInt): Bool  = op(3)
  def isSingle(op: UInt): Bool = op(6)
}

class FMA_CtrlSignal extends Bundle{
  val useAdd = Input(Bool())
  val useMul = Input(Bool())
  val isSub = Input(Bool())
  val isInv = Input(Bool())
}
class FMA_impl(ftype: FType)(implicit val p:NutCoreConfig)extends Module {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new FPDataIn(ftype.expWidth, ftype.sigWidth, 3)))
    val ctrl = new FMA_CtrlSignal
    val out = Decoupled(new FPDataOut(ftype.expWidth, ftype.sigWidth))
  })

  val (a, b, c) = (io.in.bits.src(0), io.in.bits.src(1), io.in.bits.src(2))
  val rm = io.in.bits.rm
  val useAdd = io.ctrl.useAdd
  val useMul = io.ctrl.useMul
  val isSub = io.ctrl.isSub
  val isInv = io.ctrl.isInv

  val fmulInValid = useMul && io.in.valid
  val fmulResult = Wire(new FMULOut(ftype.expWidth, ftype.sigWidth))
  val fmulOutValid, fmulInReady, faddOutValid, faddInReady = Wire(Bool())
  val fmul = FMUL(a, b, rm, ftype.expWidth, ftype.sigWidth, fmulInValid, faddInReady)
  fmulResult := fmul._1
  fmulOutValid := fmul._2
  fmulInReady := fmul._3

  val handshaked = fmulOutValid && faddInReady && useAdd
  val mul2add_valid = RegInit(false.B)
  
  when(handshaked) {
    mul2add_valid := true.B
  }
  when(faddOutValid && io.out.ready) {
    mul2add_valid := false.B
  }
  when(io.flush){
    mul2add_valid := false.B
  }

  val faddInValid = (!useMul && io.in.valid) || mul2add_valid
  val a_inter_fflags = RegEnable(fmulResult.tofadd.inter_flags, handshaked)

  val mul2add_width = ftype.expWidth + 2 * ftype.sigWidth
  def padd_tail(x: UInt, w: Int): UInt = Cat(x, 0.U((w - x.getWidth).W))
  val faddIn = Wire(Vec(2, UInt(mul2add_width.W)))
  faddIn(0) := Mux(useMul, RegEnable(fmulResult.tofadd.fp_prod, handshaked), padd_tail(a, mul2add_width))
  faddIn(1) := padd_tail(Mux(useMul, c, b), mul2add_width)
  val fadd = FADD(faddIn(0), faddIn(1), rm, isSub, isInv, ftype.expWidth, 2 * ftype.sigWidth, ftype.sigWidth,
    faddInValid, io.out.ready, mul2add_valid, false.B, Some(a_inter_fflags))
  faddOutValid := fadd._3
  faddInReady := fadd._4

  io.in.ready := DontCare
  io.out.valid := Mux(useAdd, fadd._3, fmulOutValid)
  io.out.bits.result := Mux(useAdd, fadd._1, fmulResult.result)
  io.out.bits.fflags := Mux(useAdd, fadd._2, fmulResult.fflags)

  if(p.FPGAPlatform){
    if(ftype == fp32){
      //BoringUtils.addSource(io.out.valid,"ilafp32outvalid")
    }else{
      //BoringUtils.addSource(io.out.valid,"ilafp64outvalid")
      //BoringUtils.addSource(io.in.valid, "ilafp64invalid")
      //BoringUtils.addSource(mul2add_valid, "ilafp64mul2add_valid")
      //BoringUtils.addSource(handshaked, "ilafp64handshaked")
      //BoringUtils.addSource(useAdd, "ilafp64useadd")
      //BoringUtils.addSource(useMul, "ilafp64usemul")
      //BoringUtils.addSource(faddOutValid, "ilafp64faddOutValid")
      //BoringUtils.addSource(fmulOutValid, "ilafp64fmulOutValid")
      //BoringUtils.addSource(faddInReady, "ilafp64faddInReady")
    }
  }
}

class FMAIO(ftype: FType) extends Bundle {
  val in = Flipped(DecoupledIO(new FPDataIn(ftype.expWidth, ftype.sigWidth, 3)))
  val func = Input(UInt(7.W))
  val flush = Input(Bool())
  val out = DecoupledIO(new FPDataOut(ftype.expWidth, ftype.sigWidth))
}

class FMA(implicit val p:NutCoreConfig) extends Module with FMAOpType {
  val S = fp32
  val D = fp64
  val io = IO(new FMAIO(D))
  val (valid, a, b, c, func) = (io.in.valid, io.in.bits.src(0), io.in.bits.src(1), io.in.bits.src(2), io.func)
  def access(valid: Bool, src1: UInt, src2: UInt, src3: UInt, func: UInt): FPDataOut = {
    this.valid := valid
    this.a := src1
    this.b := src2
    this.c := src3
    this.func := func
    io.out.bits
  }

  val fma_ctrl = Wire(new FMA_CtrlSignal)
  fma_ctrl.isInv := inv(func)
  fma_ctrl.isSub := sub(func)
  fma_ctrl.useAdd := add(func)
  fma_ctrl.useMul := mul(func)

  val sfma = Module(new FMA_impl(S))
  for(i <- 0 until 3){
    sfma.io.in.bits.src(i) := unbox(io.in.bits.src(i), S)
  }
  sfma.io.in.bits.rm := io.in.bits.rm
  sfma.io.in.valid := isSingle(func) && io.in.valid
  sfma.io.ctrl := fma_ctrl
  sfma.io.out.ready := io.out.ready
  sfma.io.flush := io.flush

  val dfma = Module(new FMA_impl(D))
  dfma.io.in.bits := io.in.bits
  dfma.io.in.valid := !isSingle(func) && io.in.valid
  dfma.io.ctrl := fma_ctrl
  dfma.io.out.ready := io.out.ready
  dfma.io.flush := io.flush

  val s_idle :: s_exec :: s_wait :: Nil = Enum(3)
  val state = RegInit(s_idle)

  when(io.flush) {
    state := s_idle
  }.elsewhen(io.in.fire) {
    state := s_exec
  }.elsewhen(io.out.fire) {
    state := s_idle
  }.elsewhen(io.out.valid && !io.out.fire()) {
    state := s_wait
  }.elsewhen(state === s_wait && io.out.fire()) {
    state := s_idle
  } otherwise {
    state := state
  }

  io.in.ready := state === s_idle
  io.out.valid := sfma.io.out.valid || dfma.io.out.valid
  io.out.bits.result := Mux(isSingle(func), box(sfma.io.out.bits.result, D), dfma.io.out.bits.result)
  io.out.bits.fflags := Mux(isSingle(func), sfma.io.out.bits.fflags, dfma.io.out.bits.fflags)
  
  if(p.FPGAPlatform){
    //BoringUtils.addSource(dfma.io.out.valid,"ilaDFMAoutValid")
    //BoringUtils.addSource(sfma.io.out.valid,"ilaSFMAoutValid")
  }
}


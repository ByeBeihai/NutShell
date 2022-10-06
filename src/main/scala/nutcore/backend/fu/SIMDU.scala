package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import difftest._
import top.Settings

object SIMDUOpType {
  def add16  = "b0100000".U
  def radd16 = "b0000000".U
  def kadd16 = "b0001000".U
  def uradd16= "b0010000".U
  def ukadd16= "b0011000".U
  def sub16  = "b0100001".U
  def rsub16 = "b0000001".U
  def ksub16 = "b0001001".U
  def ursub16= "b0010001".U
  def uksub16= "b0011001".U
  def add8   = "b0100100".U
  def radd8  = "b0000100".U
  def kadd8  = "b0001100".U
  def uradd8 = "b0010100".U
  def ukadd8 = "b0011100".U
  def sub8   = "b0100101".U
  def rsub8  = "b0000101".U
  def ksub8  = "b0001101".U
  def ursub8 = "b0010101".U
  def uksub8 = "b0011101".U
  def cras16 = "b0100010".U
  def rcras16= "b0000010".U
  def kcras16= "b0001010".U
  def urcras16="b0010010".U
  def ukcras16="b0011010".U
  def crsa16 = "b0100011".U
  def rcrsa16= "b0000011".U
  def kcrsa16= "b0001011".U
  def urcrsa16="b0010011".U
  def ukcrsa16="b0011011".U
  def add32  = "b0100000".U
  def radd32 = "b0000000".U
  def kadd32 = "b0001000".U
  def uradd32= "b0010000".U
  def ukadd32= "b0011000".U
  def sub32  = "b0100001".U
  def rsub32 = "b0000001".U
  def ksub32 = "b0001001".U
  def ursub32= "b0010001".U
  def uksub32= "b0011001".U
  def cras32 = "b0100010".U
  def rcras32= "b0000010".U
  def kcras32= "b0001010".U
  def urcras32="b0010010".U
  def ukcras32="b0011010".U
  def crsa32 = "b0100011".U
  def rcrsa32= "b0000011".U
  def kcrsa32= "b0001011".U
  def urcrsa32="b0010011".U
  def ukcrsa32="b0011011".U
}

class SIMDU_IO extends FunctionUnitIO {
  val flush = Input(Bool())
  val DecodeOut = new DecodeIO
  val DecodeIn = Flipped(new DecodeIO)
  val FirstStageFire = Output(Bool())
}
class SIMDU(hasBru: Boolean = false,NO1: Boolean = true) extends NutCoreModule {
  val io = IO(new SIMDU_IO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val PALU = Module(new PALU)

  io.in.ready := !valid || PALU.io.in.fire()

  io.DecodeOut.pext.OV := PALU.io.out.bits.DecodeOut.pext.OV
  io.out.bits := PALU.io.out.bits.result
  io.DecodeOut := PALU.io.out.bits.DecodeOut
  io.out.valid := PALU.io.out.valid
  PALU.io.out.ready := io.out.ready
  io.FirstStageFire := valid && PALU.io.in.ready

  val PALU_bits_next = Wire(new DecodeIO)
  val PALU_bits      = RegInit(0.U.asTypeOf(new DecodeIO))
  PALU_bits_next := PALU_bits
  val PALU_valid = RegInit(0.U.asTypeOf(Bool()))
  val PALU_valid_next = Wire(Bool())
  PALU_valid_next:= PALU_valid
  when(PALU.io.out.fire()){PALU_valid_next := false.B}
  when(valid && PALU.io.in.ready){PALU_valid_next := true.B
                                  PALU_bits_next  := io.DecodeIn}
  when(io.flush){PALU_valid_next := false.B}
  PALU_valid := PALU_valid_next
  PALU_bits  := PALU_bits_next
  PALU.io.in.valid := PALU_valid
  PALU.io.in.bits  := PALU_bits
}
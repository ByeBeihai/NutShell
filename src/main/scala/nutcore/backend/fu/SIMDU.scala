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
}

class SIMDU_IO extends FunctionUnitIO {
  val flush = Input(Bool())
  val DecodeOut = new DecodeIO
  val DecodeIn = Flipped(new DecodeIO)
  val OV = Output(Bool())
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

  io.OV := PALU.io.out.bits.OV
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
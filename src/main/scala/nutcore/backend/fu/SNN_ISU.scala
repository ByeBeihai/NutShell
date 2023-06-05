package nutcore 

import chisel3._
import chisel3.util._

import utils._

object SRFAddr {
    def VR  = "b00".U
    def LR  = "b01".U
    def TAU = "b10".U
    def ACC = "b11".U
}

class SCtrlIO extends NutCoreBundle with HasNutCoreConst{
    val isNup = Output(Bool())
    val isBpo = Output(Bool())
    val isExp = Output(Bool())
    val isTdr = Output(Bool())
    val isSum = Output(Bool())
    val isSvr = Output(Bool())
    val hasTs = Output(Bool())
    val hasAcc = Output(Bool())
    val DIn1 = Output(Vec(4, UInt(16.W)))
    val DIn2 = Output(Vec(4, UInt(16.W)))
    val SRF4 = Output(Vec(4, UInt(64.W))) 
}

class SNNISU(len: Int = 16) extends NutCoreModule{
    val io = IO(new Bundle{
        val valid = Input(Bool())
        val dcIn = Flipped(new DecodeIO) // input
        val SCtrl= new SCtrlIO // output
        val res = Output(UInt(XLEN.W))
        val sumValid = Output(Bool())
        val dcOut = new DecodeIO
    })

    val src1  = io.dcIn.data.src1
    val src2  = io.dcIn.data.src2
    val func  = io.dcIn.ctrl.fuOpType

    val srf = new SRegFile

    io.dcOut := io.dcIn

    io.SCtrl.isNup  := func === SNNOpType.nup
    io.SCtrl.isBpo  := func === SNNOpType.bpo 
    io.SCtrl.isExp  := func === SNNOpType.exp 
    io.SCtrl.isTdr  := func === SNNOpType.tdr 
    io.SCtrl.isSum  := func === SNNOpType.sum 
    io.SCtrl.isSvr  := func === SNNOpType.svr 
    def isSrfWr(isSlr: Bool, isSvr: Bool, isStau: Bool):Bool = isSlr || isSvr || isStau
    io.SCtrl.hasTs  := func === SNNOpType.nup && io.dcIn.cf.instr(25) === "b1".U
    io.SCtrl.hasAcc := func === SNNOpType.sum && io.dcIn.cf.instr(25) === "b1".U
    
    
    for(i <- 0 until( XLEN/ len)){
        io.SCtrl.DIn1(i) := src1(len * i + len - 1, len * i)
        io.SCtrl.DIn2(i) := src2(len * i + len - 1, len * i)
        io.SCtrl.SRF4(i) := srf.read(i.U)
    }
    io.sumValid := DontCare
    when(io.SCtrl.isSvr){
        io.res := 1.U
        srf.write(SRFAddr.TAU, ZeroExt(io.SCtrl.DIn1(SRFAddr.TAU), 64))
        srf.write(SRFAddr.VR, ZeroExt(io.SCtrl.DIn1(SRFAddr.VR), 64))
        srf.write(SRFAddr.LR, ZeroExt(io.SCtrl.DIn1(SRFAddr.LR), 64))
        srf.write(SRFAddr.ACC, src2)
    }.elsewhen(io.SCtrl.isSum){
        val ksiw03 = RegInit(VecInit(Seq.fill(4)(0.U(16.W))))
        val sumvalid = RegInit(false.B)
        sumvalid := io.valid
        val acc = srf.read(SRFAddr.ACC)
        for(i <- 0 until XLEN/len){
          ksiw03(i) := Mux(io.SCtrl.DIn2(i) === 1.U, io.SCtrl.DIn1(i), 0.U(16.W))
        }

        io.sumValid := sumvalid && io.valid
        io.res := Mux(io.SCtrl.hasAcc && io.sumValid, ksiw03.reduce(_ + _) + acc, ksiw03.reduce(_ + _))
        when(io.sumValid){
            srf.write(SRFAddr.ACC, io.res)
        }
    }.otherwise{
        io.sumValid := io.valid
        io.res := 0.U
    }

    for(i <- 0 to 3){
        Debug("[SNN_ISU] srf(%d): %x DIn1(i): %x DIn2(i): %x hasAcc: %b hasTs: %b\n", i.U, io.SCtrl.SRF4(i.U), io.SCtrl.DIn1(i.U), io.SCtrl.DIn2(i.U), io.SCtrl.hasAcc, io.SCtrl.hasTs)
    }
}
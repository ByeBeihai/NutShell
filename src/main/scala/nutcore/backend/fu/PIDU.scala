package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import difftest._
import top.Settings

class PIDUIO extends NutCoreBundle {
    val isAdd_64 = Output(Bool())
    val isAdd_32 = Output(Bool())
    val isAdd_16 = Output(Bool())
    val isAdd_8  = Output(Bool())
    val isAdd_Q15= Output(Bool())
    val isAdd_Q31= Output(Bool())
    val isAdd_C31= Output(Bool())
    val isAve    = Output(Bool())
    val isAdd    = Output(Bool())
    val isSub_64 = Output(Bool())
    val isSub_32 = Output(Bool())
    val isSub_16 = Output(Bool())
    val isSub_8  = Output(Bool())
    val isSub_Q15= Output(Bool())
    val isSub_Q31= Output(Bool())
    val isSub_C31= Output(Bool())
    val isCras_16= Output(Bool())
    val isCrsa_16= Output(Bool())
    val isCras_32= Output(Bool())
    val isCrsa_32= Output(Bool())
    val isCr     = Output(Bool())
    val isStas_16= Output(Bool())
    val isStsa_16= Output(Bool())
    val isStas_32= Output(Bool())
    val isStsa_32= Output(Bool())
    val isSt     = Output(Bool())
    val isComp_16= Output(Bool())
    val isComp_8 = Output(Bool())
    val isCompare= Output(Bool())
    val isMaxMin_16= Output(Bool())
    val isMaxMin_8 = Output(Bool())
    val isMaxMin_XLEN= Output(Bool())
    val isMaxMin_32= Output(Bool())
    val isMaxMin = Output(Bool())
    val isPbs = Output(Bool())
    val isRs_16  = Output(Bool())
    val isLs_16  = Output(Bool())
    val isLR_16  = Output(Bool())
    val isRs_8   = Output(Bool())
    val isLs_8   = Output(Bool())
    val isLR_8   = Output(Bool())
    val isRs_32  = Output(Bool())
    val isLs_32  = Output(Bool())
    val isLR_32  = Output(Bool())
    val isLR_Q31 = Output(Bool())
    val isLs_Q31 = Output(Bool())
    val isRs_XLEN= Output(Bool())
    val isSRAIWU = Output(Bool())
    val isFSRW   = Output(Bool())
    val isWext   = Output(Bool())
    val isShifter= Output(Bool())
    val isClip_16= Output(Bool())
    val isClip_8 = Output(Bool())
    val isclip_32= Output(Bool())
    val isClip   = Output(Bool())
    val isSat_16 = Output(Bool())
    val isSat_8  = Output(Bool())
    val isSat_32 = Output(Bool())
    val isSat_W  = Output(Bool())
    val isSat    = Output(Bool())
    val isCnt_16 = Output(Bool())
    val isCnt_8  = Output(Bool())
    val isCnt_32 = Output(Bool())
    val isCnt    = Output(Bool())
    val isSwap_16= Output(Bool())
    val isSwap_8 = Output(Bool())
    val isSwap   = Output(Bool())
    val isUnpack = Output(Bool())
    val isBitrev = Output(Bool())
    val isCmix   = Output(Bool())
    val isInsertb= Output(Bool())
    val isPackbb = Output(Bool())
    val isPackbt = Output(Bool())
    val isPacktb = Output(Bool())
    val isPacktt = Output(Bool())
    val isPack   = Output(Bool())
}
class PIDU() extends NutCoreModule with HasInstrType{
    val io = IO(new Bundle{
        val DecodeIn = Flipped(new DecodeIO)
        val Pctrl = new PIDUIO
    })
    
    val src1  = io.DecodeIn.data.src1
    val src2  = io.DecodeIn.data.src2
    val src3  = io.DecodeIn.data.src3
    val func  = io.DecodeIn.ctrl.fuOpType
    val funct3= io.DecodeIn.ctrl.funct3
    val func24= io.DecodeIn.ctrl.func24
    val func23= io.DecodeIn.ctrl.func23

    io.Pctrl.isAdd_64 := func(6,5) === "b10".U && func(2,0) === "b000".U && funct3 === "b001".U 
    io.Pctrl.isAdd_32 := func(2,0).asUInt === 0.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 2.U//false.B//VALUOpType.add32 === func
    io.Pctrl.isAdd_16 := func(2,0).asUInt === 0.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U
    io.Pctrl.isAdd_8  := func(2,0).asUInt === 4.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//false.B//VALUOpType.add8 === func
    io.Pctrl.isAdd_Q15:= func(6,4) === "b000".U && func(2,0) === "b010".U && funct3 === 1.U
    io.Pctrl.isAdd_Q31:= func(6,4) === "b000".U && func(2,0) === "b000".U && funct3 === 1.U
    io.Pctrl.isAdd_C31:= func(6,4) === "b001".U && func(2,0) === "b000".U && funct3 === 1.U
    io.Pctrl.isAve    := func === "b1110000".U && funct3 === "b000".U
    io.Pctrl.isAdd := io.Pctrl.isAdd_64 | io.Pctrl.isAdd_32 | io.Pctrl.isAdd_16 | io.Pctrl.isAdd_8 | io.Pctrl.isAdd_Q15 | io.Pctrl.isAdd_Q31 | io.Pctrl.isAdd_C31 | io.Pctrl.isAve

    io.Pctrl.isSub_64 := func(6,5) === "b10".U && func(2,0) === "b001".U && funct3 === "b001".U 
    io.Pctrl.isSub_32 := func(2,0).asUInt === 1.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 2.U//false.B//VALUOpType.sub32 === io.in.func
    io.Pctrl.isSub_16 := func(2,0).asUInt === 1.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//VALUOpType.sub16 === io.in.func
    io.Pctrl.isSub_8  := func(2,0).asUInt === 5.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//VALUOpType.sub8 === io.in.func
    io.Pctrl.isSub_Q15:= func(6,4) === "b000".U && func(2,0) === "b011".U && funct3 === 1.U
    io.Pctrl.isSub_Q31:= func(6,4) === "b000".U && func(2,0) === "b001".U && funct3 === 1.U
    io.Pctrl.isSub_C31:= func(6,4) === "b001".U && func(2,0) === "b001".U && funct3 === 1.U 

    io.Pctrl.isCras_16 := func(2,0).asUInt === 2.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 0.U
    io.Pctrl.isCrsa_16 := func(2,0).asUInt === 3.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 0.U
    io.Pctrl.isCras_32 := func(2,0).asUInt === 2.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 2.U
    io.Pctrl.isCrsa_32 := func(2,0).asUInt === 3.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 2.U
    io.Pctrl.isCr := io.Pctrl.isCras_16 | io.Pctrl.isCrsa_16 | io.Pctrl.isCras_32 | io.Pctrl.isCrsa_32

    io.Pctrl.isStas_16 := (func(6,5) === "b11".U || func(6,4) === "b101".U) && func(2,0) === "b010".U && funct3 === "b010".U
    io.Pctrl.isStsa_16 := (func(6,5) === "b11".U || func(6,4) === "b101".U) && func(2,0) === "b011".U && funct3 === "b010".U
    io.Pctrl.isStas_32 := (func(6,5) === "b11".U || func(6,4) === "b101".U) && func(2,0) === "b000".U && funct3 === "b010".U
    io.Pctrl.isStsa_32 := (func(6,5) === "b11".U || func(6,4) === "b101".U) && func(2,0) === "b001".U && funct3 === "b010".U
    io.Pctrl.isSt := io.Pctrl.isStas_16 | io.Pctrl.isStsa_16 | io.Pctrl.isStas_32 | io.Pctrl.isStsa_32

    io.Pctrl.isComp_16 := func(2,0).asUInt === 6.U && (func(6,5) === 0.U || func(6,3) === 4.U) && funct3 === 0.U 
    io.Pctrl.isComp_8  := func(2,0).asUInt === 7.U && (func(6,5) === 0.U || func(6,3) === 4.U) && funct3 === 0.U 
    io.Pctrl.isCompare := io.Pctrl.isComp_16 | io.Pctrl.isComp_8

    io.Pctrl.isMaxMin_16  := func(6,4) === 4.U && func(2,1) === 0.U && funct3 === 0.U
    io.Pctrl.isMaxMin_8   := func(6,4) === 4.U && func(2,1) === 2.U && funct3 === 0.U
    io.Pctrl.isMaxMin_XLEN:= func === "b0000101".U && funct3(2) === "b1".U  && funct3(0) === "b0".U && io.DecodeIn.cf.instr(6,0) === "b0110011".U
    io.Pctrl.isMaxMin_32  := (func(6,3) === "b1001".U || func(6,3) === "b1010".U) && func(2,1) === 0.U && funct3 === "b010".U
    io.Pctrl.isMaxMin     := io.Pctrl.isMaxMin_16 | io.Pctrl.isMaxMin_8 | io.Pctrl.isMaxMin_XLEN | io.Pctrl.isMaxMin_32

    io.Pctrl.isPbs     := func(6,1) === "b111111".U && funct3 === 0.U

    io.Pctrl.isRs_16 := func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,1) === 0.U && funct3 === 0.U
    io.Pctrl.isLs_16 := func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,0) === 2.U && funct3 === 0.U 
    io.Pctrl.isLR_16 :=(func(6,3) === 5.U || func(6,3) === 6.U)&& func(2,0) === 3.U && funct3 === 0.U
    io.Pctrl.isRs_8  := func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,1) === 2.U && funct3 === 0.U
    io.Pctrl.isLs_8  := func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,0) === 6.U && funct3 === 0.U
    io.Pctrl.isLR_8  :=(func(6,3) === 5.U || func(6,3) === 6.U)&& func(2,0) === 7.U && funct3 === 0.U
    io.Pctrl.isRs_32 :=((func(6,5)==="b01".U && func(4,3) =/= 0.U) || func(6,3) === 8.U) && func(2,1) === 0.U && funct3 === 2.U
    io.Pctrl.isLs_32 :=((func(6,5)==="b01".U && func(4,3) =/= 0.U) || func(6,3) === 8.U) && func(2,0) === 2.U && funct3 === 2.U
    io.Pctrl.isLR_32 :=(func(6,3) === 5.U || func(6,3) === 6.U) && func(2,0) === 3.U && funct3 === 2.U
    io.Pctrl.isLR_Q31:= func(6,4) === "b011".U && func(2,0) === "b111".U && funct3 === "b001".U
    io.Pctrl.isLs_Q31:= func(6,4) === "b001".U && func(2,0) === "b011".U && funct3 === "b001".U
    io.Pctrl.isRs_XLEN := (func(6,3) === "b0010".U || func(6,3)==="b1101".U) && func(2,1) === "b01".U && funct3 === "b001".U
    io.Pctrl.isSRAIWU:= func === "b0011010".U && funct3 === "b001".U
    io.Pctrl.isFSRW  := io.DecodeIn.cf.instr(26,25) === "b10".U  && funct3 === "b101".U   && func === "b0111011".U
    io.Pctrl.isWext    := func(6,4) === "b110".U && func(2,0) === "b111".U && funct3 === "b000".U
    io.Pctrl.isShifter := io.Pctrl.isRs_16 | io.Pctrl.isLs_16 | io.Pctrl.isLR_16 | io.Pctrl.isRs_8 | io.Pctrl.isLs_8 | io.Pctrl.isLR_8 | io.Pctrl.isRs_32 | io.Pctrl.isLs_32 | io.Pctrl.isLR_32 | io.Pctrl.isLs_Q31 | io.Pctrl.isLR_Q31 | io.Pctrl.isRs_XLEN | io.Pctrl.isSRAIWU | io.Pctrl.isFSRW | io.Pctrl.isWext

    io.Pctrl.isClip_16 := func === "b1000010".U & funct3 === 0.U
    io.Pctrl.isClip_8  := func === "b1000110".U & funct3 === 0.U
    io.Pctrl.isclip_32 := func(6,4) === "b111".U & func(2,0) === "b010".U & funct3 === 0.U
    io.Pctrl.isClip    := io.Pctrl.isClip_16 | io.Pctrl.isClip_8 | io.Pctrl.isclip_32

    io.Pctrl.isSat_16  := func === "b1010110".U && src2(4,0) === "b10001".U && funct3 === 0.U
    io.Pctrl.isSat_8   := func === "b1010110".U && src2(4,0) === "b10000".U && funct3 === 0.U
    io.Pctrl.isSat_32  := func === "b1010110".U && src2(4,0) === "b10010".U && funct3 === 0.U
    io.Pctrl.isSat_W   := func === "b1010110".U && src2(4,0) === "b10100".U && funct3 === 0.U
    io.Pctrl.isSat     := io.Pctrl.isSat_16 | io.Pctrl.isSat_8 | io.Pctrl.isSat_32 | io.Pctrl.isSat_W

    io.Pctrl.isCnt_16  := func === "b1010111".U && src2(4,1) === "b0100".U && funct3 === 0.U
    io.Pctrl.isCnt_8   := func === "b1010111".U && src2(4,1) === "b0000".U && funct3 === 0.U
    io.Pctrl.isCnt_32  := func === "b1010111".U && src2(4,1) === "b1100".U && funct3 === 0.U
    io.Pctrl.isCnt     := io.Pctrl.isCnt_16 | io.Pctrl.isCnt_8 | io.Pctrl.isCnt_32

    io.Pctrl.isSwap_16 := func(6,5) === 0.U && func (2,0) === "b111".U && funct3 === 1.U
    io.Pctrl.isSwap_8  := func === "b1010110".U && (src2(4,0) === "b11000".U && funct3 === 0.U && io.DecodeIn.cf.instrType === InstrPI || src2(5,0) === "b001000".U && funct3 === "b101".U && io.DecodeIn.cf.instrType === InstrPB)
    io.Pctrl.isSwap    := io.Pctrl.isSwap_16 | io.Pctrl.isSwap_8

    io.Pctrl.isUnpack  := func === "b1010110".U && (src2(4,3) === "b01".U || src2(4,0) === "b10011".U || src2(4,0) === "b10111".U) && funct3 === 0.U

    io.Pctrl.isBitrev  := (func(6,3) === "b1110".U && (func(2,0) === "b011".U || func(2,1) === "b10".U) && funct3 === 0.U
                       || func === "b0110101".U && funct3 === "b101".U && io.DecodeIn.cf.instr(6,0) === "b0010011".U && src2(4,0) === "b11111".U)

    io.Pctrl.isCmix    := io.DecodeIn.cf.instr(14,12) === "b001".U && io.DecodeIn.cf.instr(6,0) === "b0110011".U && io.DecodeIn.cf.instr(26,25) === "b11".U

    io.Pctrl.isInsertb := func === "b1010110".U && io.DecodeIn.cf.instr(24,23) === "b00".U && funct3 === "b000".U

    io.Pctrl.isPackbb  := func === "b0000100".U && funct3 === "b100".U && io.DecodeIn.cf.instr(6,0) === "b0110011".U
    io.Pctrl.isPackbt  := func === "b0001111".U && funct3 === "b010".U
    io.Pctrl.isPacktb  := func === "b0011111".U && funct3 === "b010".U
    io.Pctrl.isPacktt  := func === "b0100100".U && funct3 === "b100".U && io.DecodeIn.cf.instr(6,0) === "b0110011".U
    io.Pctrl.isPack    := io.Pctrl.isPackbb | io.Pctrl.isPackbt | io.Pctrl.isPacktb | io.Pctrl.isPacktt
}

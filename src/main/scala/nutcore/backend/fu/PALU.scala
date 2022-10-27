package nutcore

import chisel3._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import top.Settings
import difftest._

object VALUOpType {
    // rearrange OpType according to element size
    // mostly sync with VXUOpType
    def OpLen = 8
    def add8  = "b00000_00".U
    def add16 = "b00000_01".U
    def add32 = "b00000_10".U
    def add64 = "b00000_11".U
    
    def sub8  = "b00001_00".U
    def sub16 = "b00001_01".U
    def sub32 = "b00001_10".U
    def sub64 = "b00001_11".U
    
    // 00010 reserved for rsub
    
    def sll8  = "b00011_00".U
    def sll16 = "b00011_01".U
    def sll32 = "b00011_10".U
    def sll64 = "b00011_11".U
    
    def srl8  = "b00100_00".U
    def srl16 = "b00100_01".U
    def srl32 = "b00100_10".U
    def srl64 = "b00100_11".U
    
    def sra8  = "b00101_00".U
    def sra16 = "b00101_01".U
    def sra32 = "b00101_10".U
    def sra64 = "b00101_11".U
    
    
    // note: normal calculation does NOT use this!!!
    // thus this encoding may NOT be the most proper choice
    // may introduce confliction with other function
    def slt8  = "b00110_00".U
    def slt16 = "b00110_01".U
    def slt32 = "b00110_10".U
    def slt64 = "b00110_11".U
    
    def sltu8  = "b00111_00".U
    def sltu16 = "b00111_01".U
    def sltu32 = "b00111_10".U
    def sltu64 = "b00111_11".U
    
    
    
    // no extra handling needed
    def xor  = "b01100".U
    
    def or   = "b01011".U
    
    def and  = "b01010".U
    
    
    
    
}


class PALUIO extends NutCoreBundle {
    val in = Flipped(Decoupled(new DecodeIO))
    val out = Decoupled(new Bundle{
        val result = Output(UInt(XLEN.W))
        val DecodeOut = new DecodeIO
  })
}

class PALU extends NutCoreModule with HasInstrType{
    val io = IO(new PALUIO)

    val valid = io.in.valid
    val src1  = io.in.bits.data.src1
    val src2  = io.in.bits.data.src2
    val src3  = io.in.bits.data.src3
    val func  = io.in.bits.ctrl.fuOpType
    val funct3= io.in.bits.ctrl.funct3
    val func24= io.in.bits.ctrl.func24
    val func23= io.in.bits.ctrl.func23

    io.in.ready := !valid || io.out.fire()
    io.out.valid:= valid
    io.out.bits.DecodeOut := io.in.bits
    
    val isAdd_64 = func(6,5) === "b10".U && func(2,0) === "b000".U && funct3 === "b001".U 
    val isAdd_32 = func(2,0).asUInt === 0.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 2.U//false.B//VALUOpType.add32 === func
    val isAdd_16 = func(2,0).asUInt === 0.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U
    val isAdd_8  = func(2,0).asUInt === 4.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//false.B//VALUOpType.add8 === func
    val isAdd_Q15= func(6,4) === "b000".U && func(2,0) === "b010".U && funct3 === 1.U
    val isAdd_Q31= func(6,4) === "b000".U && func(2,0) === "b000".U && funct3 === 1.U
    val isAdd_C31= func(6,4) === "b001".U && func(2,0) === "b000".U && funct3 === 1.U
    val isAve    = func === "b1110000".U && funct3 === "b000".U
    val isAdd = isAdd_64 | isAdd_32 | isAdd_16 | isAdd_8 | isAdd_Q15 | isAdd_Q31 | isAdd_C31 | isAve

    val isSub_64 = func(6,5) === "b10".U && func(2,0) === "b001".U && funct3 === "b001".U 
    val isSub_32 = func(2,0).asUInt === 1.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 2.U//false.B//VALUOpType.sub32 === io.in.func
    val isSub_16 = func(2,0).asUInt === 1.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//VALUOpType.sub16 === io.in.func
    val isSub_8  = func(2,0).asUInt === 5.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//VALUOpType.sub8 === io.in.func
    val isSub_Q15= func(6,4) === "b000".U && func(2,0) === "b011".U && funct3 === 1.U
    val isSub_Q31= func(6,4) === "b000".U && func(2,0) === "b001".U && funct3 === 1.U
    val isSub_C31= func(6,4) === "b001".U && func(2,0) === "b001".U && funct3 === 1.U 

    val isCras_16 = func(2,0).asUInt === 2.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 0.U
    val isCrsa_16 = func(2,0).asUInt === 3.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 0.U
    val isCras_32 = func(2,0).asUInt === 2.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 2.U
    val isCrsa_32 = func(2,0).asUInt === 3.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 2.U
    val isCr = isCras_16 | isCrsa_16 | isCras_32 | isCrsa_32

    val isStas_16 = (func(6,5) === "b11".U || func(6,4) === "b101".U) && func(2,0) === "b010".U && funct3 === "b010".U
    val isStsa_16 = (func(6,5) === "b11".U || func(6,4) === "b101".U) && func(2,0) === "b011".U && funct3 === "b010".U
    val isStas_32 = (func(6,5) === "b11".U || func(6,4) === "b101".U) && func(2,0) === "b000".U && funct3 === "b010".U
    val isStsa_32 = (func(6,5) === "b11".U || func(6,4) === "b101".U) && func(2,0) === "b001".U && funct3 === "b010".U
    val isSt = isStas_16 | isStsa_16 | isStas_32 | isStsa_32

    val isComp_16 = func(2,0).asUInt === 6.U && (func(6,5) === 0.U || func(6,3) === 4.U) && funct3 === 0.U 
    val isComp_8  = func(2,0).asUInt === 7.U && (func(6,5) === 0.U || func(6,3) === 4.U) && funct3 === 0.U 
    val isCompare = isComp_16 | isComp_8

    val isMaxMin_16  = func(6,4) === 4.U && func(2,1) === 0.U && funct3 === 0.U
    val isMaxMin_8   = func(6,4) === 4.U && func(2,1) === 2.U && funct3 === 0.U
    val isMaxMin_XLEN= func === "b0000101".U && funct3(2) === "b1".U  && funct3(0) === "b0".U && io.in.bits.cf.instr(6,0) === "b0110011".U
    val isMaxMin_32  = (func(6,3) === "b1001".U || func(6,3) === "b1010".U) && func(2,1) === 0.U && funct3 === "b010".U
    val isMaxMin     = isMaxMin_16 | isMaxMin_8 | isMaxMin_XLEN | isMaxMin_32

    val isPbs     = func(6,1) === "b111111".U && funct3 === 0.U

    val isSub = WireInit(0.U(8.W))
    when(isSub_64 | isSub_32 | isSub_16 | isSub_8 | isComp_16 | isComp_8 | isMaxMin | isPbs | isSub_Q15 | isSub_Q31 | isSub_C31){
        isSub:= "b11111111".U
    }.elsewhen(isCras_16 | isStas_16){
        isSub:= "b00000101".U
    }.elsewhen(isCrsa_16 | isStsa_16){
        isSub:= "b00001010".U
    }.elsewhen(isCras_32 | isStas_32){
        isSub:= "b00000001".U
    }.elsewhen(isCrsa_32 | isStsa_32){
        isSub:= "b00000010".U
    }

    val isAdder = (isSub=/=0.U | isAdd | isCr | isSt) && !isCompare && !isMaxMin && !isPbs

    val SrcSigned   = func(6,4) === 0.U || func(6,4) === "b100".U || isSt && (func(6,3) === "b1011".U || func(6,3) === "b1100".U)
    val Saturating  = !isSt && func(3).asBool || isSt && !func(3).asBool
    val Translation = !Saturating && (!isSt && !(func(6,3) === 4.U) || isSt && (func(6,3) === "b1011".U || func(6,3) === "b1101".U))
    val LessEqual   = Saturating
    val LessThan    = Translation
    
    def MixPrecisionLen = XLEN + XLEN / 8 + XLEN / 8
    def extender(SrcSigned:Bool,src:UInt,width:Int)={
        val a = WireInit(0.U(width.W))
        when(SrcSigned){
            a := SignExt(src,width)
        }.otherwise{
            a := ZeroExt(src,width)
        }
        a
    }
    
    val adderRes_ori = Wire(UInt(MixPrecisionLen.W))
    val adderRes = Wire(UInt(XLEN.W))
    val add1 = Wire(UInt(MixPrecisionLen.W))
    val add2 = Wire(UInt(MixPrecisionLen.W))
    val adderRes_ori_drophighestbit = Wire(UInt(MixPrecisionLen.W))
    val add1_drophighestbit = Wire(UInt(MixPrecisionLen.W))
    val add2_drophighestbit = Wire(UInt(MixPrecisionLen.W))
    
    // todo: if possible, rewrite add_src_map like adder_gather
    def add_src_map(width: Int, src:UInt, isSub: UInt,SrcSigned:Bool,isCr:Bool) = {
        var l = List(0.U)
        for (i <- 0 until XLEN / width) {
            val SrcClip = WireInit(0.U(width.W))
            SrcClip := src(i * width + width - 1, i * width)
            when(isCr){
                if(i%2 == 0 && XLEN/width > 1){
                    SrcClip :=src((i+1) * width + width - 1, (i+1) * width)
                }else if(i >= 1){
                    SrcClip :=src((i-1) * width + width - 1, (i-1) * width)
                }
            }
            val tmp = (SrcClip ^ Fill(width, isSub(i))) + isSub(i)
            val extension = Wire(UInt(1.W))
            when(SrcSigned){
                extension := tmp(width - 1)
                when(isSub(i) && tmp === Cat(1.U , Fill(width-1,0.U))){extension := 0.U}
            }.otherwise{
                extension := 0.U
                when(isSub(i) && tmp =/= 0.U){extension := 1.U}
            }
            val tmp_list1 = List.concat(List(extension), List(tmp(width - 1,0)))
            val tmp_list2 = List.concat(List(0.U), tmp_list1)
            l = List.concat(tmp_list2 ,l)
        }
        l.dropRight(1).reduce(Cat(_, _)) // drop leading zero which we added for convenience
    }
    def add_src_map_drophighestbit(width: Int, src:UInt, isSub: UInt,isCr:Bool) = {
        var l = List(0.U)
        for (i <- 0 until XLEN / width) {
            val SrcClip = WireInit(0.U(width.W))
            SrcClip := src(i * width + width - 1, i * width)
            when(isCr){
                if(i%2 == 0 && XLEN/width > 1){
                    SrcClip :=src((i+1) * width + width - 1, (i+1) * width)
                }else if(i >= 1){
                    SrcClip :=src((i-1) * width + width - 1, (i-1) * width)
                }
            }
            val tmp = (SrcClip ^ Fill(width, isSub(i))) + isSub(i)
            val highestbit = Wire(UInt(1.W))
            highestbit := 0.U
            when(isSub(i) && tmp === Cat(1.U , Fill(width-1,0.U))){highestbit := 1.U}
            val tmp_list = List.concat(List(highestbit), List(tmp(width - 2,0)))
            val tmp_list1 = List.concat(List(0.U),tmp_list)
            val tmp_list2 = List.concat(List(0.U),tmp_list1)
            l = List.concat(tmp_list2 ,l)
        }
        l.dropRight(1).reduce(Cat(_, _)) 
    }
    
    if (XLEN == 32) {
        when (isAdd_8 | isSub_8) {
            add1 := add_src_map(8, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(8, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(8, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(8, src2, isSub,false.B)
        } .elsewhen (isAdd_16 | isSub_16) {
            add1 := add_src_map(16, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(16, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen (isAdd_32 | isSub_32) {
            add1 := add_src_map(32, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(32, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, isSub,false.B)
        } .elsewhen(isCras_16 | isCrsa_16){
            add1 := add_src_map(16, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(16, src2, isSub,SrcSigned,true.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,true.B)
        } .elsewhen(isCras_32 | isCrsa_32){
            add1 := add_src_map(32, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(32, src2, isSub,SrcSigned,true.B)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, isSub,true.B)
        } .elsewhen(isStas_16 | isStsa_16){
            add1 := add_src_map(16, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(16, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen(isStas_32 | isStsa_32){
            add1 := add_src_map(32, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(32, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, isSub,false.B)
        }.elsewhen(isComp_16){
            add1 := add_src_map(16, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(16, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen(isComp_8){
            add1 := add_src_map(8, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(8, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen(isMaxMin_16){
            add1 := add_src_map(16, src1, 0.B,!func(3).asBool,false.B)
            add2 := add_src_map(16, src2, isSub,!func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen(isMaxMin_8){
            add1 := add_src_map(8, src1, 0.B,!func(3).asBool,false.B)
            add2 := add_src_map(8, src2, isSub,!func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(8, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(8, src2, isSub,false.B)
        } .elsewhen(isMaxMin_XLEN){
            add1 := add_src_map(XLEN, src1, 0.B,true.B,false.B)
            add2 := add_src_map(XLEN, src2, isSub,true.B,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(XLEN, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(XLEN, src2, isSub,false.B)
        }.elsewhen(isPbs){
            add1 := add_src_map(8, src1, 0.B,false.B,false.B)
            add2 := add_src_map(8, src2, isSub,false.B,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(8, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(8, src2, isSub,false.B)
        } .elsewhen(isAdd_Q15 | isSub_Q15){
            add1 := add_src_map(16, Cat(Fill(16,0.U),src1(15,0)), 0.B,!func(3).asBool,false.B)
            add2 := add_src_map(16, Cat(Fill(16,0.U),src2(15,0)), isSub,!func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, Cat(Fill(16,0.U),src1(15,0)), 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, Cat(Fill(16,0.U),src2(15,0)), isSub,false.B)
        } .elsewhen(isAdd_Q31 | isSub_Q31 | isSub_C31 |isAdd_C31){
            add1 := add_src_map(32, src1(31,0), 0.B,!func(3).asBool,false.B)
            add2 := add_src_map(32, src2(31,0), isSub,!func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(32,src1(31,0), 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2(31,0), isSub,false.B)
        } .elsewhen(isAve){
            add1 := add_src_map(32, src1, 0.B,true.B,false.B)
            add2 := add_src_map(32, src2, isSub,true.B,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(32,src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, isSub,false.B)
        }.otherwise {
            add1 := DontCare
            add2 := DontCare
            add1_drophighestbit := DontCare
            add2_drophighestbit := DontCare
        }
    } else if (XLEN == 64) {
        when (isAdd_8 | isSub_8) {
            add1 := add_src_map(8, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(8, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(8, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(8, src2, isSub,false.B)
        } .elsewhen (isAdd_16 | isSub_16) {
            add1 := add_src_map(16, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(16, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen (isAdd_32 | isSub_32) {
            add1 := add_src_map(32, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(32, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, isSub,false.B)
        } .elsewhen (isAdd_64 | isSub_64) {
            add1 := add_src_map(64, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(64,  src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(64, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(64, src2, isSub,false.B)
        } .elsewhen (isCras_16 | isCrsa_16){
            add1 := add_src_map(16, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(16, src2, isSub,SrcSigned,true.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,true.B)
        } .elsewhen(isCras_32 | isCrsa_32){
            add1 := add_src_map(32, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(32, src2, isSub,SrcSigned,true.B)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, isSub,true.B)
        } .elsewhen(isStas_16 | isStsa_16){
            add1 := add_src_map(16, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(16, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen(isStas_32 | isStsa_32){
            add1 := add_src_map(32, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(32, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, isSub,false.B)
        } .elsewhen(isComp_16){
            add1 := add_src_map(16, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(16, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen(isComp_8){
            add1 := add_src_map(8, src1, 0.B,SrcSigned,false.B)
            add2 := add_src_map(8, src2, isSub,SrcSigned,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen(isMaxMin_16){
            add1 := add_src_map(16, src1, 0.B,!func(3).asBool,false.B)
            add2 := add_src_map(16, src2, isSub,!func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, isSub,false.B)
        } .elsewhen(isMaxMin_8){
            add1 := add_src_map(8, src1, 0.B,!func(3).asBool,false.B)
            add2 := add_src_map(8, src2, isSub,!func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(8, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(8, src2, isSub,false.B)
        } .elsewhen(isMaxMin_32){
            add1 := add_src_map(32, src1, 0.B,func(3).asBool,false.B)
            add2 := add_src_map(32, src2, isSub,func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, isSub,false.B)
        }.elsewhen(isMaxMin_XLEN){
            add1 := add_src_map(XLEN, src1, 0.B,true.B,false.B)
            add2 := add_src_map(XLEN, src2, isSub,true.B,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(XLEN, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(XLEN, src2, isSub,false.B)
        } .elsewhen(isPbs){
            add1 := add_src_map(8, src1, 0.B,false.B,false.B)
            add2 := add_src_map(8, src2, isSub,false.B,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(8, src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(8, src2, isSub,false.B)
        }  .elsewhen(isAdd_Q15 | isSub_Q15){
            add1 := add_src_map(16, Cat(Fill(48,0.U),src1(15,0)), 0.B,!func(3).asBool,false.B)
            add2 := add_src_map(16, Cat(Fill(48,0.U),src2(15,0)), isSub,!func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(16, Cat(Fill(48,0.U),src1(15,0)), 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, Cat(Fill(48,0.U),src2(15,0)), isSub,false.B)
        } .elsewhen(isAdd_Q31 | isSub_Q31 | isSub_C31 |isAdd_C31){
            add1 := add_src_map(32, Cat(Fill(32,0.U),src1(31,0)), 0.B,!func(3).asBool,false.B)
            add2 := add_src_map(32, Cat(Fill(32,0.U),src2(31,0)), isSub,!func(3).asBool,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(32,Cat(Fill(32,0.U),src1(31,0)), 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(32,Cat(Fill(32,0.U),src2(31,0)), isSub,false.B)
        }.elsewhen(isAve){
            add1 := add_src_map(64, src1, 0.B,true.B,false.B)
            add2 := add_src_map(64, src2, isSub,true.B,false.B)
            add1_drophighestbit := add_src_map_drophighestbit(64,src1, 0.B,false.B)
            add2_drophighestbit := add_src_map_drophighestbit(64,src2, isSub,false.B)
        }.otherwise {
            add1 := DontCare
            add2 := DontCare
            add1_drophighestbit := DontCare
            add2_drophighestbit := DontCare
        }
    } else {
        Debug(prefix = true, "Unexpected XLEN for VALU")
        add1 := DontCare
        add2 := DontCare
        add1_drophighestbit := DontCare
        add2_drophighestbit := DontCare
    }
    adderRes_ori := add1 +& add2
    adderRes_ori_drophighestbit := add1_drophighestbit +& add2_drophighestbit
    
    def gather_offset(width: Int, index: Int) = (width + 2) * index + width - 1
    
    def gather_offset_end(width: Int, index: Int) = gather_offset(width, index) - width + 1
    
    def adder_gather(adderRes_ori: UInt, width: Int) = {
        var l: List[UInt] = List(adderRes_ori(gather_offset(width, 0), gather_offset_end(width, 0)))
        if ((XLEN / width - 2) >= 0) {
            for (i <- 1 until XLEN / width) {
                l =  List.concat(List(adderRes_ori(gather_offset(width, i), gather_offset_end(width, i))), l)
            }
        }
        l.reduce(Cat(_, _))
    }
    def Saturated_Check(adderRes_ori:UInt,adderRes_ori_drophighestbit:UInt,width: Int,SrcSigned:Bool,isSub:UInt)= {
        var l = List(0.U)
        val OV = WireInit(false.B)
        for(i <- 0 until XLEN/width){
            val tmp = Wire(UInt(1.W))
            val newbits = Wire(UInt(width.W))
            newbits := adderRes_ori(gather_offset(width, i),gather_offset_end(width, i))
            when(SrcSigned){
                tmp := adderRes_ori(gather_offset(width, i)+2) ^ adderRes_ori_drophighestbit(gather_offset(width, i))
                when(tmp.asBool){
                    when(adderRes_ori(gather_offset(width, i)+2)){
                        newbits := Cat(1.U,Fill(width-1,0.U))
                    }.otherwise{
                        newbits := Cat(0.U,Fill(width-1,1.U))
                    }
                    OV := true.B
                }
            }.otherwise{
                tmp := adderRes_ori(gather_offset(width, i)+1)
                when(tmp.asBool){
                    when(isSub(i)){
                        newbits := Fill(width,0.U)
                        OV := true.B
                    }.otherwise{
                        newbits := Fill(width,1.U)
                        OV := true.B
                    }
                }
            }
            l = List.concat(List(newbits) ,l)
        }
        Cat(OV.asUInt,l.dropRight(1).reduce(Cat(_, _)))
    }
    def AdderRes_Translation(adderRes_ori:UInt,width:Int) = {
        var l = List(0.U)
        for(i <- 0 until XLEN/width){
            val tmp = adderRes_ori(gather_offset(width, i)+1,gather_offset_end(width,i)) >> 1
            l = List.concat(List(tmp) ,l)
        }
        l.dropRight(1).reduce(Cat(_, _))
    }
    def Compare(adderRes_ori:UInt,width: Int,SrcSigned:Bool,LessEqual:Bool,LessThan:Bool) = {
        var l = List(0.U)
        for(i <- 0 until XLEN/width){
            val compare_res = WireInit(false.B)
            val islessthan = adderRes_ori(gather_offset(width, i)+1).asBool
            val isequal    = adderRes_ori(gather_offset(width, i), gather_offset_end(width, i)) === 0.U
            compare_res := Mux(LessThan,islessthan,Mux(LessEqual,islessthan || isequal,isequal))
            l = List.concat(List(Fill(width,compare_res.asUInt)) ,l)
        }
        l.dropRight(1).reduce(Cat(_, _))
    }

    def MaxMin(adderRes_ori:UInt,width: Int,src1:UInt,src2:UInt,LessThan:Bool) = {
        var l = List(0.U)
        for(i <- 0 until XLEN/width){
            val maxmin_res = WireInit(0.U(width.W))
            val islessthan = adderRes_ori(gather_offset(width, i)+1).asBool
            maxmin_res := Mux(!(islessthan^LessThan),src1(i*width+width-1,i*width),src2(i*width+width-1,i*width))
            l = List.concat(List(maxmin_res) ,l)
            Debug("[PALU] islessthan %x LessThan %x xor %x maxmin_res %x src1 %x src2 %x \n",islessthan,LessThan,!(islessthan^LessThan),maxmin_res,src1(i*width+width-1,i*width),src2(i*width+width-1,i*width))
        }
        l.dropRight(1).reduce(Cat(_, _))
    }    

    when (isAdd_8 | isSub_8 | isPbs) {
        adderRes := adder_gather(adderRes_ori, 8)
    } .elsewhen (isAdd_16 | isSub_16 | isCras_16 | isCrsa_16 | isAdd_Q15 | isSub_Q15 | isStas_16 | isStsa_16) {
        adderRes := adder_gather(adderRes_ori, 16)
    } .elsewhen (isAdd_32 | isSub_32 | isCras_32 | isCrsa_32 |isAdd_Q31 |isSub_Q31|isSub_C31|isAdd_C31 | isStas_32 | isStsa_32) {
        adderRes := adder_gather(adderRes_ori, 32)
    } .elsewhen(isAdd_64 | isSub_64 | isAve) {
        adderRes := adder_gather(adderRes_ori, 64)
    } .otherwise {
        adderRes := DontCare
    }

    val adderOV = WireInit(false.B)
    val adderRes_final = WireInit(adderRes)
    when(isAdd_16 | isSub_16 | isCras_16 | isCrsa_16 | isStas_16 | isStsa_16){
        when(Saturating){
            val Saturated_Check_Res = Saturated_Check(adderRes_ori,adderRes_ori_drophighestbit,16,SrcSigned,isSub)
            adderRes_final := Saturated_Check_Res(XLEN-1,0)
            adderOV := Saturated_Check_Res(XLEN).asBool
        }.elsewhen(Translation){
            adderRes_final := AdderRes_Translation(adderRes_ori,16)
        }
    }.elsewhen(isAdd_8 | isSub_8){
        when(Saturating){
            val Saturated_Check_Res = Saturated_Check(adderRes_ori,adderRes_ori_drophighestbit,8,SrcSigned,isSub)
            adderRes_final := Saturated_Check_Res(XLEN-1,0)
            adderOV := Saturated_Check_Res(XLEN).asBool
        }.elsewhen(Translation){
            adderRes_final := AdderRes_Translation(adderRes_ori,8)
        }
    }.elsewhen(isAdd_32 | isSub_32 | isCras_32 | isCrsa_32 | isStas_32 | isStsa_32){
        when(Saturating){
            val Saturated_Check_Res = Saturated_Check(adderRes_ori,adderRes_ori_drophighestbit,32,SrcSigned,isSub)
            adderRes_final := Saturated_Check_Res(XLEN-1,0)
            adderOV := Saturated_Check_Res(XLEN).asBool
        }.elsewhen(Translation){
            adderRes_final := AdderRes_Translation(adderRes_ori,32)
        }
    }.elsewhen(isAdd_64 | isSub_64){
        when(Saturating){
            val Saturated_Check_Res = Saturated_Check(adderRes_ori,adderRes_ori_drophighestbit,64,SrcSigned,isSub)
            adderRes_final := Saturated_Check_Res(XLEN-1,0)
            adderOV := Saturated_Check_Res(XLEN).asBool
        }.elsewhen(Translation){
            adderRes_final := AdderRes_Translation(adderRes_ori,64)
        }
    }.elsewhen(isAdd_Q15| isSub_Q15){
        val Saturated_Check_Res = Saturated_Check(adderRes_ori,adderRes_ori_drophighestbit,16,!func(3).asBool,isSub)
        adderRes_final := SignExt(Saturated_Check_Res(15,0),XLEN)
        adderOV := Saturated_Check_Res(XLEN).asBool
    }.elsewhen(isAdd_Q31 | isSub_Q31 ){
        val Saturated_Check_Res = Saturated_Check(adderRes_ori,adderRes_ori_drophighestbit,32,!func(3).asBool,isSub)
        adderRes_final := SignExt(Saturated_Check_Res(31,0),XLEN)
        adderOV := Saturated_Check_Res(XLEN).asBool
    }.elsewhen(isSub_C31 |isAdd_C31){
        adderRes_final := SignExt(AdderRes_Translation(adderRes_ori,32)(31,0),XLEN)
    }.elsewhen(isAve){
        adderRes_final := (adderRes_ori+&1.U)(XLEN,1)
    }

    val compareOV = WireInit(false.B)
    val compareRes = WireInit(0.U(XLEN.W))
    when(isComp_16){
        compareRes := Compare(adderRes_ori,16,SrcSigned,LessEqual,LessThan)
    }.elsewhen(isComp_8){
        compareRes := Compare(adderRes_ori,8,SrcSigned,LessEqual,LessThan)
    }

    val maxminOV = WireInit(false.B)
    val maxminRes = WireInit(0.U(XLEN.W))
    when(isMaxMin_16){
        maxminRes := MaxMin(adderRes_ori,16,src1,src2,!func(0).asBool)
    }.elsewhen(isMaxMin_8){
        maxminRes := MaxMin(adderRes_ori,8 ,src1,src2,!func(0).asBool)
    }.elsewhen(isMaxMin_XLEN){
        maxminRes := MaxMin(adderRes_ori,XLEN ,src1,src2,!funct3(1).asBool)
    }.elsewhen(isMaxMin_32){
        maxminRes := MaxMin(adderRes_ori,32,src1,src2,!func(0).asBool)
    }

    val pbsOV  = WireInit(false.B)
    val pbsRes = WireInit(0.U(XLEN.W))
    when(isPbs){
        pbsRes := Mux(func(0).asBool,src3,0.U) + (0 until XLEN/8).map(i => Mux(adderRes_ori(gather_offset(8, i)+1),(Fill(8,1.U)^adderRes(i*8+8-1,i*8))+1.U,adderRes(i*8+8-1,i*8))).reduce(_+&_)
    }


    Debug("[PALU] src1 %x src2 %x func %x is_Add8 %x is_Sub8 %x is_Add16 %x isSub_16 %x isCras_16 %x isCrsa_16 %x SrcSigned %x Saturating %x Translation %x \n",src1,src2,func,isAdd_8,isSub_8,isAdd_16,isSub_16,isCras_16,isCrsa_16,SrcSigned,Saturating,Translation)
    Debug("[PALU] isAdd_32 %x isSub_32 %x isCras_32 %x isCrsa_32 %x isAdd_64 %x isSub_64 %x \n",isAdd_32,isSub_32,isCras_32,isCrsa_32,isAdd_64,isSub_64)
    Debug("[PALU] add1 %x add2 %x add1drop %x add2drop %x adderRes_ori %x adderRes_oridrop %x \n",add1,add2,add1_drophighestbit,add2_drophighestbit,adderRes_ori,adderRes_ori_drophighestbit)
    Debug("[PALU] addres %x adderRes_final %x adderOV %x \n",adderRes,adderRes_final,adderOV)

    Debug("[PALU] isComp_16 %x LessThan %x LessEqual %x compareRes %x \n",isComp_16,LessThan,LessEqual,compareRes)

    //shift ops
    val isRs_16 = func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,1) === 0.U && funct3 === 0.U
    val isLs_16 = func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,0) === 2.U && funct3 === 0.U 
    val isLR_16 =(func(6,3) === 5.U || func(6,3) === 6.U)&& func(2,0) === 3.U && funct3 === 0.U
    val isRs_8  = func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,1) === 2.U && funct3 === 0.U
    val isLs_8  = func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,0) === 6.U && funct3 === 0.U
    val isLR_8  =(func(6,3) === 5.U || func(6,3) === 6.U)&& func(2,0) === 7.U && funct3 === 0.U
    val isRs_32 =((func(6,5)==="b01".U && func(4,3) =/= 0.U) || func(6,3) === 8.U) && func(2,1) === 0.U && funct3 === 2.U
    val isLs_32 =((func(6,5)==="b01".U && func(4,3) =/= 0.U) || func(6,3) === 8.U) && func(2,0) === 2.U && funct3 === 2.U
    val isLR_32 =(func(6,3) === 5.U || func(6,3) === 6.U) && func(2,0) === 3.U && funct3 === 2.U
    val isLR_Q31= func(6,4) === "b011".U && func(2,0) === "b111".U && funct3 === "b001".U
    val isLs_Q31= func(6,4) === "b001".U && func(2,0) === "b011".U && funct3 === "b001".U
    val isRs_XLEN = (func(6,3) === "b0010".U || func(6,3)==="b1101".U) && func(2,1) === "b01".U && funct3 === "b001".U
    val isSRAIWU= func === "b0011010".U && funct3 === "b001".U
    val isFSRW  = io.in.bits.cf.instr(26,25) === "b10".U  && funct3 === "b101".U   && func === "b0111011".U
    val isWext    = func(6,4) === "b110".U && func(2,0) === "b111".U && funct3 === "b000".U
    val isShifter = isRs_16 | isLs_16 | isLR_16 | isRs_8 | isLs_8 | isLR_8 | isRs_32 | isLs_32 | isLR_32 | isLs_Q31 | isLR_Q31 | isRs_XLEN | isSRAIWU | isFSRW | isWext

    val Round       =((func(6,3) === "b0110".U &&(func(1) === 0.U || func(1,0) === "b11".U)
                    ||func(6,3) === "b0111".U && (func(2,1) === 0.U && func24.asBool || func(2,1) === 2.U && func23.asBool)) && funct3 === 0.U
                    ||(isRs_32 && (func(6,3) === 6.U || func(6,3) === 8.U) || isLR_32 && func(4).asBool)
                    ||(isLR_Q31 && func(3).asBool)
                    ||isRs_XLEN
                    ||isSRAIWU)
    val ShiftSigned = (isLR_16 || isLR_8 || isLR_32 || isLR_Q31 || isLs_Q31 ||isLs_32 && (func(6,3) === 6.U || func(6,3) === 8.U) || (isLs_16 || isLs_8) && (func(6,3) === "b0110".U || func === "b0111010".U && func24.asBool || func === "b0111110".U && func23.asBool))
    val Arithmetic  = (isRs_16 || isRs_8 || isRs_32) && func(0) === 0.U  || isLR_16 || isLR_8 || isLR_32 || isLR_Q31 || isRs_XLEN || isSRAIWU

    val isClip_16 = func === "b1000010".U & funct3 === 0.U
    val isClip_8  = func === "b1000110".U & funct3 === 0.U
    val isclip_32 = func(6,4) === "b111".U & func(2,0) === "b010".U & funct3 === 0.U
    val isClip = isClip_16 | isClip_8 | isclip_32

    val isSat_16  = func === "b1010110".U && src2(4,0) === "b10001".U && funct3 === 0.U
    val isSat_8   = func === "b1010110".U && src2(4,0) === "b10000".U && funct3 === 0.U
    val isSat_32  = func === "b1010110".U && src2(4,0) === "b10010".U && funct3 === 0.U
    val isSat_W   = func === "b1010110".U && src2(4,0) === "b10100".U && funct3 === 0.U
    val isSat     = isSat_16 | isSat_8 | isSat_32 | isSat_W

    val isCnt_16  = func === "b1010111".U && src2(4,1) === "b0100".U && funct3 === 0.U
    val isCnt_8   = func === "b1010111".U && src2(4,1) === "b0000".U && funct3 === 0.U
    val isCnt_32  = func === "b1010111".U && src2(4,1) === "b1100".U && funct3 === 0.U
    val isCnt     = isCnt_16 | isCnt_8 | isCnt_32

    val isSwap_16 = func(6,5) === 0.U && func (2,0) === "b111".U && funct3 === 1.U
    val isSwap_8  = func === "b1010110".U && (src2(4,0) === "b11000".U && funct3 === 0.U && io.in.bits.cf.instrType === InstrPI || src2(5,0) === "b001000".U && funct3 === "b101".U && io.in.bits.cf.instrType === InstrPB)
    val isSwap    = isSwap_16 | isSwap_8

    val isUnpack  = func === "b1010110".U && (src2(4,3) === "b01".U || src2(4,0) === "b10011".U || src2(4,0) === "b10111".U) && funct3 === 0.U

    val isBitrev  = (func(6,3) === "b1110".U && (func(2,0) === "b011".U || func(2,1) === "b10".U) && funct3 === 0.U
                   || func === "b0110101".U && funct3 === "b101".U && io.in.bits.cf.instr(6,0) === "b0010011".U && src2(4,0) === "b11111".U)

    val isCmix    = io.in.bits.cf.instr(14,12) === "b001".U && io.in.bits.cf.instr(6,0) === "b0110011".U && io.in.bits.cf.instr(26,25) === "b11".U

    val isInsertb = func === "b1010110".U && io.in.bits.cf.instr(24,23) === "b00".U && funct3 === "b000".U

    val isPackbb  = func === "b0000100".U && funct3 === "b100".U && io.in.bits.cf.instr(6,0) === "b0110011".U
    val isPackbt  = func === "b0001111".U && funct3 === "b010".U
    val isPacktb  = func === "b0011111".U && funct3 === "b010".U
    val isPacktt  = func === "b0100100".U && funct3 === "b100".U && io.in.bits.cf.instr(6,0) === "b0110011".U
    val isPack    = isPackbb | isPackbt | isPacktb | isPacktt

    def shifter(width: Int, src1:UInt, src2:UInt, Round:Bool, ShiftSigned:Bool,Righshift:Bool,Arithmetic:Bool) = {
        var l = List(0.U)
        val OV= Wire(Vec(XLEN/width,Bool()))
        (0 until XLEN/width).map(i => OV(i) := false.B)
        for(i <- 0 until XLEN/width){
            val src1_clip = src1(i * width + width -1,i * width)
            val res = Wire(UInt(width.W))
            res := src1_clip
            when(src2 =/= 0.U){
                when(Righshift){
                    val src1_cat = WireInit(src1_clip)
                    val src2_cat = WireInit(src2)
                    val tmp      = Wire(UInt(width.W))
                    when(Round){src2_cat := src2 -1.U}
                    when(Arithmetic){
                        tmp := (src1_cat.asSInt >> src2_cat).asUInt
                    }.otherwise{
                        tmp := src1_cat >> src2_cat
                    }
                    when(Round){
                        res := (extender(Arithmetic,tmp,width+1) + 1.U)(width,1).asUInt
                    }.otherwise{
                        res := tmp.asUInt
                    }
                    Debug("[PALUrightshift] src1_cat %x src2 %x src2_cat %x tmp %x res %x\n",src1_cat,src2,src2_cat,tmp,res)
                }.otherwise{
                    val tmp = Cat(Fill(width,src1_clip(width-1)),src1_clip)
                    val tmp1= tmp << src2
                    res := tmp1(width-1,0)
                    when(ShiftSigned){
                        val tmp2= WireInit(tmp1)
                        when(tmp1(2*width-1).asBool){tmp2 := Fill(2*width,1.U) ^ tmp1}
                        when(tmp2(2*width-1,width-1) =/= 0.U){
                            OV(i):= 1.U
                            when(tmp1(2*width-1).asBool){
                                res := Cat(1.U,Fill(width-1,0.U))
                            }.otherwise{
                                res := Cat(0.U,Fill(width-1,1.U))
                            }
                        }
                    Debug("[PALU] tmp %x tmp1 %x tmp2 %x tmp2(2*width-1,width-1) %x OV %x\n",tmp,tmp1,tmp2,tmp2(2*width-1,width-1),OV(i))
                    }
                    Debug("[PALU] tmp %x tmp1 %x \n",tmp,tmp1)
                }
            }
            l = List.concat(List(res) ,l)
        }
        Cat(OV.reduce(_||_).asUInt,l.dropRight(1).reduce(Cat(_, _))).asUInt
    }
    def SetSrc2(width: Int,src2:UInt,isLR:Bool) = {
        val realSrc2 = WireInit(src2(log2Up(width)-1,0))
        when(isLR){
            when(src2(log2Up(width)-1).asBool){
                val tmp = (Fill(log2Up(width),1.U) ^ src2(log2Up(width)-1,0)) + 1.U
                realSrc2 := tmp
                when(tmp === Cat(1.U,Fill(log2Up(width)-1,0.U))){
                    realSrc2 := Cat(0.U,Fill(log2Up(width)-1,1.U))
                }
            }
        }
        Cat(src2(log2Up(width)-1),realSrc2).asUInt
    }
    def cliper(width: Int, src1:UInt, src2:UInt,Arithmetic:Bool)={
        var l = List(0.U)
        val OV= Wire(Vec(XLEN/width,Bool()))
        (0 until XLEN/width).map(i => OV(i) := false.B)
        for(i <- 0 until XLEN/width){
            val res = WireInit(src1(i * width+width-1,i*width))
            val tmp0 = WireInit(src1(i * width+width-1,i*width))
            when(Arithmetic){
                tmp0 := Fill(width,src1(i*width+width-1))^src1(i * width+width-1,i*width)
            }.otherwise{
                tmp0 := src1(i * width+width-1,i*width)
            }
            val tmp = tmp0 >> src2
            val saturating = (Fill(width,1.U)<<src2).asTypeOf(UInt(width.W))
            when(tmp =/= 0.U && Arithmetic){
                OV(i) := true.B
                res := Mux(src1(i*width+width-1),saturating,Fill(width,1.U)^saturating)
            }.elsewhen(!Arithmetic && tmp =/= 0.U){
                OV(i) := true.B
                res := Mux(src1(i*width+width-1),0.U,Fill(width,1.U)^saturating)  
            }
            l = List.concat(List(res) ,l)
            Debug("[PALU]CLIPER tmp0 %x tmp %x Arithmetic %x \n",tmp0,tmp,Arithmetic)
        }
        Cat(OV.reduce(_||_).asUInt,l.dropRight(1).reduce(Cat(_, _))).asUInt
    }
    def saturator(width: Int, src1:UInt)={
        var l = List(0.U)
        val OV= Wire(Vec(XLEN/width,Bool()))
        (0 until XLEN/width).map(i => OV(i) := false.B)
        for(i <- 0 until XLEN/width){
            val res = WireInit(src1(i * width+width-1,i*width))
            val tmp = src1(i * width+width-1,i*width)
            when(tmp === Cat(1.U,Fill(width-1,0.U))){
                OV(i) := true.B
                res   := Cat(0.U,Fill(width-1,1.U))
            }.elsewhen(tmp(width-1).asBool){
                res   := (Fill(width,1.U)^tmp) + 1.U
            }
            l = List.concat(List(res) ,l)
        }
        Cat(OV.reduce(_||_).asUInt,l.dropRight(1).reduce(Cat(_, _))).asUInt
    }
    def counter(width:Int,src1:UInt,countzero:Bool)={
        var l = List(0.U)
        for(i <- 0 until XLEN/width){
            val clip = src1(i*width+width-1,i*width)
            val sample = Mux(countzero,0.U,clip(width-1))
            val compare = Wire(Vec(width,Bool()))
            (0 to width-1).map(i => compare(i) := clip(i) === sample)
            val result = Wire(Vec(width,UInt(1.W)))
            (0 to width-1).map(i => result(i) := {if(i == 0){compare(width-1-i).asUInt}else{(compare(width-1-i) && result(i-1).asBool).asUInt}})
            val tmp = result.reduce(_+&_)
            val res = Mux(countzero,tmp,tmp-1.U)
            l = List.concat(List(res) ,l)
        }
        l.dropRight(1).reduce(Cat(_, _))
    }
    def swaper(width:Int,sr1:UInt,src2:UInt,mode:UInt)={
        var l = List(0.U)
        for(i <- 0 until XLEN/width/2){
            val res = WireInit(0.U.asTypeOf(Vec(2,UInt(width.W))))
            val tmp1= src1(i * width * 2 + 2 * width -1,i * width * 2)
            val tmp2= src2(i * width * 2 + 2 * width -1,i * width * 2)
            when(mode === 0.U){
                res(1) := tmp1.asTypeOf(Vec(2,UInt(width.W)))(0)
                res(0) := tmp2.asTypeOf(Vec(2,UInt(width.W)))(0)
            }.elsewhen(mode === 1.U){
                res(1) := tmp1.asTypeOf(Vec(2,UInt(width.W)))(0)
                res(0) := tmp2.asTypeOf(Vec(2,UInt(width.W)))(1)
                Debug("[PALU] swaper 111 tmp1 %x tmp2 %x \n",tmp1.asTypeOf(Vec(2,UInt(width.W)))(0),tmp2.asTypeOf(Vec(2,UInt(width.W)))(1))
            }.elsewhen(mode === 3.U){
                res(1) := tmp1.asTypeOf(Vec(2,UInt(width.W)))(1)
                res(0) := tmp2.asTypeOf(Vec(2,UInt(width.W)))(0)
            }.elsewhen(mode === 2.U){
                res(1) := tmp1.asTypeOf(Vec(2,UInt(width.W)))(1)
                res(0) := tmp2.asTypeOf(Vec(2,UInt(width.W)))(1)
            }
            Debug("[PALU] swaper mode %x tmp1 %x tmp2 %x res %x \n",mode,tmp1,tmp2,Cat(res(1),res(0)))
            l = List.concat(List(Cat(res(1),res(0))) ,l)
        }
        l.dropRight(1).reduce(Cat(_, _))
    }
    def unpacker(sr1:UInt,mode:UInt,SrcSigned:Bool)={
        var l = List(0.U)
        for(i <- 0 until XLEN/32){
            val res = WireInit(0.U.asTypeOf(Vec(2,UInt(16.W))))
            val tmp = src1(i*32+32-1,i*32)
            res(1) := Mux(mode === "b01000".U,extender(SrcSigned,tmp.asTypeOf(Vec(4,UInt(8.W)))(1),16),Mux(mode === "b01001".U,extender(SrcSigned,tmp.asTypeOf(Vec(4,UInt(8.W)))(2),16),extender(SrcSigned,tmp.asTypeOf(Vec(4,UInt(8.W)))(3),16)))
            res(0) := Mux(mode === "b10011".U,extender(SrcSigned,tmp.asTypeOf(Vec(4,UInt(8.W)))(2),16),Mux(mode === "b01011".U,extender(SrcSigned,tmp.asTypeOf(Vec(4,UInt(8.W)))(1),16),extender(SrcSigned,tmp.asTypeOf(Vec(4,UInt(8.W)))(0),16)))
        l = List.concat(List(Cat(res(1),res(0))) ,l)
        }
        l.dropRight(1).reduce(Cat(_, _))
    }
    def bitrev(src1:UInt,src2:UInt) = {
        val space = WireInit(0.U((2*XLEN).W))
            space:= Cat(Fill(64,0.U),Reverse(src1))
        ((space << src2) << 1.U)(XLEN+XLEN-1,XLEN)
    }
    def cmix(src1:UInt,src2:UInt,src3:UInt) = {
        val space = Wire(Vec(XLEN,UInt(1.W)))
        (0 until XLEN).map(i => space(i):=Mux(src2(i).asBool,src1(i),src3(i)))
        Debug("[PALUCMIX] src1 %x src2 %x src3 %x space %x\n",src1,src2,src3,space.reduce(Cat(_,_)))
        Reverse(space.reduce(Cat(_,_)))
    }
    def insertb(src1:UInt,src2:UInt,src3:UInt) = {
        var l = List(0.U)
        for(i <- 0 until XLEN/8){
            val tmp = WireInit(src3(i*8+8-1,i*8))
            when(i.U === src2){
                tmp := src1(7,0)
            }
            l= List.concat(List(tmp),l)
            Debug("[PALUINSB] tmp %x\n",tmp)
        }
        Debug("[PALUINSB] src1 %x src2 %x src3 %x\n",src1,src2,src3)
        l.dropRight(1).reduce(Cat(_,_))
    }

    val shifterRes = WireInit(src1)
    val shifterOV  = WireInit(false.B)

    when(isRs_16 | isLs_16 |isLR_16){
        val tmp = WireInit(0.U(64.W))
            tmp:= Mux(isLR_16,SetSrc2(32,src2,true.B),SetSrc2(16,src2,false.B))
        val realSrc2 = Mux(isLR_16,tmp(log2Up(32)-1,0), tmp(log2Up(16)-1,0))
        val isLR_do_rightshift = tmp(log2Up(32))
        val tmp2 = shifter(16,src1,realSrc2,Round,ShiftSigned,isRs_16||isLR_16 && isLR_do_rightshift.asBool,Arithmetic)
        shifterRes := tmp2(XLEN-1,0).asUInt
        shifterOV  := tmp2(XLEN).asBool
    }.elsewhen(isRs_8 | isLs_8 |isLR_8){
        val tmp = WireInit(0.U(64.W))
            tmp:= Mux(isLR_8,SetSrc2(16,src2,true.B),SetSrc2(8,src2,false.B))
        val realSrc2 = Mux(isLR_8,tmp(log2Up(16)-1,0), tmp(log2Up(8)-1,0))
        val isLR_do_rightshift = tmp(log2Up(16))
        val tmp2 = shifter(8,src1,realSrc2,Round,ShiftSigned,isRs_8||isLR_8 && isLR_do_rightshift.asBool,Arithmetic)
        shifterRes := tmp2(XLEN-1,0).asUInt
        shifterOV  := tmp2(XLEN).asBool
        Debug("[PALU] isLR_do_rightshift %x tmp %x realSrc2 %x tmp2 %x \n",isLR_do_rightshift,tmp,realSrc2,tmp2)
    }.elsewhen(isRs_32 | isLs_32 | isLR_32 | isLs_Q31 | isLR_Q31){
        val tmp = WireInit(0.U(64.W))
            tmp:= Mux(isLR_Q31 | isLR_32,SetSrc2(64,src2,true.B),SetSrc2(32,src2,false.B))
        val realSrc2 = Mux(isLR_Q31 | isLR_32,tmp(log2Up(64)-1,0),tmp(log2Up(32)-1,0))
        val isLR_do_rightshift = tmp(log2Up(64))
        val tmp2 = shifter(32,Mux(isLR_Q31 | isLs_Q31,ZeroExt(src1(31,0),XLEN),src1),realSrc2,Round,ShiftSigned,isRs_32||(isLR_32 || isLR_Q31) && isLR_do_rightshift.asBool,Arithmetic)
        shifterRes := Mux(isLs_Q31 | isLR_Q31,SignExt(tmp2(31,0),XLEN),tmp2(XLEN-1,0).asUInt)
        shifterOV  := tmp2(XLEN).asBool
        Debug("[PALU] isLs_Q31 %x isLR_Q31 %x isLR_do_rightshift %x tmp %x realSrc2 %x \n",isLs_Q31,isLR_Q31,isLR_do_rightshift,tmp,realSrc2)
    }.elsewhen(isRs_XLEN){
        val tmp = SetSrc2(XLEN,src2,false.B)
        val realSrc2 = tmp(log2Up(XLEN)-1,0)
        val tmp2 = shifter(XLEN,src1,realSrc2,Round,ShiftSigned,true.B,Arithmetic)
        shifterRes := tmp2(XLEN-1,0).asUInt
        shifterOV  := tmp2(XLEN).asBool
    }.elsewhen(isSRAIWU){
        val tmp = SetSrc2(32,src2,false.B)
        val realSrc2 = tmp(log2Up(32)-1,0)
        val tmp2 = shifter(32,src1,realSrc2,Round,ShiftSigned,true.B,Arithmetic)
        shifterRes := SignExt(tmp2(32-1,0).asUInt,64)
        shifterOV  := tmp2(XLEN).asBool
    }.elsewhen(isFSRW | isWext){
        val tmp = SetSrc2(64,src2,false.B)
        val realSrc2 = tmp(log2Up(32)-1,0)
        val tmp2 = shifter(64,Mux(isFSRW,Cat(src3(31,0),src1(31,0)),src1),realSrc2,false.B,false.B,true.B,false.B)
        shifterRes := SignExt(tmp2(32-1,0).asUInt,64)
        shifterOV  := tmp2(XLEN).asBool
    }
    val clipRes = WireInit(src1)
    val clipOV  = WireInit(false.B)
    when(isClip_16){
        val tmp = cliper(16,src1,src2(3,0),!func24)
        clipRes := tmp(XLEN-1,0)
        clipOV  := tmp(XLEN)
    }.elsewhen(isClip_8){
        val tmp = cliper(8,src1,src2(2,0),!func24)
        clipRes := tmp(XLEN-1,0)
        clipOV  := tmp(XLEN)
    }.elsewhen(isclip_32){
        val tmp = cliper(32,src1,src2(4,0),!func(3))
        clipRes := tmp(XLEN-1,0)
        clipOV  := tmp(XLEN)
    }

    val satRes = WireInit(src1)
    val satOV  = WireInit(false.B)
    when(isSat_16){
        val tmp = saturator(16,src1)
        satRes := tmp(XLEN-1,0)
        satOV  := tmp(XLEN)
    }.elsewhen(isSat_8){
        val tmp = saturator(8,src1)
        satRes := tmp(XLEN-1,0)
        satOV  := tmp(XLEN)
    }.elsewhen(isSat_32){
        val tmp = saturator(32,src1)
        satRes := tmp(XLEN-1,0)
        satOV  := tmp(XLEN)
    }.elsewhen(isSat_W){
        val tmp = saturator(32,ZeroExt(src1(31,0),XLEN))
        satRes := SignExt(tmp(31,0),XLEN)
        satOV  := tmp(XLEN)
        Debug("[PALU]satw satOV %x\n",satOV)
    }

    val unpackRes = WireInit(src1)
    val unpackOV  = WireInit(false.B)
    when(isUnpack){
        unpackRes := unpacker(src1,src2(4,0)&"b11011".U,!src2(2).asBool)
    }

    val cntRes = WireInit(src1)
    val cntOV  = WireInit(false.B)
    when(isCnt_16){
        val tmp = counter(16,src1,src2(0))
        cntRes := tmp(XLEN-1,0)
    }.elsewhen(isCnt_8){
        val tmp = counter(8,src1,src2(0))
        cntRes := tmp(XLEN-1,0)
    }.elsewhen(isCnt_32){
        val tmp = counter(32,src1,src2(0))
        cntRes := tmp(XLEN-1,0)
    }

    val swapRes= WireInit(src1)
    val swapOV = WireInit(false.B)
    when(isSwap_16){
        swapRes := swaper(16,src1,src2,func(4,3))
    }.elsewhen(isSwap_8){
        swapRes := swaper(8,src1,src1,"b01".U)
    }

    val bitrevRes= WireInit(src1)
    val bitrevOV = WireInit(false.B)
    when(isBitrev){
        bitrevRes := Reverse(src1)(XLEN-1,0)
    }

    val cmixRes= WireInit(src1)
    val cmixOV = WireInit(false.B)
    when(isCmix){
        cmixRes := cmix(src1,src2,src3)
    }

    val insbRes= WireInit(src1)
    val insbOV = WireInit(false.B)
    when(isInsertb){
        insbRes := insertb(src1,src2(log2Up(XLEN/8)-1,0),src3)
    }

    val packRes= WireInit(src1)
    val packOV = WireInit(false.B)
    when(isPack){
        val bottom = Mux(isPackbb || isPacktb,src2(31,0),src2(63,32))
        val top    = Mux(isPackbb || isPackbt,src1(31,0),src1(63,32))
        packRes := Mux(isPackbb||isPacktt,Cat(bottom,top),Cat(top,bottom))
    }

    when(isPack){
        io.out.bits.result := packRes
        io.out.bits.DecodeOut.pext.OV := packOV
    }.elsewhen(isMaxMin){
        io.out.bits.result := maxminRes
        io.out.bits.DecodeOut.pext.OV := maxminOV
    }.elsewhen(isSwap){
        io.out.bits.result := swapRes
        io.out.bits.DecodeOut.pext.OV := swapOV
    }.elsewhen(isCmix){
        io.out.bits.result := cmixRes
        io.out.bits.DecodeOut.pext.OV := cmixOV
    }.elsewhen(isAdder){
        io.out.bits.result := adderRes_final
        io.out.bits.DecodeOut.pext.OV := adderOV
    }.elsewhen(isShifter){
        io.out.bits.result := shifterRes
        io.out.bits.DecodeOut.pext.OV := shifterOV
    }.elsewhen(isCompare){
        io.out.bits.result := compareRes
        io.out.bits.DecodeOut.pext.OV := compareOV
    }.elsewhen(isClip){
        io.out.bits.result := clipRes
        io.out.bits.DecodeOut.pext.OV := clipOV
    }.elsewhen(isSat){
        io.out.bits.result := satRes
        io.out.bits.DecodeOut.pext.OV := satOV
    }.elsewhen(isCnt){
        io.out.bits.result := cntRes
        io.out.bits.DecodeOut.pext.OV := cntOV
    }.elsewhen(isUnpack){
        io.out.bits.result := unpackRes
        io.out.bits.DecodeOut.pext.OV := unpackOV
    }.elsewhen(isPbs){
        io.out.bits.result := pbsRes
        io.out.bits.DecodeOut.pext.OV := pbsOV
    }.elsewhen(isBitrev){
        io.out.bits.result := bitrevRes
        io.out.bits.DecodeOut.pext.OV := bitrevOV
    }.elsewhen(isInsertb){
        io.out.bits.result := insbRes
        io.out.bits.DecodeOut.pext.OV := insbOV
    }.otherwise{
        io.out.bits.result := adderRes_final
        io.out.bits.DecodeOut.pext.OV := adderOV
    }
    Debug("[PALU] isRs_16 %x isLs_16 %x isLR_16 %x isRs_8 %x isLs_16 %x isLR_8 %x isRs_32 %x isLs_32 %x isLR_32 %x\n",isRs_16,isLs_16,isLR_16,isRs_8,isLs_8,isLR_8,isRs_32,isLs_32,isLR_32)
    Debug("[PALU] Round %x ShiftSigned %x Arithmetic %x\n",Round,ShiftSigned,Arithmetic)
    Debug("[PALU] shifterRes %x shifterOV %x \n",shifterRes,shifterOV)
    Debug("[PALU] MaxMinRes %x maxminOV %x \n",maxminRes,maxminOV)
    Debug("[PALU] isAdder %x isShifter %x isCompare %x isMaxMin %x\n",isAdder,isShifter,isCompare,isMaxMin)
}


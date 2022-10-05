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
        val OV = Output(Bool())
  })
}

class PALU extends NutCoreModule {
    val io = IO(new PALUIO)

    val valid = io.in.valid
    val src1  = io.in.bits.data.src1
    val src2  = io.in.bits.data.src2
    val func  = io.in.bits.ctrl.fuOpType

    io.in.ready := !valid || io.out.fire()
    io.out.valid:= valid
    io.out.bits.DecodeOut := io.in.bits
    
    val isAdd_64 = false.B//VALUOpType.add64 === func
    val isAdd_32 = false.B//VALUOpType.add32 === func
    val isAdd_16 = func(2,0).asUInt === 0.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)
    val isAdd_8 = false.B//VALUOpType.add8 === func
    val isAdd = isAdd_64 | isAdd_32 | isAdd_16 | isAdd_8

    val isSub_64 = false.B//VALUOpType.sub64 === io.in.func
    val isSub_32 = false.B//VALUOpType.sub32 === io.in.func
    val isSub_16 = func(2,0).asUInt === 1.U && func(6,5).asUInt === 0.U//VALUOpType.sub16 === io.in.func
    val isSub_8 = false.B//VALUOpType.sub8 === io.in.func
    val isSub = isSub_64 | isSub_32 | isSub_16 | isSub_8

    val SrcSigned   = func(6,4) === 0.U
    val Saturating  = func(0).asBool
    val Translation = !Saturating && !(func(6,3) === 4.U)
    
    def MixPrecisionLen = XLEN + XLEN / 8 + XLEN / 8
    
    val adderRes_ori = Wire(UInt(MixPrecisionLen.W))
    val adderRes = Wire(UInt(XLEN.W))
    val add1 = Wire(UInt(MixPrecisionLen.W))
    val add2 = Wire(UInt(MixPrecisionLen.W))
    val adderRes_ori_drophighestbit = Wire(UInt(MixPrecisionLen.W))
    val add1_drophighestbit = Wire(UInt(MixPrecisionLen.W))
    val add2_drophighestbit = Wire(UInt(MixPrecisionLen.W))
    
    // todo: if possible, rewrite add_src_map like adder_gather
    def add_src_map(width: Int, src:UInt, isSub: Bool,SrcSigned:Bool) = {
        var l = List(0.U)
        for (i <- 0 until XLEN / width) {
            val tmp = (src(i * width + width - 1, i * width) ^ Fill(width, isSub)) + isSub
            val extension = Wire(UInt(1.W))
            when(SrcSigned){
                extension := tmp(width - 1)
            }.otherwise{
                extension := 0.U
            }
            val tmp_list1 = List.concat(List(extension), List(tmp(width - 1,0)))
            val tmp_list2 = List.concat(List(0.U), tmp_list1)
            l = List.concat(tmp_list2 ,l)
        }
        l.dropRight(1).reduce(Cat(_, _)) // drop leading zero which we added for convenience
    }
    def add_src_map_drophighestbit(width: Int, src:UInt, isSub: Bool) = {
        var l = List(0.U)
        for (i <- 0 until XLEN / width) {
            val tmp = (src(i * width + width - 2, i * width) ^ Fill(width, isSub)) + isSub
            val tmp_list = List.concat(List(0.U), List(tmp(width - 2,0)))
            val tmp_list1 = List.concat(List(0.U),tmp_list)
            val tmp_list2 = List.concat(List(0.U),tmp_list)
            l = List.concat(tmp_list2 ,l)
        }
        l.dropRight(1).reduce(Cat(_, _)) 
    }
    
    if (XLEN == 32) {
        when (isAdd_8 | isSub_8) {
            add1 := add_src_map(8, src1, 0.B,SrcSigned)
            add2 := add_src_map(8, src2, isSub_8,SrcSigned)
            add1_drophighestbit := add_src_map_drophighestbit(8, src1, 0.B)
            add2_drophighestbit := add_src_map_drophighestbit(8, src2, 0.B)
        } .elsewhen (isAdd_16 | isSub_16) {
            add1 := add_src_map(16, src1, 0.B,SrcSigned)
            add2 := add_src_map(16, src2, isSub_16,SrcSigned)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, 0.B)
        } .elsewhen (isAdd_32 | isSub_32) {
            add1 := add_src_map(32, src1, 0.B,SrcSigned)
            add2 := add_src_map(32, src2, isSub_32,SrcSigned)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, 0.B)
        } .otherwise {
            add1 := DontCare
            add2 := DontCare
            add1_drophighestbit := DontCare
            add2_drophighestbit := DontCare
        }
    } else if (XLEN == 64) {
        when (isAdd_8 | isSub_8) {
            add1 := add_src_map(8, src1, 0.B,SrcSigned)
            add2 := add_src_map(8, src2, isSub_8,SrcSigned)
            add1_drophighestbit := add_src_map_drophighestbit(8, src1, 0.B)
            add2_drophighestbit := add_src_map_drophighestbit(8, src2, 0.B)
        } .elsewhen (isAdd_16 | isSub_16) {
            add1 := add_src_map(16, src1, 0.B,SrcSigned)
            add2 := add_src_map(16, src2, isSub_16,SrcSigned)
            add1_drophighestbit := add_src_map_drophighestbit(16, src1, 0.B)
            add2_drophighestbit := add_src_map_drophighestbit(16, src2, 0.B)
        } .elsewhen (isAdd_32 | isSub_32) {
            add1 := add_src_map(32, src1, 0.B,SrcSigned)
            add2 := add_src_map(32, src2, isSub_32,SrcSigned)
            add1_drophighestbit := add_src_map_drophighestbit(32, src1, 0.B)
            add2_drophighestbit := add_src_map_drophighestbit(32, src2, 0.B)
        } .elsewhen (isAdd_64 | isSub_64) {
            add1 := add_src_map(64, src1, 0.B,SrcSigned)
            add2 := add_src_map(64,  src2, isSub_64,SrcSigned)
            add1_drophighestbit := add_src_map_drophighestbit(64, src1, 0.B)
            add2_drophighestbit := add_src_map_drophighestbit(64, src2, 0.B)
        } .otherwise {
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
    def Saturated_Check(adderRes_ori:UInt,adderRes_ori_drophighestbit:UInt,width: Int,SrcSigned:Bool) = {
        var l = List(0.U)
        for(i <- 0 until XLEN/width){
            val tmp = Wire(UInt(1.W))
            when(SrcSigned){
                tmp := adderRes_ori(gather_offset(width, i)+2) ^ adderRes_ori_drophighestbit(gather_offset(width, i))
            }.otherwise{
                tmp := adderRes_ori(gather_offset(width, i)+1)
            }
            l = List.concat(List(tmp) ,l)
        }
        l.reduce(Cat(_, _)) =/= 0.U
    }
    def AdderRes_Translation(adderRes_ori:UInt,width:Int) = {
        var l = List(0.U)
        for(i <- 0 until XLEN/width){
            val tmp = adderRes_ori(gather_offset(width, i)+1,gather_offset_end(width,i)) >> 1
            l = List.concat(List(tmp) ,l)
        }
        l.dropRight(1).reduce(Cat(_, _))
    }

    when (isAdd_8 | isSub_8) {
        adderRes := adder_gather(adderRes_ori, 8)
    } .elsewhen (isAdd_16 | isSub_16) {
        adderRes := adder_gather(adderRes_ori, 16)
    } .elsewhen (isAdd_32 | isSub_32) {
        adderRes := adder_gather(adderRes_ori, 32)
    } .elsewhen(isAdd_64 | isSub_64) {
        adderRes := adder_gather(adderRes_ori, 64)
    } .otherwise {
        adderRes := DontCare
    }

    io.out.bits.OV := false.B
    val adderRes_final = WireInit(adderRes)
    when(isAdd_16 | isSub_16){
        when(Saturating){
            io.out.bits.OV := Saturated_Check(adderRes_ori,adderRes_ori_drophighestbit,16,SrcSigned)
        }.elsewhen(Translation){
            adderRes_final := AdderRes_Translation(adderRes_ori,16)
        }
    }

    io.out.bits.result := adderRes_final
    Debug("[PALU] src1 %x src2 %x func %x is_Add16 %x SrcSigned %x Saturating %x Translation %x \n",src1,src2,func,isAdd_16,SrcSigned,Saturating,Translation)
    Debug("[PALU] add1 %x add2 %x add1drop %x add2drop %x adderRes_ori %x adderRes_oridrop %x \n",add1,add2,add1_drophighestbit,add2_drophighestbit,adderRes_ori,adderRes_ori_drophighestbit)
    Debug("[PALU] addres %x adderRes_final %x OV %x \n",adderRes,adderRes_final,io.out.bits.OV)
}


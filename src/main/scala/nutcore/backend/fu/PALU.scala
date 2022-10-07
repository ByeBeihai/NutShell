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

class PALU extends NutCoreModule {
    val io = IO(new PALUIO)

    val valid = io.in.valid
    val src1  = io.in.bits.data.src1
    val src2  = io.in.bits.data.src2
    val func  = io.in.bits.ctrl.fuOpType
    val funct3= io.in.bits.ctrl.funct3
    val func24= io.in.bits.ctrl.func24
    val func23= io.in.bits.ctrl.func23

    io.in.ready := !valid || io.out.fire()
    io.out.valid:= valid
    io.out.bits.DecodeOut := io.in.bits
    
    val isAdd_64 = false.B//VALUOpType.add64 === func
    val isAdd_32 = func(2,0).asUInt === 0.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 2.U//false.B//VALUOpType.add32 === func
    val isAdd_16 = func(2,0).asUInt === 0.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U
    val isAdd_8  = func(2,0).asUInt === 4.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//false.B//VALUOpType.add8 === func
    val isAdd = isAdd_64 | isAdd_32 | isAdd_16 | isAdd_8

    val isSub_64 = false.B//VALUOpType.sub64 === io.in.func
    val isSub_32 = func(2,0).asUInt === 1.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 2.U//false.B//VALUOpType.sub32 === io.in.func
    val isSub_16 = func(2,0).asUInt === 1.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//VALUOpType.sub16 === io.in.func
    val isSub_8  = func(2,0).asUInt === 5.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U) && funct3 === 0.U//VALUOpType.sub8 === io.in.func

    val isCras_16 = func(2,0).asUInt === 2.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 0.U
    val isCrsa_16 = func(2,0).asUInt === 3.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 0.U
    val isCras_32 = func(2,0).asUInt === 2.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 2.U
    val isCrsa_32 = func(2,0).asUInt === 3.U && (func(6,5).asUInt === 0.U ||  func(6,3) === 4.U)&& funct3 === 2.U
    val isCr = isCras_16 | isCrsa_16 | isCras_32 | isCrsa_32

    val isSub = WireInit(0.U(8.W))
    when(isSub_64 | isSub_32 | isSub_16 | isSub_8){
        isSub:= "b11111111".U
    }.elsewhen(isCras_16){
        isSub:= "b00000101".U
    }.elsewhen(isCrsa_16){
        isSub:= "b00001010".U
    }.elsewhen(isCras_32){
        isSub:= "b00000001".U
    }.elsewhen(isCrsa_32){
        isSub:= "b00000010".U
    }

    val isAdder = isSub=/=0.U | isAdd | isCr

    val SrcSigned   = func(6,4) === 0.U
    val Saturating  = func(3).asBool && !(func(6,3) === 4.U)
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
        } .otherwise {
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

    when (isAdd_8 | isSub_8) {
        adderRes := adder_gather(adderRes_ori, 8)
    } .elsewhen (isAdd_16 | isSub_16 | isCras_16 | isCrsa_16) {
        adderRes := adder_gather(adderRes_ori, 16)
    } .elsewhen (isAdd_32 | isSub_32 | isCras_32 | isCrsa_32) {
        adderRes := adder_gather(adderRes_ori, 32)
    } .elsewhen(isAdd_64 | isSub_64) {
        adderRes := adder_gather(adderRes_ori, 64)
    } .otherwise {
        adderRes := DontCare
    }

    val adderOV = WireInit(false.B)
    val adderRes_final = WireInit(adderRes)
    when(isAdd_16 | isSub_16 | isCras_16 | isCrsa_16){
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
    }.elsewhen(isAdd_32 | isSub_32 | isCras_32 | isCrsa_32){
        when(Saturating){
            val Saturated_Check_Res = Saturated_Check(adderRes_ori,adderRes_ori_drophighestbit,32,SrcSigned,isSub)
            adderRes_final := Saturated_Check_Res(XLEN-1,0)
            adderOV := Saturated_Check_Res(XLEN).asBool
        }.elsewhen(Translation){
            adderRes_final := AdderRes_Translation(adderRes_ori,32)
        }
    }
    Debug("[PALU] src1 %x src2 %x func %x is_Add8 %x is_Sub8 %x is_Add16 %x isSub_16 %x isCras_16 %x isCrsa_16 %x SrcSigned %x Saturating %x Translation %x \n",src1,src2,func,isAdd_8,isSub_8,isAdd_16,isSub_16,isCras_16,isCrsa_16,SrcSigned,Saturating,Translation)
    Debug("[PALU] isAdd_32 %x isSub_32 %x isCras_32 %x isCrsa_32 %x \n",isAdd_32,isSub_32,isCras_32,isCrsa_32)
    Debug("[PALU] add1 %x add2 %x add1drop %x add2drop %x adderRes_ori %x adderRes_oridrop %x \n",add1,add2,add1_drophighestbit,add2_drophighestbit,adderRes_ori,adderRes_ori_drophighestbit)
    Debug("[PALU] addres %x adderRes_final %x OV %x \n",adderRes,adderRes_final,adderOV)

    //shift ops
    val isRs_16 = func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,1) === 0.U
    val isLs_16 = func(6,5) === 1.U && func(4,3) =/= 0.U && func(2,0) === 2.U
    val isLR_16 =(func(6,3) === 5.U || func(6,3) === 6.U)&& func(2,0) === 3.U
    val isShifter = isRs_16 | isLs_16 | isLR_16

    val Round       =(func(6,3) === "b0110".U &&(func(1) === 0.U || func(1,0) === "b11".U)
                    ||func(6,3) === "b0111".U && (func(2,1) === 0.U && func24.asBool || func(2,1) === 2.U && func23.asBool))
    val ShiftSigned = (isLR_16 || isLs_16 && (func(6,3) === "b0110".U || func === "b0111010".U && func24.asBool || func === "b0111101".U && func23.asBool))
    val Arithmetic  = isRs_16 && func(0) === 0.U || isLR_16

    def shifter(width: Int, src1:UInt, src2:UInt, Round:Bool, ShiftSigned:Bool,Righshift:Bool,Arithmetic:Bool) = {
        var l = List(0.U)
        val OV= WireInit(0.U((XLEN/width).W))
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
                        res := (Cat(tmp(width-1),tmp) + 1.U)(width,1).asUInt
                    }.otherwise{
                        res := tmp.asUInt
                    }
                }.otherwise{
                    val tmp = Cat(Fill(width,src1_clip(width-1)),src1_clip)
                    val tmp1= tmp << src2
                    res := tmp1(width-1,0)
                    when(ShiftSigned){
                        val tmp2= WireInit(tmp1)
                        when(tmp1(2*width-1).asBool){tmp2 := Fill(2*width,1.U) ^ tmp1}
                        when(tmp2(2*width-1,width-1) =/= 0.U){
                            OV.asTypeOf(Vec(XLEN/width,Bool()))(i):= 1.U
                            when(tmp1(2*width-1).asBool){
                                res := Cat(1.U,Fill(width-1,0.U))
                            }.otherwise{
                                res := Cat(0.U,Fill(width-1,1.U))
                            }
                        }
                    Debug("[PALU] tmp %x tmp1 %x tmp2 %x \n",tmp,tmp1,tmp2)
                    }
                }
            }
            l = List.concat(List(res) ,l)
        }
        Cat((OV=/=0.U).asUInt,l.dropRight(1).reduce(Cat(_, _))).asUInt
    }
    def SetSrc2(width: Int,src2:UInt,isLR:Bool) = {
        val realSrc2 = WireInit(src2(log2Up(width)-1,0))
        when(isLR){
            when(src2(log2Up(width)).asBool){
                val tmp = Fill(log2Up(width),1.U) ^ src2(log2Up(width)-1,0) + 1.U
                realSrc2 := tmp
                when(tmp === 0.U){
                    realSrc2 := Fill(log2Up(width),1.U)
                }
            }
        }
        Cat(src2(log2Up(width)),realSrc2).asUInt
    }

    val shifterRes = WireInit(src1)
    val shifterOV  = WireInit(false.B)

    when(isRs_16 | isLs_16 |isLR_16){
        val tmp = SetSrc2(16,src2,isLR_16)
        val realSrc2 = tmp(log2Up(16)-1,0)
        val isLR_do_rightshift = tmp(log2Up(16))
        val tmp2 = shifter(16,src1,realSrc2,Round,ShiftSigned,isRs_16||isLR_16 && isLR_do_rightshift.asBool,Arithmetic)
        shifterRes := tmp2(XLEN-1,0).asUInt
        shifterOV  := tmp2(XLEN).asBool
    }   

    when(isAdder){
        io.out.bits.result := adderRes_final
        io.out.bits.DecodeOut.pext.OV := adderOV
    }.elsewhen(isShifter){
        io.out.bits.result := shifterRes
        io.out.bits.DecodeOut.pext.OV := shifterOV
    }.otherwise{
        io.out.bits.result := adderRes_final
        io.out.bits.DecodeOut.pext.OV := adderOV
    }
    Debug("[PALU] isRs_16 %x isLs_16 %x isLR_16 %x \n",isRs_16,isLs_16,isLR_16)
    Debug("[PALU] Round %x ShiftSigned %x Arithmetic %x\n",Round,ShiftSigned,Arithmetic)
    Debug("[PALU] shifterRes %x shifterOV %x \n",shifterRes,shifterOV)
}


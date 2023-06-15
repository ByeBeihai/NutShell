/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._

trait HasInstrType {
  def InstrN  = "b00000".U
  def InstrI  = "b00100".U
  def InstrIZ = "b01100".U
  def InstrR  = "b00101".U
  def InstrS  = "b00010".U
  def InstrB  = "b00001".U
  def InstrU  = "b00110".U
  def InstrJ  = "b00111".U
  def InstrA  = "b01110".U
  def InstrSA = "b01111".U // Atom Inst: SC
  def InstrP  = "b010100".U
  def InstrPI = "b010101".U
  def InstrPM = "b010110".U
  def InstrPB = "b010111".U
  def InstrPRD= "b011100".U
  def InstrPLDR= "b11101".U
  def InstrPSTR= "b11001".U
  def InstrPLDI= "b11111".U
  def InstrPSTI= "b11011".U
  def InstrSNN= "b11110".U
  def isrfWen(instrType : UInt): Bool = instrType(2)
  def isInstrPLS(instrType : UInt): Bool = instrType(4) & instrType(3) & instrType(0)
}

// trait CompInstConst {
//   val RVCRegNumTable = Array(
//     BitPat("b000") -> 8.U,
//     BitPat("b001") -> 9.U,
//     BitPat("b010") -> 10.U,
//     BitPat("b011") -> 11.U,
//     BitPat("b100") -> 12.U,
//     BitPat("b101") -> 13.U,
//     BitPat("b110") -> 14.U,
//     BitPat("b111") -> 15.U
//   )
// }

object SrcType {
  def reg = "b0".U
  def pc  = "b1".U
  def imm = "b1".U
  def apply() = UInt(1.W)
}

object FuType extends HasNutCoreConst {
  def addition = if(Polaris_SNN_WAY_NUM != 0){if(Polaris_SNN_WAY_NUM == 2){2}else{1}}else{0}
  def num = 5 + Polaris_Independent_Bru + Polaris_SIMDU_WAY_NUM + addition
  def width = 4
  def aluint = if(Polaris_Independent_Bru == 1){Polaris_Independent_Bru + 3 + Polaris_SIMDU_WAY_NUM}else{0}
  def alu = aluint.U(width.W)
  def alu1int= if(Polaris_Independent_Bru == 1){Polaris_Independent_Bru + 3 + Polaris_SIMDU_WAY_NUM + 1}else{2}
  def alu1 = alu1int.U(width.W)
  def lsuint = if(Polaris_Independent_Bru == 1){2+Polaris_SIMDU_WAY_NUM}else{3+Polaris_SIMDU_WAY_NUM}
  def lsu = lsuint.U(width.W)
  def mduint = if(Polaris_Independent_Bru == 1){3+Polaris_SIMDU_WAY_NUM}else{4+Polaris_SIMDU_WAY_NUM}
  def mdu = mduint.U(width.W)
  def csrint = 1
  def csr = csrint.U(width.W)
  def mou = "b1001".U
  def bruint = if(Polaris_Independent_Bru == 1){0}else{alu1int}
  def bru = bruint.U(width.W)
  def simdu = simduint.U(width.W)
  def simduint = if(Polaris_SIMDU_WAY_NUM != 0){if(Polaris_Independent_Bru == 1){2}else{3}}else{0}
  def simdu1 = simdu1int.U(width.W)
  def simdu1int = if(Polaris_SIMDU_WAY_NUM != 0){if(Polaris_Independent_Bru == 1){3}else{4}}else{0}
  def snnuint = if(Polaris_SNN_WAY_NUM != 0){if(Polaris_SNN_WAY_NUM == 2){num-2}else{num-1}}else{0}
  def snnu = snnuint.U(width.W)
  def snnu1int = if(Polaris_SNN_WAY_NUM != 0){if(Polaris_SNN_WAY_NUM == 2){num-1}else{0}}else{0}
  def snnu1 = snnu1int.U(width.W)
  def apply() = UInt(width.W)
}

object FuOpType {
  def apply() = UInt(7.W)
}

object Instructions extends HasInstrType with HasNutCoreParameter {
  def NOP = 0x00000013.U
  val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def DecodeTable = RVIInstr.table ++ NutCoreTrap.table ++
    (if (HasMExtension) RVMInstr.table else Nil) ++
    (if (HasCExtension) RVCInstr.table else Nil) ++
    Priviledged.table ++
    //Priviledged.table_s ++
    RVAInstr.table ++
    RVZicsrInstr.table ++ RVZifenceiInstr.table ++
    (if(Polaris_SIMDU_WAY_NUM != 0){
    RVPInstr.table ++
    RVPIInstr.table ++
    RVPMInstr.table ++
    RVPBInstr.table ++
    RVPRDInstr.table 
    }else Nil) ++
    (if(Polaris_Vector_LDST){
    RVPLSInstr.table
    }else Nil) ++
    (if(Polaris_SNN_WAY_NUM != 0){
    RVSNNInstr.table 
    }else Nil)
}

object CInstructions extends HasInstrType with HasNutCoreParameter{
  def NOP = 0x00000013.U
  val DecodeDefault = List(RVCInstr.ImmNone, RVCInstr.DtCare, RVCInstr.DtCare, RVCInstr.DtCare)
  // val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def CExtraDecodeTable = RVCInstr.cExtraTable
}

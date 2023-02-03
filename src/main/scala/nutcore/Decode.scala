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
  def InstrN  = "b0000".U
  def InstrI  = "b0100".U
  def InstrIZ = "b1100".U
  def InstrR  = "b0101".U
  def InstrS  = "b0010".U
  def InstrB  = "b0001".U
  def InstrU  = "b0110".U
  def InstrJ  = "b0111".U
  def InstrA  = "b1110".U
  def InstrSA = "b1111".U // Atom Inst: SC
  def InstrP  = "b10100".U
  def InstrPI = "b10101".U
  def InstrPM = "b10110".U
  def InstrPB = "b10111".U
  def InstrPRD= "b11100".U

  def isrfWen(instrType : UInt): Bool = instrType(2)
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
  def num = if(Issue_Num == 1){6}else{8}
  def alu = if(Issue_Num == 1){"b000".U}else{"b110".U}
  def lsu = "b100".U
  def mdu = "b101".U
  def csr = "b001".U
  def csrint = 1
  def mou = "b1000".U
  def alu1= if(Issue_Num == 1){"b011".U}else{"b111".U}
  def bru = if(Issue_Num == 1){alu1}else{"b000".U}
  def simdu = "b010".U
  def simduint = 2
  def simdu1 = "b011".U
  def simdu1int = 3
  def apply() = UInt(4.W)
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
    RVPInstr.table ++
    RVPIInstr.table ++
    RVPMInstr.table ++
    RVPBInstr.table ++
    RVPRDInstr.table 
}

object CInstructions extends HasInstrType with HasNutCoreParameter{
  def NOP = 0x00000013.U
  val DecodeDefault = List(RVCInstr.ImmNone, RVCInstr.DtCare, RVCInstr.DtCare, RVCInstr.DtCare)
  // val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def CExtraDecodeTable = RVCInstr.cExtraTable
}

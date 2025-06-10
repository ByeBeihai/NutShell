/**************************************************************************************
* Copyright (c) 2025 Institute of Computing Technology, CAS
* Copyright (c) 2025 University of Chinese Academy of Sciences
* 
* polaris is licensed under Mulan PSL v2.
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
/**************************************************************************************
* Reference:
* Zong, J., Wang, J., Li, G., Wu, R., Zhao D.*, Polaris 23: a high throughput neuromorphic processing element by RISC-V customized instruction extension for spiking neural network (RV-SNN 2.0) and SIMD-style implementation of LIF model with backpropagation STDP. J Supercomput 81, 398 (2025)
***************************************************************************************/
package PolarisCore

import chisel3._
import chisel3.util._
import utils._
import PolarisCore.backend.fu.fpu.fudian._

object float {
  case class FType(expWidth: Int, sigWidth: Int) {
    val len = expWidth + sigWidth
  }

  val fp16 = FType(5, 11)
  val fp32 = FType(8, 24)
  val fp64 = FType(11, 53)

  def unbox(x: UInt, ftype: FType): UInt = {
    Mux(x.head(x.getWidth - ftype.len).andR,
      x.tail(ftype.len),
      FloatPoint.defaultNaNUInt(ftype.expWidth, ftype.sigWidth)
    )
  }

  def box(x: UInt, ftype: FType): UInt = {
    require(x.getWidth <= ftype.len)
    Cat(~0.U((ftype.len - x.getWidth).W), x)
  }
}

object FPUOpType extends FMAOpType with FCONVOpType with FDivSqrtOpType with FCOMPOpType {

}


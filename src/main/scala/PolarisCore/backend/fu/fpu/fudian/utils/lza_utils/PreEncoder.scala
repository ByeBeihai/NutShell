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
package PolarisCore.backend.fu.fpu.fudian.utils.lza_utils

import chisel3._
import chisel3.util._

class preEncoderIO(val len: Int) extends Bundle {
  val g, s, e= Input(UInt(len.W))
  val f = Output(UInt(len.W))
}
class PreEncoder(val len: Int) extends Module {
  val io = IO(new preEncoderIO(len))
  val (g, s, e) = (io.g, io.s, io.e)
  val f = Wire(Vec(len, Bool()))
  for (i <- 0 until len) {
    if (i == 0) {
      f(i) := (e(i + 1) & g(i)) |
           ((~e(i + 1)) & s(i)) |
              (e(i + 1) & s(i)) |
              (e(i + 1) & s(i))
    } else if(i == len - 1) {
      f(i) := (g(i) & (~s(i - 1))) | (s(i) & (~g(i - 1)))
    }else{
      f(i) := (e(i+1) & g(i) & (~s(i-1))) |
           ((~e(i+1)) & s(i) & (~s(i-1))) |
              (e(i+1) & s(i) & (~g(i-1))) |
           ((~e(i+1)) & g(i) & (~g(i-1)))
    }
  }
  io.f := Cat(f.reverse)
}
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
package nutcore.backend.fu.fpu.fudian.utils

import chisel3._
import chisel3.util._

/**
  * in => shift | collect sticky bit => {in_shifted, sticky}
  */
class ShiftRightJam(val len: Int) extends Module {
  val max_shift_width = log2Up(len + 1)
  val io = IO(new Bundle() {
    val in = Input(UInt(len.W))
    val shamt = Input(UInt())
    val out = Output(UInt(len.W))
    val sticky = Output(Bool())
  })
  val exceed_max_shift = io.shamt > len.U
  val shamt = io.shamt(max_shift_width - 1, 0)
  val sticky_mask =
    ((1.U << shamt).asUInt - 1.U)(len - 1, 0) | Fill(len, exceed_max_shift)
  io.out := Mux(exceed_max_shift, 0.U, io.in >> io.shamt)
  io.sticky := (io.in & sticky_mask).orR
}

object ShiftRightJam {
  def apply(x: UInt, shamt: UInt): (UInt, Bool) = {
    val shiftRightJam = Module(new ShiftRightJam(x.getWidth))
    shiftRightJam.io.in := x
    shiftRightJam.io.shamt := shamt
    (shiftRightJam.io.out, shiftRightJam.io.sticky)
  }
}

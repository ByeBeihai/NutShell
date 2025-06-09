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
import nutcore.backend.fu.fpu.fudian.utils.lza_utils._

class LzaIO(val len: Int) extends Bundle {

  val a, b = Input(UInt(len.W))
  val lzc = Output(UInt(log2Up(len).W))
  val error = Output(Bool())
  val zero = Output(Bool())
}
class LZA(len: Int) extends Module {
  val io = IO(new LzaIO(len))
  val (a, b) = (io.a, io.b)
  val g, s, e = Wire(Vec(len, Bool()))
  for (i <- 0 until len){
    g(i) := a(i) & (~b(i))
    s(i) := (~a(i)) & b(i)
    e(i) := ~(a(i) ^ b(i))
  }

  val preEncoder = Module(new PreEncoder(len))
  preEncoder.io.g := Cat(g.reverse)
  preEncoder.io.s := Cat(s.reverse)
  preEncoder.io.e := Cat(e.reverse)
  val f = preEncoder.io.f

  val errorDetector = Module(new ErrorDetector(len))
  errorDetector.io.g := Cat(g.reverse)
  errorDetector.io.s := Cat(s.reverse)
  errorDetector.io.e := Cat(e.reverse)

  io.error := errorDetector.io.y
  io.lzc := LZC(f)
  io.zero := !f.orR
}

object LZA8 extends App {
  emitVerilog(new LZA(8), Array("--target-dir", "generated"))
}

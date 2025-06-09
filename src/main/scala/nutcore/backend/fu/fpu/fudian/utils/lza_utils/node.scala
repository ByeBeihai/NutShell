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
package nutcore.backend.fu.fpu.fudian.utils.lza_utils

import chisel3._
import chisel3.util._

abstract class NodeM2N(x: Int, y: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(Vec(3, UInt(x.W)))
    val out = Output(Vec(3, UInt(y.W)))
  })
}

class node extends NodeM2N(3, 2){
  val (p, n, z) = (io.in(0), io.in(1), io.in(2))
  val pout, nout, zout = Wire(Vec(2,Bool()))
  pout(0) := p(0) | (z(0) & p(1))
  pout(1) := ((~z(0)) & p(1)) | (p(2) & (z(0) | z(1)))
  zout(0) := z(0) & z(1)
  zout(1) := z(2) & (z(0) | z(1))
  nout(0) := ~(p(0) | z(0))
  nout(1) := ~(p(1) | z(1))
  io.out(0) := Cat(pout.reverse)
  io.out(1) := Cat(nout.reverse)
  io.out(2) := Cat(zout.reverse)
}

//class node(Pin: UInt, Nin: UInt, Zin: UInt)(sel: Int) = {
//  val Pout, Zout, Nout = Wire(Vec(2, Bool()))
//  Pout(0) := Pin(0) | (Zin(0) & Pin(1))
//  Pout(1) := (~Zin(0)) & Pin(1) | Pin(2) & (Zin(0) | Zin(1))
//  Zout(0) := Zin(0) & Zin(1)
//  Zout(1) := Zin(2) & (Zin(0) | Zin(1))
//  Nout(0) := ~(Pout(0) | Zout(0))
//  Nout(1) := ~(Pout(1) | Zout(1))
//  sel match {
//    case 0 => Pout
//    case 1 => Zout
//    case 2 => Nout
//  }
//}
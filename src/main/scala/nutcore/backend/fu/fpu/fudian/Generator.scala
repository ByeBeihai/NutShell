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
package nutcore.backend.fu.fpu.fudian

import chisel3.RawModule
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

object Generator extends App {

  def getModuleGen(
    name:      String,
    expWidth:  Int,
    precision: Int
  ): () => RawModule = {
    val pkg = this.getClass.getPackageName
    name match {
      case "FPToFP" =>
        val (inE, inP, oE, oP) = expWidth match {
          case -1 => (8, 24, 11, 53)
          case -2 => (11, 53, 8, 24)
        }
        () => new FPToFP(inE, inP, oE, oP)
      case _ =>
        val c =
          Class
            .forName(pkg + "." + name)
            .getConstructor(Integer.TYPE, Integer.TYPE)
        () =>
          c.newInstance(
            expWidth.asInstanceOf[Object],
            precision.asInstanceOf[Object]
          ).asInstanceOf[RawModule]
    }
  }

  val (module, expWidth, precision, firrtlOpts) = ArgParser.parse(args)
  (new ChiselStage).execute(
      firrtlOpts,
      Seq(
        ChiselGeneratorAnnotation(getModuleGen(module, expWidth, precision))
      )
    )
}

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

object ArgParser {

  val help =
    """
      |Usage:
      |--fu <name>
      |--ftype <32|64>
      |
      |example: --fu FADD -ftype 64 -td build
      |""".stripMargin

  /**
    * parse command args
    * @param args command line args
    * @return (()=>fu, firrtlOpts)
    */
  def parse(args: Array[String]): (String, Int, Int, Array[String]) = {
    var firrtpOpts = List[String]()
    var expWidth:  Option[Int] = None
    var precision: Option[Int] = None
    var module:    Option[String] = None
    def nextArg(list: List[String]): Unit = list match {
      case "--fu" :: name :: tail =>
        module = Some(name)
        nextArg(tail)
      case "--ftype" :: "32" :: tail =>
        expWidth = Some(8)
        precision = Some(24)
        nextArg(tail)
      case "--ftype" :: "64" :: tail =>
        expWidth = Some(11)
        precision = Some(53)
        nextArg(tail)
      case "--ftype" :: "32_64" :: tail =>
        expWidth = Some(-1)
        precision = Some(-1)
        nextArg(tail)
      case "--ftype" :: "64_32" :: tail =>
        expWidth = Some(-2)
        precision = Some(-2)
        nextArg(tail)
      case unknown :: tail =>
        firrtpOpts :+= unknown
        nextArg(tail)
      case Nil =>
    }
    nextArg(args.toList)
    require(module.nonEmpty && expWidth.nonEmpty && precision.nonEmpty, help)
    (module.get, expWidth.get, precision.get, firrtpOpts.toArray)
  }

}

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

package device

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import utils._

import bus.simplebus._
import difftest._

class ClintIO extends Bundle {
  val mtip = Output(Bool())
  val msip = Output(Bool())
}

class AXI4CLINT(sim: Boolean = false) extends AXI4SlaveModule(new AXI4Lite, new ClintIO) {
  val mtime = RegInit(0.U(64.W))  // unit: us
  val mtimecmp = RegInit(0.U(64.W))
  val msip = RegInit(0.U(64.W))

  val clk = (if (!sim) 100 /* 100MHz / 1000000 */ else 100)
  val freq = RegInit(clk.U(16.W))
  val inc = RegInit(1.U(16.W))

  val cnt = RegInit(0.U(16.W))
  val nextCnt = cnt + 1.U
  cnt := Mux(nextCnt < freq, nextCnt, 0.U)
  val tick = (nextCnt === freq)
  when (tick) { mtime := mtime + inc }

  if (sim) {
    val isWFI = WireInit(false.B)
    BoringUtils.addSink(isWFI, "isWFI")
    when (isWFI) { mtime := mtime + 100000.U }
  }

  val mapping = Map(
    RegMap(0x0, msip),
    RegMap(0x4000, mtimecmp),
    RegMap(0x8000, freq),
    RegMap(0x8008, inc),
    RegMap(0xbff8, mtime)
  )
  def getOffset(addr: UInt) = addr(15,0)

  RegMap.generate(mapping, getOffset(raddr), in.r.bits.data,
    getOffset(waddr), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))

  io.extra.get.mtip := RegNext(mtime >= mtimecmp && mtimecmp =/= 0.U(64.W))
  io.extra.get.msip := RegNext(msip =/= 0.U)
  //printf("mtime %x mtimecmp %x freq %x\n",mtime,mtimecmp,freq);
  BoringUtils.addSource(freq, "freq")
}

class ODINIO(val N: Int, val M: Int) extends Bundle {
 val CLK = Input(Clock())
 val RST = Input(Bool())
 val SCK = Input(Clock())
 val MOSI = Input(UInt(1.W))
 val MISO = Output(UInt(1.W))
 val AERINADDR = Input(UInt((2*M+1).W))
 val AERINREQ = Input(UInt(1.W))
 val AERINACK = Output(UInt(1.W))
 val AEROUTADDR = Output(UInt(M.W))
 val AEROUTREQ = Output(UInt(1.W))
 val AEROUTACK = Input(UInt(1.W))
}

trait HasODINIO extends BaseModule{
 val N: Int
 val M: Int
}

class ODINBlackBox(val N: Int, val M: Int) extends BlackBox(Map("N" -> N, "M" -> M)) with HasBlackBoxResource with HasODINIO{
 val io = IO(new Bundle{
    val CLK = Input(Clock())
    val RST = Input(Bool())
    val SCK = Input(Clock())
    val MOSI = Input(UInt(1.W))
    val MISO = Output(UInt(1.W))
    val AERIN_ADDR = Input(UInt((2*M+1).W))
    val AERIN_REQ = Input(UInt(1.W))
    val AERIN_ACK = Output(UInt(1.W))
    val AEROUT_ADDR = Output(UInt(M.W))
    val AEROUT_REQ = Output(UInt(1.W))
    val AEROUT_ACK = Input(UInt(1.W))
 }).suggestName("io")
 //addResource( "/ODINBlackBox.v")
 //addResource( "/aer_out.v")
 //addResource( "/controller.v")
 //addResource( "/fifo.v")
 //addResource( "/izh_calcium.v")
 //addResource( "/izh_effective_threshold.v" )
 //addResource( "izh_input_accumulator.v" )
 //addResource( "izh_neuron_state.v")
 //addResource( "izh_neuron.v" )
 //addResource( "izh_stimulation_strength.v" )
 //addResource( "lif_calcium.v" )
 //addResource( "lif_neuron_state.v" )
 //addResource( "lif_neuron.v" )
 //addResource( "neuron_core.v" )
 //addResource( "sdsp_update.v" )
 ///addResource( "spi_slave.v" )
 //addResource( "synaptic_core.v" )
 //addResource( "scheduler.v" )
}

class AXI4ODIN(sim: Boolean = false) extends AXI4SlaveModule(new AXI4Lite) {

  def N:Int = 256
  def M:Int = 8
  def slow:Int = 4 //spi接口的速度设置为比标准时钟慢4倍
  val odin_impl=Module(new ODINBlackBox(N,M)) //例化黑盒状态的odin

  val clock_odin = WireInit(false.B)
  BoringUtils.addSink(clock_odin, "clock")
  odin_impl.io.CLK:=clock_odin.asClock
  odin_impl.io.RST:=reset.asBool

  //用MMIO的模式来读写aer端口
  val aerinack=RegInit(0.U(1.W))
  val aeroutack=RegInit(0.U(1.W))
  val aerinreq=RegInit(0.U(1.W))
  val aeroutreq=RegInit(0.U(1.W))
  val aerinaddr=RegInit(0.U((2*M+1).W))
  val aeroutaddr=RegInit(0.U((M).W))
 
  odin_impl.io.AEROUT_ACK:=aeroutack
  aeroutaddr:=odin_impl.io.AEROUT_ADDR
  aeroutreq:=odin_impl.io.AEROUT_REQ
  odin_impl.io.AERIN_REQ := aerinreq
  odin_impl.io.AERIN_ADDR := aerinaddr
  aerinack := odin_impl.io.AERIN_ACK

  //此处为aer转换到mmio后的内存映射地址 0x100000-0x200000用于这段映射
  val mapping = Map(
    MaskedRegMap(0x100000, aeroutreq,MaskedRegMap.UnwritableMask),
    MaskedRegMap(0X100010, aeroutaddr,MaskedRegMap.UnwritableMask),
    MaskedRegMap(0x100020, aeroutack),
    MaskedRegMap(0x100030, aerinreq),
    MaskedRegMap(0x100040, aerinaddr),
    MaskedRegMap(0x100050, aerinack,MaskedRegMap.UnwritableMask))
  def getOffset(addr: UInt) = addr(23,0)
  MaskedRegMap.generate(mapping, getOffset(raddr), in.r.bits.data,
    getOffset(waddr), in.w.fire(), in.w.bits.data)
  
  //设置SPI传输所需要的寄存器，spi的读写传输都是20+20个spi周期，前20周期为地址传输 后20周期为数据传输
  val spi_finish = RegInit(0.U(2.W)) //
  val spi_addr = RegInit(0.U(20.W)) //
  val spi_data = RegInit(0.U(20.W)) //
  val spi_cnt  = RegInit(0.U(log2Up(slow*40).W)) //
  val spi_rdata= Reg(Vec(40, UInt(1.W))) //

  //单独设置SPI的时钟，以4个标准时钟为一个SPI时钟周期，计数器以1 2 3结尾时为高电平 以0结尾时低电平
  odin_impl.io.SCK:= (spi_cnt(1,0) =/= 0.U).asClock()

  //读写请求（均以写请求的方式出现，以地址最高位区分实际的读写）落在0x000000-0x100000之间 激活SPI读写组件
  when(~waddr(20) && in.w.fire()){
    spi_finish := Cat(1.U(1.W),waddr(19)) //高位表示是否处于SPI读写任务中，低位表示当前任务是读还是写 
    spi_addr   := waddr(19,0)
    spi_data   := in.w.bits.data(19,0) //写任务的数据
    spi_cnt    := 0.U //读写任务计数器重置
    spi_rdata  := 0.U.asTypeOf(spi_rdata) //读任务的读数据，为代码编写方便，只用第39-20位。0-19位无操作
  }
  when(spi_finish(1)){
    spi_cnt := spi_cnt + 1.U
  }
  when(spi_cnt === (slow * 39+3).U){ //当计数器处于第39个spi周期的第3个普通时钟时，下一拍结束读写任务
    spi_cnt := 0.U
    spi_finish := Cat(0.U,spi_finish(0))
    spi_addr := 0.U
  }

  when(spi_finish(1) && spi_addr(19)){ //读任务模式
    when((spi_cnt >> 2) < 20.U){ //前20个周期通过MOSI传输读地址
      odin_impl.io.MOSI := Reverse(spi_addr)((spi_cnt >> 2)) //spi_cnt右移2位（也就是除4）后得到的是当前的SPI周期数
      odin_impl.io.MISO := DontCare
    }.otherwise{ //后20个周期通过MISO读取数据
      odin_impl.io.MOSI := DontCare
      when(spi_cnt(1,0) === 2.U){spi_rdata(spi_cnt >> 2) := odin_impl.io.MISO} //当前的SPI周期数大于等于20时从MISO读取数据。从spi_rdata的第20位开始存储
    }
  }.elsewhen(spi_finish(1) && ~spi_addr(19)){//写任务模式
    val total_trans_bag = Reverse(Cat(spi_addr,spi_data)) //写任务只需要走MOSI，依次传输地址和数据
    odin_impl.io.MOSI := total_trans_bag((spi_cnt >> 2))
  }

  when(getOffset(raddr) === 0x100060.U){//将spi_finish和spi_rdata拼接为10位的数据，映射到0x100018且是只读状态,处理器可以通过读取这个地址来判断SPI传输任务是否结束（看最高位）
    val rdata = spi_rdata.asUInt        //对于读任务，该10位数据的低8位就是读任务完成后从SPI读出来的8位数据
    in.r.bits.data := Cat(spi_finish,Reverse(rdata(39,32)))
  }
  when(reset.asBool){
    spi_rdata := 0.U.asTypeOf(spi_rdata)
  }
  Debug() {
    printf("[odin] aerinreq %x raddr %x waddr %x rdata %x wdata%x wen %x \n",aerinreq,getOffset(raddr),getOffset(waddr),in.r.bits.data,in.w.bits.data, in.w.fire())
  }
  //printf("[SPI] sck %x mosi %x miso %x spi_cnt %x spi_addr %x spi_data %x \n",odin_impl.io.SCK.asBool,odin_impl.io.MOSI,odin_impl.io.MISO,spi_cnt,spi_addr,spi_data)
}

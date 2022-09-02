package nutcore
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import top.Settings

class SIMD_LSU extends NutCoreModule with HasLSUConst {
  val io = IO(new UnpipeLSUIO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, dtlbPF: Bool): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    dtlbPF := io.dtlbPF
    io.out.bits
  }
    val lsExecUnit = Module(new LSExecUnit)
    lsExecUnit.io.instr := DontCare
    io.dtlbPF := lsExecUnit.io.dtlbPF
    io.dmem <> lsExecUnit.io.dmem
    io.out.bits := lsExecUnit.io.out.bits
    io.loadAddrMisaligned := lsExecUnit.io.loadAddrMisaligned
    io.storeAddrMisaligned := lsExecUnit.io.storeAddrMisaligned

    // LSU control FSM state
    val s_idle :: s_exec :: s_load :: s_lr :: s_sc :: s_amo_l :: s_amo_a :: s_amo_s :: Nil = Enum(8)

    // LSU control FSM
    val state = RegInit(s_idle)

    if(!IndependentAddrCalcState){
        lsExecUnit.io.in.valid     := io.in.valid
        lsExecUnit.io.out.ready    := io.out.ready 
        lsExecUnit.io.in.bits.src1 := src1 + src2
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := func
        lsExecUnit.io.wdata        := io.wdata
        io.in.ready                := lsExecUnit.io.out.fire() 
        io.out.valid               := lsExecUnit.io.out.valid  
        state := s_idle
    }

    when(io.loadAddrMisaligned || io.storeAddrMisaligned){
      state := s_idle
      io.out.valid := true.B
      io.in.ready := true.B
    }

    val lsuMMIO = WireInit(false.B)
    BoringUtils.addSink(lsuMMIO, "lsuMMIO")

    val mmioReg = RegInit(false.B)
    when (!mmioReg) { mmioReg := lsuMMIO }
    when (io.out.valid) { mmioReg := false.B }
    io.isMMIO := mmioReg && io.out.valid
}
class new_lsu extends NutCoreModule with HasLSUConst {
  val io = IO(new UnpipeLSUIO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, dtlbPF: Bool): UInt = {
    this.valid := valid
    this.src1 := src1+src2
    this.src2 := src2
    this.func := func
    dtlbPF := io.dtlbPF
    io.out.bits
  }

  val addr = src1
  val addrLatch = addr
  val isStore = valid && LSUOpType.isStore(func)
  val partialLoad = !isStore && (func =/= LSUOpType.ld)

  val s_idle :: s_wait_resp :: Nil = Enum(2)
  val state = RegInit(s_idle)

  val dtlbFinish = WireInit(false.B)
  val dtlbPF = WireInit(false.B)
  val dtlbEnable = WireInit(false.B)
  io.dtlbPF := dtlbPF
  if (Settings.get("HasDTLB")) {
    BoringUtils.addSink(dtlbFinish, "DTLBFINISH")
    BoringUtils.addSink(dtlbPF, "DTLBPF")
    BoringUtils.addSink(dtlbEnable, "DTLBENABLE")
  }

  switch (state) {
    is (s_idle) { when (io.dmem.req.fire()) { state := s_wait_resp } }
    is (s_wait_resp) { when (io.dmem.resp.fire()) { state := s_idle } }
  }

  Debug(io.dmem.req.fire(), "[LSU] %x, size %x, wdata_raw %x, isStore %x\n", addr, func(1,0), io.wdata, isStore)

  val size = func(1,0)
  val reqAddr  = Mux((XLEN>=VAddrBits).B,addr(VAddrBits-1,0),SignExt(addr,VAddrBits))
  val reqWdata = LookupTree(func(1,0),List(
                  0.U -> Fill(XLEN/8 ,io.wdata(7 ,0)),
                  1.U -> Fill(XLEN/16,io.wdata(15,0)),
                  2.U -> Fill(XLEN/32,io.wdata(31,0)),
                  3.U -> Fill(XLEN/64,io.wdata(63,0))
                ))
  val reqWmask = LookupTree(func(1,0),List(
                  0.U -> (0x1.U << addr(XLEN/32,0)), 
                  1.U -> (0x3.U << addr(XLEN/32,0)), 
                  2.U -> (0xf.U << addr(XLEN/32,0)), 
                  3.U -> (0xff.U<< addr(XLEN/32,0)) 
                ))
  val rdata = io.dmem.resp.bits.rdata
  val rdataSel = LookupTree(addr(XLEN/32, 0), List(
                  0.U -> rdata(XLEN-1, 0),
                  1.U -> rdata(XLEN-1, 8),
                  2.U -> rdata(XLEN-1, 16),
                  3.U -> rdata(XLEN-1, 24),
                  4.U -> rdata(XLEN-1, 32),
                  5.U -> rdata(XLEN-1, 40),
                  6.U -> rdata(XLEN-1, 48),
                  7.U -> rdata(XLEN-1, 56)
                ))
  val rdataPartialLoad = LookupTree(func, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))
  val addrAligned = LookupTree(func(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (addr(0) === 0.U),   //h
    "b10".U   -> (addr(1,0) === 0.U), //w
    "b11".U   -> (addr(2,0) === 0.U)  //d
  ))
  io.dmem.req.bits.apply(
    addr = reqAddr, 
    size = size, 
    wdata = reqWdata,
    wmask = reqWmask,
    cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read))
  io.dmem.req.valid := valid && (state === s_idle) && !io.loadAddrMisaligned && !io.storeAddrMisaligned
  io.dmem.resp.ready := true.B
  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata(XLEN-1,0))
  io.out.valid := Mux( dtlbPF && state =/= s_idle || io.loadAddrMisaligned || io.storeAddrMisaligned, true.B, io.dmem.resp.fire() && (state === s_wait_resp))
  io.in.ready := (state === s_idle) || dtlbPF

  Debug(io.out.fire(), "[LSU-EXECUNIT] state %x dresp %x dpf %x lm %x sm %x\n", state, io.dmem.resp.fire(), dtlbPF, io.loadAddrMisaligned, io.storeAddrMisaligned)


  io.isMMIO := DontCare

  val isAMO = WireInit(false.B)
  BoringUtils.addSink(isAMO, "ISAMO2")
  BoringUtils.addSource(addr, "LSUADDR")

  io.loadAddrMisaligned :=  valid && !isStore && !isAMO && !addrAligned
  io.storeAddrMisaligned := valid && (isStore || isAMO) && !addrAligned

  Debug(io.loadAddrMisaligned || io.storeAddrMisaligned, "misaligned addr detected\n")

  BoringUtils.addSource(io.dmem.isRead() && io.dmem.req.fire(), "perfCntCondMloadInstr")
  BoringUtils.addSource(BoolStopWatch(io.dmem.isRead(), io.dmem.resp.fire()), "perfCntCondMloadStall")
  BoringUtils.addSource(BoolStopWatch(io.dmem.isWrite(), io.dmem.resp.fire()), "perfCntCondMstoreStall")
  BoringUtils.addSource(io.isMMIO, "perfCntCondMmmioInstr")


    val lsuMMIO = WireInit(false.B)
    BoringUtils.addSink(lsuMMIO, "lsuMMIO")

    val mmioReg = RegInit(false.B)
    when (!mmioReg) { mmioReg := lsuMMIO }
    when (io.out.valid) { mmioReg := false.B }
    io.isMMIO := mmioReg && io.out.valid
}


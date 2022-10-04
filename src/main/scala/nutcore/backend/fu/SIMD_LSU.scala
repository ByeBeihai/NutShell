package nutcore
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import top.Settings

class SIMD_LSU_IO extends FunctionUnitIO {
  val wdata = Input(UInt(XLEN.W))
  val dmem = new SimpleBusUC(addrBits = VAddrBits)
  val isMMIO = Output(Bool())
  val loadAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
  val storeAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
  val flush = Input(Bool())
  val DecodeOut = new DecodeIO
  val DecodeIn = Flipped(new DecodeIO)
}

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

class LSU_RESP extends NutCoreModule with HasLSUConst{
  val io = IO(new Bundle {
      val DecodeIn = Flipped(Decoupled(new DecodeIO))
      val DecodeOut = new DecodeIO
      val flush = Input(Bool())
      val dmemresp = Flipped(Decoupled(new SimpleBusRespBundle(0, 0)))
      val isMMIO = Output(Bool())
      val addr = Input(UInt(XLEN.W))
      val func = Input(FuOpType())
      val out = Decoupled(Output(UInt(XLEN.W)))
  })

  io.dmemresp.ready := true.B

  val rdatacache = RegInit(0.U(64.W))
  val s_wait_resp :: s_wait_fire ::Nil = Enum(2)
  val state = RegInit(s_wait_resp)

  val addr = io.addr
  val func = io.func
  val rdata = Mux(state ===s_wait_fire,rdatacache, io.dmemresp.bits.rdata)
  val isStore = LSUOpType.isStore(func)
  val partialLoad = !isStore && (func =/= LSUOpType.ld)
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
  switch (state) {
    is (s_wait_resp) { when (io.dmemresp.fire() && io.out.fire() || io.flush) { state := s_wait_resp 
                      }.elsewhen(io.dmemresp.fire() && !io.out.fire()) {state := s_wait_fire
                                                                        rdatacache := rdata} }
    is (s_wait_fire) { when(io.out.fire() || io.flush){state := s_wait_resp}}
  }
  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata(XLEN-1,0))
  io.out.valid := io.dmemresp.fire() && (state === s_wait_resp)|| state ===s_wait_fire
  io.DecodeOut := io.DecodeIn.bits

  io.DecodeIn.ready := !io.DecodeIn.valid || io.out.fire()

  val lsuMMIO = WireInit(false.B)
  BoringUtils.addSink(lsuMMIO, "lsuMMIO")

  val mmioReg = RegInit(false.B)
  when (!mmioReg) { mmioReg := lsuMMIO }
  when (io.out.valid) { mmioReg := false.B }
  io.isMMIO := mmioReg && io.out.valid

  Debug( "[LSURESP] valid %x pc %x instno %x\n outfire %x \n",io.DecodeIn.valid, io.DecodeOut.cf.pc,io.DecodeOut.InstNo,io.out.fire())
}


class pipeline_lsu extends NutCoreModule with HasLSUConst {
  val io = IO(new SIMD_LSU_IO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  val DecodeIn = io.DecodeIn
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1+src2
    this.src2 := src2
    this.func := func
    io.out.bits
  }
  val lsu_resp = Module(new LSU_RESP)
  val s_idle :: s_wait_req ::Nil = Enum(2)
  val state = RegInit(s_wait_req)
  val addr = src1
  val isStore = valid && LSUOpType.isStore(func)
  val pipefire = WireInit(false.B)

  Debug( "[LSU] pc %x instno %x addr %x, size %x, wdata_raw %x, isStore %x reqfire %x \n", DecodeIn.cf.pc,DecodeIn.InstNo, addr, func(1,0), io.wdata, isStore,io.dmem.req.fire())

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
  val addrAligned = LookupTree(func(1,0), List(
                  0.U   -> true.B,              
                  1.U   -> (addr(0) === 0.U),   
                  2.U   -> (addr(1,0) === 0.U), 
                  3.U   -> (addr(2,0) === 0.U)  
  ))

  io.dmem.req.bits.apply(
    addr = reqAddr, 
    size = size, 
    wdata = reqWdata,
    wmask = reqWmask,
    cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read))

  io.dmem.req.valid := valid && (state === s_idle) && !io.loadAddrMisaligned && !io.storeAddrMisaligned && !io.flush 
  io.dmem.resp.ready := true.B
  io.out.bits := lsu_resp.io.out.bits
  io.out.valid := lsu_resp.io.out.valid
  lsu_resp.io.out.ready := io.out.ready
  lsu_resp.io.flush := io.flush
  lsu_resp.io.dmemresp <> io.dmem.resp 
  io.in.ready := pipefire || !valid
  io.isMMIO := lsu_resp.io.isMMIO
  io.DecodeOut := lsu_resp.io.DecodeOut
  Debug("[LSU-EXECUNIT] state %x dresp %x dpf %x lm %x sm %x resfire %x outvalid %x \n", state, io.dmem.resp.fire(), false.B, io.loadAddrMisaligned, io.storeAddrMisaligned,io.dmem.resp.fire(),io.out.valid)

  switch (state) {
    is (s_idle) { when(io.flush || io.dmem.req.fire() && pipefire){
                    state := s_idle
                }.elsewhen (io.dmem.req.fire() && !pipefire){ 
                    state := s_wait_req } }
    is (s_wait_req) {when (pipefire || io.flush) { state := s_idle }}
  }

  io.loadAddrMisaligned :=  valid && !isStore && !addrAligned
  io.storeAddrMisaligned := valid && isStore && !addrAligned

  Debug(io.loadAddrMisaligned || io.storeAddrMisaligned, "misaligned addr detected\n")

  BoringUtils.addSource(io.dmem.isRead() && io.dmem.req.fire(), "perfCntCondMloadInstr")
  BoringUtils.addSource(BoolStopWatch(io.dmem.isRead(), io.dmem.resp.fire()), "perfCntCondMloadStall")
  BoringUtils.addSource(BoolStopWatch(io.dmem.isWrite(), io.dmem.resp.fire()), "perfCntCondMstoreStall")
  BoringUtils.addSource(io.isMMIO, "perfCntCondMmmioInstr")

  BoringUtils.addSource(addr, "LSUADDR")
  BoringUtils.addSource(pipefire, "lsu_firststage_fire")

  //connect it with lsuresp
  val lsu_resp_valid = RegInit(false.B)
  when (lsu_resp.io.out.fire()) { lsu_resp_valid := false.B }
  val lsu_resp_invalid = (state === s_idle && io.dmem.req.fire() || state === s_wait_req)
  pipefire :=  lsu_resp_invalid && lsu_resp.io.DecodeIn.ready
  when (lsu_resp_invalid && lsu_resp.io.DecodeIn.ready) { lsu_resp_valid := true.B }
  when (io.flush) { lsu_resp_valid := false.B }

  lsu_resp.io.DecodeIn.bits := RegEnable(DecodeIn, lsu_resp_invalid && lsu_resp.io.DecodeIn.ready)
  lsu_resp.io.addr := RegEnable(addr, lsu_resp_invalid && lsu_resp.io.DecodeIn.ready)
  lsu_resp.io.func := RegEnable(func, lsu_resp_invalid && lsu_resp.io.DecodeIn.ready)
  lsu_resp.io.DecodeIn.valid := lsu_resp_valid
}

class multicycle_SIMD_LSU_IO extends FunctionUnitIO {
  val wdata = Input(UInt(XLEN.W))
  val dmem = new SimpleBusUC(addrBits = VAddrBits)
  val isMMIO = Output(Bool())
  val loadAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
  val storeAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
  val flush = Input(Bool())
}

class multi_cycle_lsu extends NutCoreModule with HasLSUConst {
  val io = IO(new SIMD_LSU_IO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1+src2
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val addr = src1
  val isStore = valid && LSUOpType.isStore(func)
  val partialLoad = !isStore && (func =/= LSUOpType.ld)
  val rdatacache = RegInit(0.U(64.W))

  val s_idle :: s_wait_resp :: s_wait_fire ::Nil = Enum(3)
  val state = RegInit(s_idle)

  Debug( "[LSU] addr %x, size %x, wdata_raw %x, isStore %x reqfire %x \n", addr, func(1,0), io.wdata, isStore,io.dmem.req.fire())

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
  val rdata = Mux(state ===s_wait_fire,rdatacache, io.dmem.resp.bits.rdata)
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
                  0.U   -> true.B,              
                  1.U   -> (addr(0) === 0.U),   
                  2.U   -> (addr(1,0) === 0.U), 
                  3.U   -> (addr(2,0) === 0.U)  
  ))
  io.dmem.req.bits.apply(
    addr = reqAddr, 
    size = size, 
    wdata = reqWdata,
    wmask = reqWmask,
    cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read))
  io.dmem.req.valid := valid && (state === s_idle) && !io.loadAddrMisaligned && !io.storeAddrMisaligned && !io.flush
  io.dmem.resp.ready := true.B
  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata(XLEN-1,0))
  io.out.valid := Mux( io.loadAddrMisaligned || io.storeAddrMisaligned, false.B, io.dmem.resp.fire() && (state === s_wait_resp)|| state ===s_wait_fire)
  io.in.ready := !valid || io.out.fire()
  io.isMMIO := DontCare
  Debug("[LSU-EXECUNIT] state %x dresp %x dpf %x lm %x sm %x resfire %x outvalid %x \n", state, io.dmem.resp.fire(), false.B, io.loadAddrMisaligned, io.storeAddrMisaligned,io.dmem.resp.fire(),io.out.valid)

  switch (state) {
    is (s_idle) { when(io.flush){
                    state := s_idle
                }.elsewhen (io.dmem.req.fire()){ 
                    state := s_wait_resp } }
    is (s_wait_resp) { when (io.dmem.resp.fire() && io.out.fire() || io.flush) { state := s_idle 
                      }.elsewhen(io.dmem.resp.fire() && !io.out.fire()) {state := s_wait_fire
                                                                        rdatacache := rdata} }
    is (s_wait_fire) { when(io.out.fire() || io.flush){state := s_idle}}
  }

  io.loadAddrMisaligned :=  valid && !isStore && !addrAligned
  io.storeAddrMisaligned := valid && isStore && !addrAligned

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

  BoringUtils.addSource(addr, "LSUADDR")
  BoringUtils.addSource(io.out.fire(), "lsu_firststage_fire")

  io.DecodeOut := io.DecodeIn
}

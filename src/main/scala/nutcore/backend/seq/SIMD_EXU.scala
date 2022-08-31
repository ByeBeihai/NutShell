package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import top.Settings
import difftest._

class SIMD_EXU(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val in = Vec(Issue_Num,Flipped(Decoupled(new DecodeIO)))
    val out = Vec(Issue_Num,Decoupled(new CommitIO))
    val flush = Input(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val forward = Vec(Issue_Num,new ForwardIO)
    val memMMU = Flipped(new MemMMUIO)
  })

  def WhoTakeIt(futype:UInt):UInt = PriorityMux(VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.fuType === futype && io.in(i).valid)).zipWithIndex.map{case(a,b) => (a,b.U)})
  def isBru(func: UInt) = func(4)

  val src1     = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.data.src1))
  val src2     = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.data.src2))
  val fuType   = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.fuType))
  val fuOpType = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.fuOpType))

  val fuValids = VecInit((0 to FuType.num).map(i => {val raw = Wire(Vec(Issue_Num,Bool()))
                                                  for(j <- 0 to Issue_Num-1){
                                                      raw(j) := fuType(j) === i.U && io.in(j).valid && !io.flush
                                                  }
                                                  raw.reduce(_||_)
                                                 }))
  //ALU
  val alu = Module(new ALU(hasBru = true))
  val WhoTakeAlu = WhoTakeIt(FuType.alu)
  val aluOut = alu.access(valid = fuValids(FuType.alu), src1 = src1(WhoTakeAlu), src2 = src2(WhoTakeAlu), func = fuOpType(WhoTakeAlu))
  alu.io.cfIn := io.in(WhoTakeAlu).bits.cf
  alu.io.offset := io.in(WhoTakeAlu).bits.data.imm
  alu.io.out.ready := true.B

  //LSU
  val lsu = Module(new UnpipelinedLSU)
  val WhoTakeLsu = WhoTakeIt(FuType.csr)
  val lsuTlbPF = WireInit(false.B)
  val lsuOut = lsu.access(valid = fuValids(FuType.lsu), src1 = src1(WhoTakeLsu), src2 = io.in(WhoTakeLsu).bits.data.imm, func = fuOpType(WhoTakeLsu), dtlbPF = lsuTlbPF)
  lsu.io.wdata := src2(WhoTakeLsu)
  lsu.io.instr := io.in(WhoTakeLsu).bits.cf.instr
  for(i <- 0 to Issue_Num-1){
    io.out(i).bits.isMMIO := i.U === WhoTakeLsu && (lsu.io.isMMIO || (AddressSpace.isMMIO(io.in(WhoTakeLsu).bits.cf.pc) && io.out(i).valid))
  }
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := true.B

  //MDU
  val mdu = Module(new MDU)
  val WhoTakeMdu = WhoTakeIt(FuType.mdu)
  val mduOut = mdu.access(valid = fuValids(FuType.mdu), src1 = src1(WhoTakeMdu), src2 = src2(WhoTakeMdu), func = fuOpType(WhoTakeMdu))
  mdu.io.out.ready := true.B

  //CSRU
  val csr = Module(new CSR)
  val WhoTakeCsr = WhoTakeIt(FuType.csr)
  val csrOut = csr.access(valid = fuValids(FuType.csr), src1 = src1(WhoTakeCsr), src2 = src2(WhoTakeCsr), func = fuOpType(WhoTakeCsr))
  csr.io.cfIn := io.in(WhoTakeCsr).bits.cf
  csr.io.cfIn.exceptionVec(loadAddrMisaligned) := lsu.io.loadAddrMisaligned
  csr.io.cfIn.exceptionVec(storeAddrMisaligned) := lsu.io.storeAddrMisaligned
  csr.io.instrValid := io.in(WhoTakeCsr).valid && !io.flush
  csr.io.isBackendException := false.B
  for(i <- 0 to Issue_Num-1){
    io.out(i).bits.intrNO := csr.io.intrNO
  }
  csr.io.isBackendException := false.B
  csr.io.out.ready := true.B
  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  //MOU
  val mou = Module(new MOU)
  val WhoTakeMou = WhoTakeIt(FuType.mou)
  mou.access(valid = fuValids(FuType.mou), src1 = src1(WhoTakeMou), src2 = src2(WhoTakeMou), func = fuOpType(WhoTakeMou))
  mou.io.cfIn := io.in(WhoTakeMou).bits.cf
  mou.io.out.ready := true.B

  val empty_RedirectIO = Wire(new RedirectIO)
  empty_RedirectIO.target := 0.U
  empty_RedirectIO.rtype := 0.U
  empty_RedirectIO.valid := false.B

  for(i <- 0 to Issue_Num-1){
    io.out(i).bits.decode := DontCare
  }
  for(k <- 0 to Issue_Num-1){
    (io.out(k).bits.decode.ctrl, io.in(k).bits.ctrl) match { case (o, i) =>
        o.rfWen := i.rfWen && (!lsuTlbPF && !lsu.io.loadAddrMisaligned && !lsu.io.storeAddrMisaligned || !fuValids(FuType.lsu)) && !(csr.io.wenFix && fuValids(FuType.csr))
        o.rfDest := i.rfDest
        o.fuType := i.fuType
    }
  }
  for(k <- 0 to Issue_Num-1){
    io.out(k).bits.decode.cf.pc := io.in(k).bits.cf.pc
    io.out(k).bits.decode.cf.instr := io.in(k).bits.cf.instr
    io.out(k).bits.decode.cf.runahead_checkpoint_id := io.in(k).bits.cf.runahead_checkpoint_id
    io.out(k).bits.decode.cf.isBranch := io.in(k).bits.cf.isBranch
    io.out(k).bits.decode.cf.redirect <> Mux(mou.io.redirect.valid && WhoTakeMou === k.U, mou.io.redirect,
                                            Mux(csr.io.redirect.valid && WhoTakeCsr === k.U, csr.io.redirect, 
                                                Mux(WhoTakeAlu === k.U,alu.io.redirect,empty_RedirectIO)))
  }
  val ExuValid = VecInit((0 to Issue_Num-1).map(k => !io.in(k).valid || io.in(k).valid && MuxLookup(fuType(k), true.B, List(FuType.lsu -> lsu.io.out.valid,FuType.mdu -> mdu.io.out.valid))
                         )).reduce(_&&_)
  for(i <- 0 to Issue_Num-1){
    io.out(i).valid := Mux(io.in(i).valid,ExuValid,false.B) 
  }
  for(k <- 0 to Issue_Num-1){
    io.out(k).bits.commits(FuType.alu) := aluOut
    io.out(k).bits.commits(FuType.lsu) := lsuOut
    io.out(k).bits.commits(FuType.csr) := csrOut
    io.out(k).bits.commits(FuType.mdu) := mduOut
    io.out(k).bits.commits(FuType.mou) := 0.U
  }
  for(i <- 0 to Issue_Num-1){
      if(i == 0){
        io.in(i).ready := VecInit((0 to Issue_Num-1).map(i => !io.in(i).valid||io.out(i).fire())).reduce(_&&_)
      }else{
        io.in(i).ready := false.B
      }
  }
  val WAWinExu = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool())) 
                                                      for(j <- 0 to i){
                                                        raw(j) := false.B 
                                                      }
                                                      for(j <- i+1 to Issue_Num-1){
                                                        raw(j) := io.in(j).valid && io.in(j).bits.ctrl.rfDest === io.in(i).bits.ctrl.rfDest && io.in(j).bits.ctrl.rfWen
                                                      }
                                                      raw.reduce(_||_)}))
  for(i <- 0 to Issue_Num-1){
    io.forward(i).valid := io.in(i).valid & io.out(i).valid
    io.forward(i).wb.rfWen := io.in(i).bits.ctrl.rfWen && !WAWinExu(i)
    io.forward(i).wb.rfDest := io.in(i).bits.ctrl.rfDest
    io.forward(i).wb.rfData := MuxLookup(fuType(i),aluOut,List(FuType.alu->aluOut,FuType.lsu->lsuOut,FuType.csr->csrOut,FuType.mdu->mduOut))
    io.forward(i).fuType := io.in(i).bits.ctrl.fuType
  }

  if (!p.FPGAPlatform) {
    val cycleCnt = WireInit(0.U(64.W))
    val instrCnt = WireInit(0.U(64.W))
    val nutcoretrap = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.isNutCoreTrap && io.in(i).valid)).reduce(_||_)

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nutcoretrap, "nutcoretrap")

    val difftest = Module(new DifftestTrapEvent)
    difftest.io.clock    := clock
    difftest.io.coreid   := 0.U // TODO: nutshell does not support coreid auto config
    difftest.io.valid    := nutcoretrap
    difftest.io.code     := io.in(0).bits.data.src1
    difftest.io.pc       := io.in(0).bits.cf.pc
    difftest.io.cycleCnt := cycleCnt
    difftest.io.instrCnt := instrCnt
  }
}

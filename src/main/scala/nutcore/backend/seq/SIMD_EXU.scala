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

  def WhoTakeThisOperator(futype:UInt):UInt = PriorityMux(VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.fuType === futype && io.in(i).valid)).zipWithIndex.map{case(a,b) => (a,b.U)})

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
  val alu = Module(new ALU(hasBru = true,NO1 = true))
  val WhoTakeAlu = WhoTakeThisOperator(FuType.alu)
  val aluOut = alu.access(valid = fuValids(FuType.alu), src1 = src1(WhoTakeAlu), src2 = src2(WhoTakeAlu), func = fuOpType(WhoTakeAlu))
  alu.io.cfIn := io.in(WhoTakeAlu).bits.cf
  alu.io.offset := io.in(WhoTakeAlu).bits.data.imm
  alu.io.out.ready := true.B

  //ALU1
  val alu1 = Module(new ALU(hasBru = false,NO1 = false))
  val WhoTakeAlu1 = WhoTakeThisOperator(FuType.alu1)
  val alu1Out = alu1.access(valid = fuValids(FuType.alu1), src1 = src1(WhoTakeAlu1), src2 = src2(WhoTakeAlu1), func = fuOpType(WhoTakeAlu1))
  alu1.io.cfIn := io.in(WhoTakeAlu1).bits.cf
  alu1.io.offset := io.in(WhoTakeAlu1).bits.data.imm
  alu1.io.out.ready := true.B

  //LSU
  val lsu = Module(new new_lsu)
  val WhoTakeLsu = WhoTakeThisOperator(FuType.csr)
  val lsuOut = lsu.access(valid = fuValids(FuType.lsu), src1 = src1(WhoTakeLsu), src2 = io.in(WhoTakeLsu).bits.data.imm, func = fuOpType(WhoTakeLsu))
  lsu.io.wdata := src2(WhoTakeLsu)
  for(i <- 0 to Issue_Num-1){
    io.out(i).bits.isMMIO := i.U === WhoTakeLsu && (lsu.io.isMMIO || (AddressSpace.isMMIO(io.in(WhoTakeLsu).bits.cf.pc) && io.out(i).valid))
  }
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := true.B
  lsu.io.flush := io.flush

  //MDU
  val mdu = Module(new MDU)
  val WhoTakeMdu = WhoTakeThisOperator(FuType.mdu)
  val mduOut = mdu.access(valid = fuValids(FuType.mdu), src1 = src1(WhoTakeMdu), src2 = src2(WhoTakeMdu), func = fuOpType(WhoTakeMdu))
  mdu.io.out.ready := true.B
  mdu.io.flush := io.flush

  //CSRU
  val csr = Module(new CSR)
  val WhoTakeCsr = WhoTakeThisOperator(FuType.csr)
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

  //connect wires
  val empty_RedirectIO = Wire(new RedirectIO)
  empty_RedirectIO.target := 0.U
  empty_RedirectIO.rtype := 0.U
  empty_RedirectIO.valid := false.B

  val IcantWrite = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool())) 
                                                          for(j <- 0 to i){
                                                              raw(j) := ((lsu.io.loadAddrMisaligned || lsu.io.storeAddrMisaligned) && j.U === WhoTakeLsu) || (csr.io.wenFix && fuValids(FuType.csr) && j.U === WhoTakeCsr)
                                                          }
                                                          for(j <- i+1 to Issue_Num-1){
                                                              raw(j) := false.B 
                                                          }
                                                          raw.reduce(_||_)
                                                      }))

  for(i <- 0 to Issue_Num-1){
    io.out(i).bits.decode <> io.in(i).bits
  }
  Debug(io.out(0).fire(),"[SIMD_EXU] InstNo %x\n", io.out(0).bits.decode.InstNo)
  for(k <- 0 to Issue_Num-1){
    io.out(k).bits.decode.ctrl.rfWen := io.in(k).bits.ctrl.rfWen && !IcantWrite(k)
    io.out(k).bits.decode.cf.redirect <> Mux(csr.io.redirect.valid && WhoTakeCsr === k.U, csr.io.redirect, 
                                                Mux(alu.valid && WhoTakeAlu === k.U,alu.io.redirect,
                                                  Mux(alu1.valid && WhoTakeAlu1 === k.U,alu1.io.redirect,empty_RedirectIO)))
  }
  val ExuValid = VecInit((0 to Issue_Num-1).map(k => !io.in(k).valid || io.in(k).valid && MuxLookup(fuType(k), true.B, List(FuType.lsu -> lsu.io.out.valid,FuType.mdu -> mdu.io.out.valid))
                         )).reduce(_&&_)
  for(i <- 0 to Issue_Num-1){
    io.out(i).valid := Mux(io.in(i).valid,ExuValid,false.B) 
  }
  for(k <- 0 to Issue_Num-1){
    io.out(k).bits.commits:= DontCare
    io.out(k).bits.commits(FuType.alu) := aluOut
    io.out(k).bits.commits(FuType.lsu) := lsuOut
    io.out(k).bits.commits(FuType.csr) := csrOut
    io.out(k).bits.commits(FuType.mdu) := mduOut
    io.out(k).bits.commits(FuType.alu1):= alu1Out
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
    io.forward(i).wb.rfData := MuxLookup(fuType(i),aluOut,List(FuType.alu->aluOut,FuType.alu1->alu1Out,FuType.lsu->lsuOut,FuType.csr->csrOut,FuType.mdu->mduOut))
    io.forward(i).fuType := io.in(i).bits.ctrl.fuType
    io.forward(i).InstNo := io.in(i).bits.InstNo
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
  //Debug
  {
      Debug("[SIMD_EXU] valid %x pc %x futype %x \n", io.in(0).valid, io.in(0).bits.cf.pc, io.in(0).bits.ctrl.fuType)
      for(i<- 0 to Issue_Num-1){
      Debug("[SIMD_EXU] [Issue: %x ]TakeBranch %x BranchTo %x \n", i.U, io.out(i).bits.decode.cf.redirect.valid, io.out(i).bits.decode.cf.redirect.target)
      }
  }
}

class new_SIMD_EXU(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val in = Vec(FuType.num,Flipped(Decoupled(new DecodeIO)))
    val out = Vec(FuType.num,Decoupled(new CommitIO))
    val flush = Input(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val forward = Vec(FuType.num,new ForwardIO)
    val memMMU = Flipped(new MemMMUIO)
  })

  def notafter(ptr1:UInt,ptr2:UInt,flag1:UInt,flag2:UInt):Bool= (ptr1 <= ptr2) && (flag1 === flag2) || (ptr1 > ptr2) && (flag1 =/= flag2)

  val src1     = VecInit((0 to FuType.num-1).map(i => io.in(i).bits.data.src1))
  val src2     = VecInit((0 to FuType.num-1).map(i => io.in(i).bits.data.src2))
  val fuType   = VecInit((0 to FuType.num-1).map(i => io.in(i).bits.ctrl.fuType))
  val fuOpType = VecInit((0 to FuType.num-1).map(i => io.in(i).bits.ctrl.fuOpType))

  //ALU
  val aluidx = FuType.alu
  val alu = Module(new ALU(hasBru = true,NO1 = true))
  val aluOut = alu.access(valid = io.in(aluidx).valid, src1 = src1(aluidx), src2 = src2(aluidx), func = fuOpType(aluidx))
  alu.io.cfIn := io.in(aluidx).bits.cf
  alu.io.offset := io.in(aluidx).bits.data.imm
  alu.io.out.ready := true.B
  Debug("aluidx %x \n",aluidx)


  //ALU1
  val alu1idx = FuType.alu1
  val alu1 = Module(new ALU(hasBru = true,NO1 = false))
  val alu1Out = alu1.access(valid = io.in(alu1idx).valid, src1 = src1(alu1idx), src2 = src2(alu1idx), func = fuOpType(alu1idx))
  alu1.io.cfIn := io.in(alu1idx).bits.cf
  alu1.io.offset := io.in(alu1idx).bits.data.imm
  alu1.io.out.ready := true.B

  //LSU
  val lsuidx = FuType.lsu
  val lsu = Module(new new_lsu)
  val lsuOut = lsu.access(valid = io.in(lsuidx).valid, src1 = src1(lsuidx), src2 = io.in(lsuidx).bits.data.imm, func = fuOpType(lsuidx))
  lsu.io.wdata := src2(lsuidx)
  for(i <- 0 to FuType.num-1){
    io.out(i).bits.isMMIO := i.U === lsuidx && (lsu.io.isMMIO || (AddressSpace.isMMIO(io.in(lsuidx).bits.cf.pc) && io.out(i).valid))
  }
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := io.out(lsuidx).ready
  lsu.io.flush := io.flush

  //MDU
  val mduidx = FuType.mdu
  val mdu = Module(new MDU)
  val mduOut = mdu.access(valid = io.in(mduidx).valid, src1 = src1(mduidx), src2 = src2(mduidx), func = fuOpType(mduidx))
  mdu.io.out.ready := io.out(mduidx).ready
  mdu.io.flush := io.flush

  //MOU to be done
  val mouidx = FuType.mou

  //CSRU
  val csridx = FuType.csr
  val csr = Module(new CSR)
  val csrOut = csr.access(valid = io.in(csridx).valid, src1 = src1(csridx), src2 = src2(csridx), func = fuOpType(csridx))
  csr.io.cfIn := Mux(io.in(csridx).valid, io.in(csridx).bits.cf,io.in(lsuidx).bits.cf)
  csr.io.cfIn.exceptionVec(loadAddrMisaligned) := lsu.io.loadAddrMisaligned && !io.flush
  csr.io.cfIn.exceptionVec(storeAddrMisaligned) := lsu.io.storeAddrMisaligned && !io.flush
  csr.io.instrValid := (io.in(csridx).valid || lsu.io.loadAddrMisaligned || lsu.io.storeAddrMisaligned) && !io.flush //need to know what does it mean
  csr.io.isBackendException := false.B
  for(i <- 0 to FuType.num-1){
    io.out(i).bits.intrNO := csr.io.intrNO
  }
  csr.io.isBackendException := false.B
  csr.io.out.ready := true.B
  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  //connect wires
  val empty_RedirectIO = Wire(new RedirectIO)
  empty_RedirectIO.target := 0.U
  empty_RedirectIO.rtype := 0.U
  empty_RedirectIO.valid := false.B

  val IcantWrite = VecInit((0 to FuType.num-1).map(i => {
                                                          val lsuexp = (lsu.io.loadAddrMisaligned || lsu.io.storeAddrMisaligned) && io.in(lsuidx).valid && notafter(io.in(lsuidx).bits.InstNo,io.in(i).bits.InstNo,io.in(lsuidx).bits.InstFlag,io.in(i).bits.InstFlag)
                                                          val csrfix =  csr.io.wenFix && io.in(csridx).valid && notafter(io.in(csridx).bits.InstNo,io.in(i).bits.InstNo,io.in(csridx).bits.InstFlag,io.in(i).bits.InstFlag)
                                                          lsuexp || csrfix
                                                        }))

  for(i <- 0 to FuType.num-1){
    io.out(i).bits.decode <> io.in(i).bits
  }
  for(k <- 0 to FuType.num-1){
    io.out(k).bits.decode.ctrl.rfWen := io.in(k).bits.ctrl.rfWen && !IcantWrite(k)
    io.out(k).bits.decode.cf.redirect <> Mux(csr.io.redirect.valid && csridx === k.U, csr.io.redirect, 
                                            Mux(alu.valid && aluidx === k.U,alu.io.redirect,
                                                Mux(alu1.valid && alu1idx === k.U,alu1.io.redirect,empty_RedirectIO)))
  }
  for(i <- 0 to FuType.num-1){
    io.out(i).valid := MuxLookup(i.U, io.in(i).valid,Array(FuType.alu -> io.in(aluidx).valid,
                                                           FuType.lsu -> lsu.io.out.valid,
                                                           FuType.mdu -> mdu.io.out.valid,
                                                           FuType.csr -> io.in(csridx).valid,
                                                           FuType.mou -> false.B,
                                                           FuType.alu1-> io.in(alu1idx).valid))
  }
  for(k <- 0 to FuType.num-1){
    io.out(k).bits.commits:= DontCare
    io.out(k).bits.commits(FuType.alu) := aluOut
    io.out(k).bits.commits(FuType.lsu) := lsuOut
    io.out(k).bits.commits(FuType.csr) := csrOut
    io.out(k).bits.commits(FuType.mdu) := mduOut
    io.out(k).bits.commits(FuType.alu1):= alu1Out
  }
  for(i <- 0 to FuType.num-1){
    io.in(i).ready := !io.in(i).valid || io.out(i).fire()
  }
  io.in(alu1idx).ready := false.B
  for(i <- 0 to FuType.num-1){
    io.forward(i).valid := io.in(i).valid & io.out(i).valid
    io.forward(i).wb.rfWen := io.in(i).bits.ctrl.rfWen && !IcantWrite(i)
    io.forward(i).wb.rfDest := io.in(i).bits.ctrl.rfDest
    io.forward(i).wb.rfData := MuxLookup(fuType(i),aluOut,Array(FuType.alu->aluOut,FuType.alu1->alu1Out,FuType.lsu->lsuOut,FuType.csr->csrOut,FuType.mdu->mduOut))
    io.forward(i).fuType := io.in(i).bits.ctrl.fuType
    io.forward(i).InstNo := io.in(i).bits.InstNo
  }

  when(csr.io.cfIn.exceptionVec.reduce(_||_)){
    io.out(lsuidx).bits.decode.cf.redirect := io.out(csridx).bits.decode.cf.redirect
  }

  if (!p.FPGAPlatform) {
    val cycleCnt = WireInit(0.U(64.W))
    val instrCnt = WireInit(0.U(64.W))
    val nutcoretrap = VecInit((0 to FuType.num-1).map(i => io.in(i).bits.ctrl.isNutCoreTrap && io.in(i).valid)).reduce(_||_)

    val tarpNo = PriorityMux(io.in.map(i => i.bits.ctrl.isNutCoreTrap).zipWithIndex.map{case(a,b)=>(a,b.U)})

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nutcoretrap, "nutcoretrap")

    val difftest = Module(new DifftestTrapEvent)
    difftest.io.clock    := clock
    difftest.io.coreid   := 0.U // TODO: nutshell does not support coreid auto config
    difftest.io.valid    := nutcoretrap
    difftest.io.code     := io.in(tarpNo).bits.data.src1
    difftest.io.pc       := io.in(tarpNo).bits.cf.pc
    difftest.io.cycleCnt := cycleCnt
    difftest.io.instrCnt := instrCnt
  }
  //Debug
  {
      for(i <- 0 to FuType.num-1){
        Debug("[SIMD_EXU] issue %x valid %x outvalid %x pc %x futype %x instrno %x outdata %x \n", i.U,io.in(i).valid, io.out(i).valid,io.in(i).bits.cf.pc, io.in(i).bits.ctrl.fuType, io.in(i).bits.InstNo, io.out(i).bits.commits(i))
      }
      for(i<- 0 to FuType.num-1){
      Debug("[SIMD_EXU] [Issue: %x ]TakeBranch %x BranchTo %x \n", i.U, io.out(i).bits.decode.cf.redirect.valid, io.out(i).bits.decode.cf.redirect.target)
      }
  }
}

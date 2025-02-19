package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import top.Settings
import difftest._

class new_SIMD_EXU(implicit val p: NutCoreConfig) extends NutCoreModule with HasInstrType {
  val io = IO(new Bundle {
    val in = Vec(FuType.num,Flipped(Decoupled(new DecodeIO)))
    val out = Vec(FuType.num,Decoupled(new SIMD_CommitIO))
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

  //basic connect
  val empty_RedirectIO = Wire(new RedirectIO)
  empty_RedirectIO.target := 0.U
  empty_RedirectIO.rtype := 0.U
  empty_RedirectIO.valid := false.B

  for(i <- 0 to FuType.num-1){
    io.out(i).bits.decode <> io.in(i).bits
    io.out(i).bits.decode.ctrl.rfWen := io.in(i).bits.ctrl.rfWen
    io.out(i).bits.decode.cf.redirect <> empty_RedirectIO
    io.out(i).bits.commits:= DontCare
    io.out(i).bits.vector_commits:= DontCare
    io.in(i).ready := !io.in(i).valid || io.out(i).fire()
  }

  //ALU
  val aluidx = FuType.alu
  
  val alu = Module(new ALU(hasBru = true,NO1 = false))
  val aluOut = alu.access(valid = io.in(aluidx).valid, src1 = src1(aluidx), src2 = src2(aluidx), func = fuOpType(aluidx))
  alu.io.cfIn := io.in(aluidx).bits.cf
  alu.io.offset := io.in(aluidx).bits.data.imm
  alu.io.out.ready := io.out(aluidx).ready
  Debug("aluidx %x \n",aluidx)


  //ALU1
  val alu1idx = FuType.alu1
  val alu1 = Module(new ALU(hasBru = true,NO1 = if(Polaris_Independent_Bru == 1){false}else{true}))
  val alu1Out = alu1.access(valid = io.in(alu1idx).valid, src1 = src1(alu1idx), src2 = src2(alu1idx), func = fuOpType(alu1idx))
  alu1.io.cfIn := io.in(alu1idx).bits.cf
  alu1.io.offset := io.in(alu1idx).bits.data.imm
  alu1.io.out.ready := io.out(alu1idx).ready
  
  //SIMDU
  if(Polaris_SIMDU_WAY_NUM == 2){
    val simduidx = FuType.simdu
    val simdu1idx = FuType.simdu1
    val simdu = Module(new SIMDU_2way)
    
    val (simduOut,simdu1Out) = simdu.access(valid0 = io.in(simduidx).valid, src01 = src1(simduidx), src02 = src2(simduidx), func0 = fuOpType(simduidx),valid1 = io.in(simdu1idx).valid, src11 = src1(simdu1idx), src12 = src2(simdu1idx), func1 = fuOpType(simdu1idx))
    simdu.io.DecodeIn(0) := io.in(simduidx).bits
    simdu.io.DecodeIn(1) := io.in(simdu1idx).bits
    simdu.io.out(0).ready := io.out(simduidx).ready
    simdu.io.out(1).ready := io.out(simdu1idx).ready
    simdu.io.flush := io.flush
    val simdu_firststage_fire = Wire(Bool())
    simdu_firststage_fire := simdu.io.FirstStageFire(0)
    BoringUtils.addSource(simdu_firststage_fire, "simdu_fs_fire")
    val simdu1_firststage_fire = Wire(Bool())
    simdu1_firststage_fire := simdu.io.FirstStageFire(1)
    BoringUtils.addSource(simdu1_firststage_fire, "simdu1_fs_fire")
    
    io.out(simduidx).bits.decode <> simdu.io.DecodeOut(0)
    io.out(simdu1idx).bits.decode <> simdu.io.DecodeOut(1)
    io.out(simduidx).bits.decode.ctrl.rfWen := simdu.io.DecodeOut(0).ctrl.rfWen 
    io.out(simdu1idx).bits.decode.ctrl.rfWen:= simdu.io.DecodeOut(1).ctrl.rfWen
    io.out(FuType.simdu).valid := simdu.io.out(0).valid
    io.out(FuType.simdu1).valid := simdu.io.out(1).valid
    io.out(FuType.simdu).bits.commits := simduOut
    io.out(FuType.simdu1).bits.commits := simdu1Out
    io.in(simduidx).ready  := simdu.io.in(0).ready
    io.in(simdu1idx).ready := simdu.io.in(1).ready
  }else if(Polaris_SIMDU_WAY_NUM == 1){
    val simduidx = FuType.simdu
    val simdu = Module(new SIMDU)
    val simduOut = simdu.access(valid = io.in(simduidx).valid, src1 = src1(simduidx), src2 = src2(simduidx), func = fuOpType(simduidx))
    simdu.io.DecodeIn := io.in(simduidx).bits
    simdu.io.out.ready := io.out(simduidx).ready
    simdu.io.flush := io.flush
    val simdu_firststage_fire = Wire(Bool())
    simdu_firststage_fire := simdu.io.FirstStageFire
    BoringUtils.addSource(simdu_firststage_fire, "simdu_fs_fire")

    io.out(simduidx).bits.decode <> simdu.io.DecodeOut
    io.out(simduidx).bits.decode.ctrl.rfWen := simdu.io.DecodeOut.ctrl.rfWen
    io.out(FuType.simdu).valid := simdu.io.out.valid
    io.out(FuType.simdu).bits.commits := simduOut
    io.in(simduidx).ready := simdu.io.in.ready
  }

  //SNNU
  if(Polaris_SNN_WAY_NUM == 2){
    val snnuidx = FuType.snnu
    val snnu1idx = FuType.snnu1
    val snnu = Module(new SNNU_2WAY)
    val (snnuOut,snnu1Out) = snnu.access(valid0 = io.in(snnuidx).valid, src01 = src1(snnuidx), src02 = src2(snnuidx), func0 = fuOpType(snnuidx),valid1 = io.in(snnu1idx).valid, src11 = src1(snnu1idx), src12 = src2(snnu1idx), func1 = fuOpType(snnu1idx))
    snnu.io.dcIn(0) := io.in(snnuidx).bits
    snnu.io.dcIn(1) := io.in(snnu1idx).bits
    snnu.io.out(0).ready := io.out(snnuidx).ready
    snnu.io.out(1).ready := io.out(snnu1idx).ready
    snnu.io.flush := io.flush
    val snnu_firststage_fire = Wire(Bool())
    snnu_firststage_fire := snnu.io.FirstStageFire(0)
    BoringUtils.addSource(snnu_firststage_fire, "snnu_fs_fire")
    val snnu1_firststage_fire = Wire(Bool())
    snnu1_firststage_fire := snnu.io.FirstStageFire(1)
    BoringUtils.addSource(snnu1_firststage_fire, "snnu1_fs_fire")
    
    io.out(snnuidx).bits.decode <> snnu.io.dcOut(0)
    io.out(snnu1idx).bits.decode <> snnu.io.dcOut(1)
    io.out(snnuidx).bits.decode.ctrl.rfWen := snnu.io.dcOut(0).ctrl.rfWen 
    io.out(snnu1idx).bits.decode.ctrl.rfWen:= snnu.io.dcOut(1).ctrl.rfWen
    io.out(FuType.snnu).valid := snnu.io.out(0).valid
    io.out(FuType.snnu1).valid := snnu.io.out(1).valid
    io.out(FuType.snnu).bits.commits := snnuOut
    io.out(FuType.snnu1).bits.commits := snnu1Out
    io.in(snnuidx).ready  := snnu.io.in(0).ready
    io.in(snnu1idx).ready := snnu.io.in(1).ready
    Debug("snnuidx %x \n",snnuidx)
    Debug("snnu1idx %x \n",snnu1idx)
  }else if(Polaris_SNN_WAY_NUM == 1){
    val snnuidx = FuType.snnu
    val snnu = Module(new SNNU)
    val snnuOut = snnu.access(valid = io.in(snnuidx).valid, src1 = src1(snnuidx), src2 = src2(snnuidx), func = fuOpType(snnuidx))
    snnu.io.out.ready := io.out(snnuidx).ready
    snnu.io.dcIn := io.in(snnuidx).bits
    snnu.io.flush := io.flush
    val snnu_firststage_fire = Wire(Bool())
    snnu_firststage_fire := snnu.io.FirstStageFire(0)
    BoringUtils.addSource(snnu_firststage_fire, "snnu_fs_fire")
    io.out(snnuidx).bits.decode <> snnu.io.dcOut
    io.out(snnuidx).bits.decode.ctrl.rfWen := snnu.io.dcOut.ctrl.rfWen
    io.out(FuType.snnu).valid := snnu.io.out.valid
    io.out(FuType.snnu).bits.commits := snnuOut
    io.in(snnuidx).ready := snnu.io.in.ready
    Debug("snnuidx %x \n",snnuidx)
  }

  //MDU
  val mduidx = FuType.mdu
  val mdu = Module(new MDU)
  val mduOut = mdu.access(valid = io.in(mduidx).valid, src1 = src1(mduidx), src2 = src2(mduidx), func = fuOpType(mduidx))
  mdu.io.out.ready := io.out(mduidx).ready
  mdu.io.flush := io.flush

  //FPU
  val csr_rm = Wire(UInt(3.W))
  def rmInvalid(rm: UInt): Bool = rm === "b101".U || rm === "b110".U
  def fflags_mask(valid: Bool) = Fill(5, valid.asUInt)
  def getRM(func3: UInt, csr_rm: UInt) = Mux(func3 === "b111".U, csr_rm, func3)
  //fma
  val fmaidx = FuType.fma
  val fma = Module(new FMA)
  val fmaOut = fma.access(valid = io.in(fmaidx).valid, src1 = src1(fmaidx), src2 = src2(fmaidx), src3 = io.in(fmaidx).bits.data.src3, func = fuOpType(fmaidx))
  val fma_fflags = fmaOut.fflags & fflags_mask(fma.io.out.valid) | Cat(rmInvalid(fma.io.in.bits.rm), 0.U(4.W))
  fma.io.out.ready := io.out(fmaidx).ready
  fma.io.flush := io.flush
  fma.io.in.bits.rm := getRM(io.in(fmaidx).bits.ctrl.funct3, csr_rm)
  //fdivsqrt
  val fdivsqrtidx = FuType.fdivsqrt
  val fdivsqrt = Module(new FDivSqrt)
  val fdivsqrtOut = fdivsqrt.access(valid = io.in(fdivsqrtidx).valid, src1 = src1(fdivsqrtidx), src2 = src2(fdivsqrtidx), func = fuOpType(fdivsqrtidx))
  val fdivsqrt_fflags = fdivsqrtOut.fflags & fflags_mask(fdivsqrt.io.out.valid) | Cat(rmInvalid(fdivsqrt.io.in.bits.rm), 0.U(4.W))
  fdivsqrt.io.out.ready := io.out(fdivsqrtidx).ready
  fdivsqrt.io.flush := io.flush
  fdivsqrt.io.in.bits.rm := getRM(io.in(fdivsqrtidx).bits.ctrl.funct3, csr_rm)
  //fconv
  val fconvidx = FuType.fconv
  val fconv = Module(new FCONV)
  val fconvOut = fconv.access(valid = io.in(fconvidx).valid, src1 = src1(fconvidx), src2 = src2(fconvidx), func = fuOpType(fconvidx))
  val fconv_fflags = fconvOut.fflags & fflags_mask(fconv.io.out.valid) | Cat(rmInvalid(fconv.io.in.bits.rm), 0.U(4.W))
  fconv.io.out.ready := io.out(fconvidx).ready
  fconv.io.flush := io.flush
  fconv.io.in.bits.rm := getRM(io.in(fconvidx).bits.ctrl.funct3, csr_rm)
  //fcomp
  val fcompidx = FuType.fcomp
  val fcomp = Module(new FCOMP)
  val fcompOut = fcomp.access(valid = io.in(fcompidx).valid, src1 = src1(fcompidx), src2 = src2(fcompidx), func = fuOpType(fcompidx))
  val fcomp_fflags = fcompOut.fflags & fflags_mask(fcomp.io.out.valid)
  fcomp.io.out.ready := io.out(fcompidx).ready
  fcomp.io.flush := io.flush
  fcomp.io.in.bits.rm := getRM(io.in(fcompidx).bits.ctrl.funct3, csr_rm)
  val fpuOutValid = Seq(fma.io.out.valid, fdivsqrt.io.out.valid, fconv.io.out.valid, fcomp.io.out.valid).reduce(_||_)
  val fpu_fflags = Seq(fma_fflags, fdivsqrt_fflags, fconv_fflags, fcomp_fflags).reduce(_|_)
//  for(i <- 0 to FuType.num-1){
//    io.out(i).bits.decode.ctrl.fReg.wen := false.B
//  }
//  io.out(fmaidx).bits.decode.ctrl.fReg.wen := true.B
//  io.out(fdivsqrtidx).bits.decode.ctrl.fReg.wen := true.B
//  io.out(fconvidx).bits.decode.ctrl.fReg.wen := fconv.io.decode.ctrl.fReg.wen

  //bru
  val bruidx = FuType.bruint
  if(Polaris_Independent_Bru == 1){
    val bru = Module(new ALU(hasBru = true,NO1 = true))
    val bruOut = bru.access(valid = io.in(bruidx).valid, src1 = src1(bruidx), src2 = src2(bruidx), func = fuOpType(bruidx))
    bru.io.cfIn := io.in(bruidx).bits.cf
    bru.io.offset := io.in(bruidx).bits.data.imm
    bru.io.out.ready := io.out(bruidx).ready
    io.out(bruidx).bits.decode.cf.redirect <> bru.io.redirect
    io.out(FuType.bru).valid := io.in(bruidx).valid 
    io.out(FuType.bru).bits.commits := bruOut
  }

  //LSU
  val lsuidx = FuType.lsu
  val BeforeLSUhasRedirect = notafter(io.in(bruidx).bits.InstNo,io.in(lsuidx).bits.InstNo,io.in(bruidx).bits.InstFlag,io.in(lsuidx).bits.InstFlag)&&io.out(bruidx).bits.decode.cf.redirect.valid
  val lsu = Module(new pipeline_lsu_atom)
  lsu.io.DecodeIn := io.in(lsuidx).bits
  val lsuOut = lsu.access(valid = io.in(lsuidx).valid && !BeforeLSUhasRedirect, src1 = src1(lsuidx), src2 = Mux(io.in(lsuidx).bits.ctrl.rfVector,src2(lsuidx),io.in(lsuidx).bits.data.imm), func = fuOpType(lsuidx))
  val lsuVectorOut = lsu.v_access(io.in(lsuidx).bits.data.src_vector)
  lsu.io.wdata := Mux(lsu.io.DecodeIn.ctrl.fReg.src2Ren && lsu.io.DecodeIn.ctrl.fuOpType === LSUOpType.sw,float.unbox(src2(lsuidx), float.fp32), src2(lsuidx))
  for(i <- 0 to FuType.num-1){
    io.out(i).bits.isMMIO := i.U === lsuidx && (lsu.io.isMMIO && io.out(i).valid)
  }
  Debug("lsu_is_mmio %x \n",io.out(lsuidx).bits.isMMIO)
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := io.out(lsuidx).ready
  lsu.io.flush := io.flush
  io.out(lsuidx).bits.decode <> lsu.io.DecodeOut
  io.out(lsuidx).bits.decode.ctrl.rfWen := lsu.io.DecodeOut.ctrl.rfWen
  val toFReg = VecInit(fmaidx, fdivsqrtidx, fconvidx, lsuidx).map(i => io.out(i).bits.decode.ctrl.fReg.wen && io.out(i).valid).reduce(_||_)

  //CSRU
  val csridx = FuType.csr
  val csr_bits = Mux(io.in(csridx).valid, io.in(csridx).bits,lsu.io.DecodeOut)
  val csr = Module(new new_SIMD_CSR)
  val csrOut = csr.access(valid = io.in(csridx).valid, src1 = src1(csridx), src2 = src2(csridx), func = fuOpType(csridx),isMou = csr_bits.ctrl.isMou)
  Debug("isMou %x lsumou %x csrmou %x\n",csr_bits.ctrl.isMou,lsu.io.DecodeOut.ctrl.isMou,io.in(csridx).bits.ctrl.isMou)
  csr.io.cfIn := Mux(io.in(csridx).valid, io.in(csridx).bits.cf,lsu.io.DecodeOut.cf)
  csr.io.ctrlIn := Mux(io.in(csridx).valid, io.in(csridx).bits.ctrl,lsu.io.DecodeOut.ctrl)
  csr.io.cfIn.exceptionVec(loadAddrMisaligned) := lsu.io.loadAddrMisaligned 
  csr.io.cfIn.exceptionVec(storeAddrMisaligned) := lsu.io.storeAddrMisaligned
  val hasLoadPF = lsu.io.loadPF//RegInit(false.B)
  val hasStorePF= lsu.io.storePF//RegInit(false.B)
  csr.io.fpu.fflags.valid := fpuOutValid
  csr.io.fpu.fflags.bits := fpu_fflags
  csr.io.fpu.dirty_fs := toFReg
  csr_rm := csr.io.fpu.frm
  /*
  when(io.memMMU.dmem.loadPF){
    hasLoadPF := true.B
  } 
  when(io.memMMU.dmem.storePF){
    hasStorePF := true.B
  }
  when(io.flush){
    hasLoadPF := false.B
    hasStorePF := false.B
  }
  */
  BoringUtils.addSource(io.memMMU.dmem.loadPF,"loadPF")
  BoringUtils.addSource(io.memMMU.dmem.storePF,"storePF")
  csr.io.cfIn.exceptionVec(loadPageFault) := hasLoadPF 
  csr.io.cfIn.exceptionVec(storePageFault) := hasStorePF
  val lsuexp = (lsu.io.loadAddrMisaligned || lsu.io.storeAddrMisaligned || hasStorePF || hasLoadPF)// && lsu.io.in.valid
  val csrfix =  csr.io.wenFix && io.in(csridx).valid
  csr.io.instrValid := (io.in(csridx).valid || lsuexp) && io.out(csridx).fire()
  //csr.io.isBackendException := false.B
  for(i <- 0 to FuType.num-1){
    io.out(i).bits.intrNO := DontCare
  }
  //csr.io.isBackendException := false.B
  csr.io.out.ready := true.B
  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  when(lsuexp){
    io.out(csridx).bits.decode.InstNo := lsu.io.DecodeOut.InstNo
    io.out(csridx).bits.decode.InstFlag := lsu.io.DecodeOut.InstFlag
    io.out(csridx).bits.decode := csr_bits
  }
  when(lsuexp || csrfix){
    io.out(csridx).bits.decode.ctrl.rfWen := false.B
  }

  //PerU need no parameter,use boringutils
  val PerfU = Module(new PerfU)

  io.out(csridx).bits.decode.cf.redirect <> csr.io.redirect
  io.out(aluidx).bits.decode.cf.redirect <> alu.io.redirect
  io.out(alu1idx).bits.decode.cf.redirect <> alu1.io.redirect
  
  io.out(FuType.alu).valid := io.in(aluidx).valid
  io.out(FuType.alu1).valid := io.in(alu1idx).valid
  io.out(FuType.lsu).valid := lsu.io.out.valid && !lsuexp
  io.out(FuType.mdu).valid := mdu.io.out.valid
  io.out(FuType.csr).valid := io.in(csridx).valid || lsuexp
  io.out(FuType.fma).valid := fma.io.out.valid
  io.out(FuType.fdivsqrt).valid := fdivsqrt.io.out.valid
  io.out(FuType.fconv).valid := fconv.io.out.valid
  io.out(FuType.fcomp).valid := fcomp.io.out.valid

  io.out(FuType.alu).bits.commits := aluOut
  io.out(FuType.lsu).bits.commits := Mux(io.out(lsuidx).bits.decode.ctrl.fReg.wen && lsu.io.DecodeOut.ctrl.fuOpType === LSUOpType.lw,float.box(lsuOut(31, 0), float.fp64), lsuOut)
  io.out(FuType.csr).bits.commits := csrOut
  io.out(FuType.mdu).bits.commits := mduOut
  io.out(FuType.alu1).bits.commits:= alu1Out
  io.out(FuType.fma).bits.commits := fmaOut.result
  io.out(FuType.fdivsqrt).bits.commits := fdivsqrtOut.result
  io.out(FuType.fconv).bits.commits := fconvOut.result
  io.out(FuType.fcomp).bits.commits := fcompOut.result

  for(i <- 0 to FuType.num-1){io.out(i).bits.vector_commits := 0.U}
  io.out(FuType.lsu).bits.vector_commits := lsuVectorOut

  io.in(lsuidx).ready := lsu.io.in.ready && !BeforeLSUhasRedirect

  for(i <- 0 to FuType.num-1){
    io.forward(i).valid := io.out(i).valid
    io.forward(i).wb.rfWen := io.out(i).bits.decode.ctrl.rfWen //&& !IcantWrite(i)
    io.forward(i).wb.rfDest := io.out(i).bits.decode.ctrl.rfDest
    io.forward(i).wb.rfData := io.out(i).bits.commits
    io.forward(i).wb.toFReg := io.out(i).bits.decode.ctrl.fReg.wen
    io.forward(i).fuType := io.out(i).bits.decode.ctrl.fuType
    io.forward(i).InstNo := io.out(i).bits.decode.InstNo
  }

  if (!p.FPGAPlatform) {
    val cycleCnt = WireInit(0.U(64.W))
    val instrCnt = WireInit(0.U(64.W))
    val nutcoretrap = VecInit((0 to FuType.num-1).map(i => io.in(i).bits.ctrl.isNutCoreTrap && io.in(i).valid)).reduce(_||_)

    val tarpNo = PriorityMux(io.in.map(i => i.bits.ctrl.isNutCoreTrap).zipWithIndex.map{case(a,b)=>(a,b.U)})

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nutcoretrap, "nutcoretrap")
    val csrops = io.in(csridx).valid === true.B
    BoringUtils.addSource(csrops, "csrops")
    BoringUtils.addSource(io.in(csridx).valid && io.in.map(i=>i.valid.asUInt).reduce(_+&_) =/= 1.U,"csrnotalone")
    val LsuWorking = io.in(lsuidx).valid === true.B
    BoringUtils.addSource(LsuWorking, "perfCntCondLsuWorking")
    //BoringUtils.addSource(io.in(mouidx).valid && io.in.map(i=>i.valid.asUInt).reduce(_+&_) =/= 1.U,"mounotalone")

    val difftest = Module(new DifftestTrapEvent)
    difftest.io := DontCare
    difftest.io.clock    := clock
    difftest.io.coreid   := 0.U // TODO: nutshell does not support coreid auto config
    difftest.io.valid    := nutcoretrap
    difftest.io.code     := io.in(tarpNo).bits.data.src1
    difftest.io.pc       := io.in(tarpNo).bits.cf.pc
    difftest.io.cycleCnt := cycleCnt
    difftest.io.instrCnt := instrCnt
  }else{
    when(io.in(csridx).bits.ctrl.isNutCoreTrap){
      io.out(csridx).valid := false.B
    }
  }
  //Debug
  {
      for(i <- 0 to FuType.num-1){
        Debug("[SIMD_EXU] issue %x valid %x outvalid %x pc %x futype %x instrno %x outdata %x \n", i.U,io.in(i).valid, io.out(i).valid,io.in(i).bits.cf.pc, io.in(i).bits.ctrl.fuType, io.out(i).bits.decode.InstNo, io.out(i).bits.commits)
      }
      for(i<- 0 to FuType.num-1){
      Debug("[SIMD_EXU] [Issue: %x ]BeforeLSUhasRedirect %x TakeBranch %x BranchTo %x \n", i.U,BeforeLSUhasRedirect, io.out(i).bits.decode.cf.redirect.valid, io.out(i).bits.decode.cf.redirect.target)
      }
  }
  
}
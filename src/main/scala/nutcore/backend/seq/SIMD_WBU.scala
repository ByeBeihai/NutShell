package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
import difftest._

class new_SIMD_WBU(implicit val p: NutCoreConfig) extends NutCoreModule with HasRegFileParameter with HasLSUConst{
  val io = IO(new Bundle {
    val in = Vec(Commit_num,Flipped(Decoupled(new SIMD_CommitIO)))
    val wb = new new_SIMD_WriteBackIO
    val redirect = new RedirectIO
  })
  def bool2int(b:Boolean) = if (b) 1 else 0

  val redirct_index = PriorityMux(VecInit((0 to Commit_num-1).map(i => io.in(i).bits.decode.cf.redirect.valid && io.in(i).valid)).zipWithIndex.map{case(a,b)=>(a,b.U)})
  io.redirect := io.in(redirct_index).bits.decode.cf.redirect
  io.redirect.valid := VecInit((0 to Commit_num-1).map(i => io.in(i).bits.decode.cf.redirect.valid && io.in(i).valid)).reduce(_||_)

  val FronthasRedirect = VecInit((0 to Commit_num-1).map(i => i.U > redirct_index))

  //forward
  for(i <- 0 to Commit_num-1){
    io.wb.rfWen(i) := io.in(i).bits.decode.ctrl.rfWen && io.in(i).valid
    io.wb.toFReg(i) := io.in(i).bits.decode.ctrl.fReg.wen
    io.wb.rfDest(i) := io.in(i).bits.decode.ctrl.rfDest
    io.wb.WriteData(i) := io.in(i).bits.commits
    io.wb.valid(i) :=io.in(i).valid
    io.wb.InstNo(i) := io.in(i).bits.decode.InstNo
  }
  io.wb.ReadDataVec := DontCare
  io.wb.WriteDestVec:= DontCare
  io.wb.WriteDataVec:= DontCare
  io.wb.VecInstNo   := DontCare
  val WriteDataVec = WireInit(0.U.asTypeOf(io.wb.ReadDataVec))
  val WriteDestVec = WireInit(0.U.asTypeOf(io.wb.rfSrcVec))
  val vec_num = WireInit(0.U(log2Up(NRReg).W))
  val vec_no = WireInit(0.U.asTypeOf(io.wb.VecInstNo))
  if(Polaris_Vector_LDST){
    for(i <- 0 to Commit_num-1){
        when(io.in(i).valid && io.in(i).bits.decode.ctrl.rfVector && io.in(i).bits.decode.ctrl.rfWen){
          vec_no       := io.in(i).bits.decode.InstNo
          vec_num      := 1.U(log2Up(NRReg).W) << io.in(i).bits.decode.ctrl.fuOpType(3,2)
          WriteDataVec := io.in(i).bits.vector_commits.asTypeOf(io.wb.ReadDataVec)
          WriteDestVec := (0 to vector_rdata_width/XLEN-1).map(j => Mux(j.U < vec_num,io.wb.rfDest(i)+j.U(log2Up(NRReg).W),0.U))
        }
    }
    io.wb.WriteDestVec := WriteDestVec
    io.wb.WriteDataVec := WriteDataVec
    io.wb.VecInstNo    := vec_no
  }

  //register (banks)
  val rf = new RegFile
  val fpRf = new RegFile
  if(Polaris_RegBanks){
    val rf_banks = for (i <- 0 until Issue_Num) yield {
      val rf_unit = for (j <- 0 until 2 + bool2int(Polaris_SIMDU_WAY_NUM != 0)) yield {
        val rf = new RegFile
        rf
      } 
      rf_unit
    }
    for(i<-0 to Commit_num-1){
      when (io.wb.rfWen(i) && !FronthasRedirect(i)) {(0 to Issue_Num-1).map(m => ((0 to (2 + bool2int(Polaris_SIMDU_WAY_NUM != 0) - 1)).map(n => rf_banks(m)(n).write(io.wb.rfDest(i), io.wb.WriteData(i)))))}
    }
    for(i <- 0 to Issue_Num-1){
      io.wb.ReadData1(i):=rf_banks(i)(0).read(io.wb.rfSrc1(i))
      io.wb.ReadData2(i):=rf_banks(i)(1).read(io.wb.rfSrc2(i))
      io.wb.ReadData3(i):=DontCare
      if(Polaris_SIMDU_WAY_NUM!=0){
        io.wb.ReadData3(i):=rf_banks(i)(2).read(io.wb.rfSrc3(i))
      }
    }
    if(Polaris_Vector_LDST){
      for(i <- 0 to vector_rdata_width/XLEN-1){
        io.wb.ReadDataVec(i):=rf_banks(0)(0).read(io.wb.rfSrcVec(i))
        (0 to Issue_Num-1).map(m => ((0 to (2 + bool2int(Polaris_SIMDU_WAY_NUM != 0) - 1)).map(n => rf_banks(m)(n).write(WriteDestVec(i), WriteDataVec(i)))))
      }
    }
    if (!p.FPGAPlatform) {
      when(reset.asBool){(0 to Issue_Num-1).map(i => ((0 to (2 + bool2int(Polaris_SIMDU_WAY_NUM != 0) - 1)).map(j => (0 to NRReg-1).map(k => rf_banks(i)(j).write(k.U, 0.U)))))}
      val difftest = Module(new DifftestArchIntRegState)
      difftest.io.clock  := clock
      difftest.io.coreid := 0.U 
      difftest.io.gpr    := VecInit((0 to NRReg-1).map(i => rf_banks(0)(0).read(i.U)))
    }
  }else{
    
    for(i<-0 to Commit_num-1){
      when (io.wb.rfWen(i) && !FronthasRedirect(i)) {
      when(io.wb.toFReg(i)) {
        fpRf.write(io.wb.rfDest(i), io.wb.WriteData(i))
        Debug("!!!WriteFloatReg!!! rfDest %x WriteData %x\n", io.wb.rfDest(i), io.wb.WriteData(i))
      }.otherwise {
        rf.write(io.wb.rfDest(i), io.wb.WriteData(i))
        Debug("!!!WriteIntReg!!! rfDest %x WriteData %x\n", io.wb.rfDest(i), io.wb.WriteData(i))
      }
      }
      when(reset.asBool){
        rf.write(io.wb.rfDest(i), 0.U)
        fpRf.write(io.wb.rfDest(i), 0.U)
      }
    }
    for(i <- 0 to Issue_Num-1){
      io.wb.ReadData1(i):= Mux(io.wb.src1fpRen(i), fpRf.read(io.wb.rfSrc1(i), fp = true), rf.read(io.wb.rfSrc1(i)))
      io.wb.ReadData2(i):= Mux(io.wb.src2fpRen(i), fpRf.read(io.wb.rfSrc2(i), fp = true), rf.read(io.wb.rfSrc2(i)))
      io.wb.ReadData3(i):= Mux(io.wb.src3fpRen(i), fpRf.read(io.wb.rfSrc3(i), fp = true), rf.read(io.wb.rfSrc3(i)))
    }
    if(Polaris_Vector_LDST){
      for(i <- 0 to vector_rdata_width/XLEN-1){
        io.wb.ReadDataVec(i):=rf.read(io.wb.rfSrcVec(i))
        rf.write(WriteDestVec(i), WriteDataVec(i))
      }
    }    
    if (!p.FPGAPlatform) {
      when(reset.asBool){(0 to NRReg-1).map(k => rf.write(k.U, 0.U))}
      val difftest = Module(new DifftestArchIntRegState)
      difftest.io.clock  := clock
      difftest.io.coreid := 0.U 
      difftest.io.gpr    := VecInit((0 to NRReg-1).map(i => rf.read(i.U)))
    }
  }

  
  for(i <- 0 to Commit_num-1){
    io.in(i).ready := true.B
  }

  val freq = WireInit(0.U(64.W))
  BoringUtils.addSink(freq,"freq");
  Debug("[SIMD_WBU] freq:%d\n",freq)

  //P-EXT
  if(Polaris_SIMDU_WAY_NUM!=0){
    val bool_wire = WireInit(false.B)
    for(i <- 0 to Commit_num-1){
      when(io.in(i).valid && io.in(i).bits.decode.pext.OV && !FronthasRedirect(i)){
        bool_wire := true.B
      }
    }
    BoringUtils.addSource(bool_wire,"OVWEN")
  }
  
  if(!p.FPGAPlatform){
  val runahead_redirect = Module(new DifftestRunaheadRedirectEvent)
  runahead_redirect.io.clock := clock
  runahead_redirect.io.coreid := 0.U
  runahead_redirect.io.valid := io.redirect.valid
  runahead_redirect.io.pc := io.in(redirct_index).bits.decode.cf.pc // for debug only
  runahead_redirect.io.target_pc := io.in(redirct_index).bits.decode.cf.redirect.target // for debug only
  runahead_redirect.io.checkpoint_id := io.in(redirct_index).bits.decode.cf.runahead_checkpoint_id // make sure it is right
  // when(runahead_redirect.io.valid) {
  //   printf("DUT pc %x redirect to %x cpid %x\n", runahead_redirect.io.pc, runahead_redirect.io.target_pc, runahead_redirect.io.checkpoint_id)
  // }
  }

  val commit_num = (0 to Commit_num-1).map(i => (io.in(i).valid && !FronthasRedirect(i)).asUInt).reduce(_+&_)
  for(i <- 0 to 0){
  BoringUtils.addSource(io.in(i).valid, "perfCntCondMinstret")
  BoringUtils.addSource(commit_num=/=0.U, "perfCntCondMultiCommit")
  BoringUtils.addSource(commit_num===2.U, "perfCntCondMultiCommit2")
  BoringUtils.addSource(commit_num===3.U, "perfCntCondMultiCommit3")
  BoringUtils.addSource(commit_num===4.U, "perfCntCondMultiCommit4")
  BoringUtils.addSource(commit_num===5.U, "perfCntCondMultiCommit5")
  BoringUtils.addSource(commit_num===6.U, "perfCntCondMultiCommit6")
  }
  io.wb.rfVector :=DontCare

  for(i <- 0 to Commit_num-1){
    Debug("[SIMD_WBU] issue %x valid %x pc %x wen %x wdata %x wdest %x futype %x instno %x isMMIO %x OV %x redirectvalid %x redirecttarget %x \n",i.U,io.in(i).valid,io.in(i).bits.decode.cf.pc,io.wb.rfWen(i),io.wb.WriteData(i),io.wb.rfDest(i),io.in(i).bits.decode.ctrl.fuType,io.in(i).bits.decode.InstNo,io.in(i).bits.isMMIO,io.in(i).bits.decode.pext.OV,io.in(i).bits.decode.cf.redirect.valid,io.in(i).bits.decode.cf.redirect.target)
  }
  Debug("[SIMD_WBU] redirctindex %x redirctvalid %x redircttarget %x \n",redirct_index,io.redirect.valid,io.redirect.target)
  if (!p.FPGAPlatform) {
    for(i <- 0 to Commit_num-1){
    val difftest_commit = Module(new DifftestInstrCommit)
    difftest_commit.io.clock    := clock
    difftest_commit.io.coreid   := 0.U
    difftest_commit.io.index    := i.U

    difftest_commit.io.valid    := RegNext(io.in(i).valid && !FronthasRedirect(i))
    difftest_commit.io.pc       := RegNext(SignExt(io.in(i).bits.decode.cf.pc, AddrBits))
    difftest_commit.io.instr    := RegNext(io.in(i).bits.decode.cf.instr)
    difftest_commit.io.skip     := RegNext(io.in(i).bits.isMMIO /*|| io.in(i).bits.decode.ctrl.rfVector*/)
    difftest_commit.io.isRVC    := RegNext(io.in(i).bits.decode.cf.instr(1,0)=/="b11".U)
    difftest_commit.io.rfwen    := RegNext(io.wb.rfWen(i) && io.wb.rfDest(i) =/= 0.U && !io.wb.toFReg(i)) // && valid(ringBufferTail)(i) && commited(ringBufferTail)(i)
    difftest_commit.io.fpwen    := RegNext(io.wb.rfWen(i) && io.wb.toFReg(i))
    //difftest_commit.io.wdata    := RegNext(io.wb.WriteData(i))
    difftest_commit.io.wdest    := RegNext(io.wb.rfDest(i))
    difftest_commit.io.wpdest   := RegNext(io.wb.rfDest(i))


    val difftest_int_wb = Module(new DifftestIntWriteback)
    difftest_int_wb.io.clock := clock
    difftest_int_wb.io.coreid := 0.U
    difftest_int_wb.io.valid := RegNext(io.wb.rfWen(i) && io.wb.rfDest(i) =/= 0.U && !io.wb.toFReg(i))
    difftest_int_wb.io.dest := RegNext(io.wb.rfDest(i))
    difftest_int_wb.io.data := RegNext(io.wb.WriteData(i))

    val difftest_fp_wb = Module(new DifftestFpWriteback)
    difftest_fp_wb.io.clock := clock
    difftest_fp_wb.io.coreid := 0.U
    difftest_fp_wb.io.valid := RegNext(io.wb.rfWen(i) && io.wb.toFReg(i))
    difftest_fp_wb.io.dest := RegNext(io.wb.rfDest(i))
    difftest_fp_wb.io.data := RegNext(io.wb.WriteData(i))

    val runahead_commit = Module(new DifftestRunaheadCommitEvent)
    runahead_commit.io.clock := clock
    runahead_commit.io.coreid := 0.U
    runahead_commit.io.index := i.U
    runahead_commit.io.valid := RegNext(io.in(i).valid && io.in(i).bits.decode.cf.isBranch)
    runahead_commit.io.pc    := RegNext(SignExt(io.in(i).bits.decode.cf.pc, AddrBits))
    // when(runahead_commit.io.valid) {
    //   printf("DUT commit branch %x\n", runahead_commit.io.pc)
    // }

    val difftest_intReg = Module(new DifftestArchIntRegState)
    difftest_intReg.io.clock  := clock
    difftest_intReg.io.coreid := 0.U
    difftest_intReg.io.gpr    := VecInit((0 to NRReg-1).map(i => rf.read(i.U)))
    val difftest_fpReg = Module(new DifftestArchFpRegState)
    difftest_fpReg.io.clock  := clock
    difftest_fpReg.io.coreid := 0.U
    difftest_fpReg.io.fpr    := VecInit((0 to NRReg-1).map(i => fpRf.read(i.U, fp = true)))
    }
  } else {
    for(i <- 0 to 0){
    BoringUtils.addSource(io.in(i).valid, "ilaWBUvalid")
    BoringUtils.addSource(io.in(i).bits.decode.cf.pc, "ilaWBUpc")
    BoringUtils.addSource(io.wb.rfWen(i), "ilaWBUrfWen")
    BoringUtils.addSource(io.wb.rfDest(i), "ilaWBUrfDest")
    BoringUtils.addSource(io.wb.WriteData(i), "ilaWBUrfData")
    BoringUtils.addSource(io.in(i).bits.decode.cf.instr,"ilaInstr")
    }
  }
}
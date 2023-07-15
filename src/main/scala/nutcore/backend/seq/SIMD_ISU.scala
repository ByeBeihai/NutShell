package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import difftest._

class new_SIMD_ISU(implicit val p:NutCoreConfig)extends NutCoreModule with HasRegFileParameter with HasLSUConst{
    val io = IO(new Bundle{
        val in = Vec(2,Flipped(Decoupled(new DecodeIO)))
        val out = Vec(Issue_Num,Decoupled(new DecodeIO))
        val wb = Flipped(new new_SIMD_WriteBackIO)
        val forward = Vec(Forward_num,Flipped(new ForwardIO))
        val flush = Input(Bool())
        val num_enterwbu = Input(UInt(log2Up(Queue_num).W))
        val TailPtr = Output(UInt(log2Up(Queue_num).W))
    })
    for(i<-0 to Issue_Num-1){
        io.out(i):=DontCare
    }
    val InstBoard = Module(new InstBoard)
    val q = Module(new InstQueue)

    def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool): Bool = (rfSrc =/= 0.U) && (rfSrc === rfDest) && wen
    def isCsrMouOp(i:Int):Bool = io.in(i).bits.ctrl.fuType === FuType.csr || io.in(i).bits.ctrl.fuType === FuType.mou
    def isLatestData(rfSrc: UInt,InstNo:UInt):Bool = InstBoard.io.RInstNo(rfSrc) === InstNo

    val rfSrc1 = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfSrc1))
    val rfSrc2 = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfSrc2))
    val rfSrc3 = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfSrc2))
    val rfSrcVec=VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfDest))
    val rfDest = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfDest))
    val rfWen  = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfWen ))

    val forwardRfWen = VecInit((0 to Forward_num-1).map(i => io.forward(i).wb.rfWen && io.forward(i).valid))
    val src1DependEX = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Forward_num-1).map(j => isLatestData(rfSrc1(i),io.forward(j).InstNo) && isDepend(rfSrc1(i), io.forward(j).wb.rfDest, forwardRfWen(j))))))
    val src2DependEX = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Forward_num-1).map(j => isLatestData(rfSrc2(i),io.forward(j).InstNo) && isDepend(rfSrc2(i), io.forward(j).wb.rfDest, forwardRfWen(j))))))
    val src1DependWB = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Commit_num-1).map(j => isLatestData(rfSrc1(i),io.wb.InstNo(j)) && isDepend(rfSrc1(i), io.wb.rfDest(j), io.wb.rfWen(j))))))
    val src2DependWB = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Commit_num-1).map(j => isLatestData(rfSrc2(i),io.wb.InstNo(j)) && isDepend(rfSrc2(i), io.wb.rfDest(j), io.wb.rfWen(j))))))

    val src1Ready = VecInit((0 to Issue_Num-1).map(i => !InstBoard.io.valid(rfSrc1(i))||src1DependEX(i).reduce(_||_)||src1DependWB(i).reduce(_||_)))
    val src2Ready = VecInit((0 to Issue_Num-1).map(i => !InstBoard.io.valid(rfSrc2(i))||src2DependEX(i).reduce(_||_)||src2DependWB(i).reduce(_||_)))
    val src3Ready = VecInit((0 to Issue_Num-1).map(i => true.B))
    val srcVecReady = VecInit((0 to Issue_Num-1).map(i => true.B))

    val RAWinIssue = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool())) 
                                                        for(j <- 0 to i-1){
                                                                val ReadAfterWrite = isDepend(rfSrc1(i),rfDest(j),rfWen(j))||isDepend(rfSrc2(i),rfDest(j),rfWen(j))||(if(Polaris_SIMDU_WAY_NUM != 0){isDepend(rfSrc3(i),rfDest(j),rfWen(j))}else{false.B})
                                                                val VecLDSTraw     = if(Polaris_Vector_LDST){io.in(j).bits.ctrl.rfWen && Mux(io.in(i).bits.ctrl.rfVector,Mux(io.in(i).bits.ctrl.rfWen,io.in(j).bits.ctrl.rfVector,true.B),io.in(j).bits.ctrl.rfVector)}else{false.B}
                                                                raw(j) := io.in(j).valid && (ReadAfterWrite || VecLDSTraw)
                                                            }
                                                        for(j <- i to Issue_Num-1){
                                                                raw(j) := false.B 
                                                        }
                                                        raw.reduce(_||_)}))

    val FrontHasCsrMouOp = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool())) 
                                                            for(j <- 0 to i-1){
                                                                raw(j) := io.in(j).valid && (io.in(j).bits.ctrl.fuType === FuType.csr || io.in(j).bits.ctrl.fuType === FuType.mou)
                                                            }
                                                            for(j <- i to Issue_Num-1){
                                                                raw(j) := false.B 
                                                            }
                                                            raw.reduce(_||_)}))
    val FrontisClear = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool())) 
                                                            for(j <- 0 to i-1){
                                                                raw(j) := !io.in(j).valid
                                                            }
                                                            for(j <- i to Issue_Num-1){
                                                                raw(j) := false.B 
                                                            }
                                                            raw.reduce(_||_)}))

    for(i <- 0 to Issue_Num-1){
        if(i == 0){
            io.out(i).valid := io.in(i).valid && src1Ready(i) && src2Ready(i) && srcVecReady(i) && (if(Polaris_SIMDU_WAY_NUM != 0){src3Ready(i)}else{true.B}) && !(isCsrMouOp(i) && q.io.TailPtr =/= q.io.HeadPtr)
        }else{
            io.out(i).valid := io.in(i).valid && src1Ready(i) && src2Ready(i) && srcVecReady(i) && (if(Polaris_SIMDU_WAY_NUM != 0){src3Ready(i)}else{true.B}) && !RAWinIssue(i) && !FrontHasCsrMouOp(i) && !isCsrMouOp(i)
            Debug("[SIMD_ISU] RAWinIssue %x FrontHasCsrMouOp %x isCsrMouOp %x FrontisClear %x \n",RAWinIssue(i),FrontHasCsrMouOp(i),isCsrMouOp(i),FrontisClear(i))
        }
    }

    io.in(0).ready := !io.in(0).valid||io.out(0).fire()
    if(Issue_Num == 1){
        io.in(1).ready := false.B
    }else{
        io.in(1).ready := !io.in(1).valid||io.out(1).fire()
    }

    for(i <- 0 to Issue_Num-1){
        io.out(i).bits.data.src1 := PriorityMux(Seq(
        (io.in(i).bits.ctrl.src1Type === SrcType.pc) -> SignExt(io.in(i).bits.cf.pc, AddrBits),
        src1DependEX(i).reduce(_||_) -> io.forward(PriorityMux(src1DependEX(i).zipWithIndex.map{case(a,b)=>(a,b.U)})).wb.rfData, //io.forward.wb.rfData,
        src1DependWB(i).reduce(_||_) -> io.wb.WriteData(PriorityMux(src1DependWB(i).zipWithIndex.map{case(a,b)=>(a,b.U)})), //io.wb.rfData,
        (io.in(i).bits.ctrl.src2Type === SrcType.reg) -> io.wb.ReadData1(i)
        ))
    }
    for(i <- 0 to Issue_Num-1){
        io.out(i).bits.data.src2 := PriorityMux(Seq(
        (io.in(i).bits.ctrl.src2Type =/= SrcType.reg) -> io.in(i).bits.data.imm,
        src2DependEX(i).reduce(_||_) -> io.forward(PriorityMux(src2DependEX(i).zipWithIndex.map{case(a,b)=>(a,b.U)})).wb.rfData, //io.forward.wb.rfData,
        src2DependWB(i).reduce(_||_)  -> io.wb.WriteData(PriorityMux(src2DependWB(i).zipWithIndex.map{case(a,b)=>(a,b.U)})), //io.wb.rfData,
        (io.in(i).bits.ctrl.src2Type === SrcType.reg) -> io.wb.ReadData2(i)
        ))
    }
    for(i <- 0 to Issue_Num-1){
        io.out(i).bits.data.imm  := io.in(i).bits.data.imm
        io.out(i).bits.data.src_vector := 0.U
        io.out(i).bits.cf <> io.in(i).bits.cf
        io.out(i).bits.ctrl := io.in(i).bits.ctrl
        io.out(i).bits.ctrl.isBru := ALUOpType.isBru(io.in(i).bits.ctrl.fuOpType)
        io.out(i).bits.ctrl.isSrc1Forward := src1DependEX(i).reduce(_||_)
        io.out(i).bits.ctrl.isSrc2Forward := src2DependEX(i).reduce(_||_)
    }

    io.wb.rfSrc1 := VecInit((0 to Issue_Num-1).map(i => rfSrc1(i)))
    io.wb.rfSrc2 := VecInit((0 to Issue_Num-1).map(i => rfSrc2(i)))
    io.wb.rfSrc3 := DontCare
    if(Polaris_SIMDU_WAY_NUM != 0){
        (0 to Issue_Num-1).map(i => rfSrc3(i) := io.in(i).bits.ctrl.rfSrc3)
        val src3DependEX = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Forward_num-1).map(j => isLatestData(rfSrc3(i),io.forward(j).InstNo) && isDepend(rfSrc3(i), io.forward(j).wb.rfDest, forwardRfWen(j))))))
        val src3DependWB = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Commit_num-1).map(j => isLatestData(rfSrc3(i),io.wb.InstNo(j)) && isDepend(rfSrc3(i), io.wb.rfDest(j), io.wb.rfWen(j))))))
        (0 to Issue_Num-1).map(i => src3Ready(i) := !InstBoard.io.valid(rfSrc3(i))||src3DependEX(i).reduce(_||_)||src3DependWB(i).reduce(_||_))
        for(i <- 0 to Issue_Num-1){
            io.out(i).bits.data.src3 := PriorityMux(Seq(
            src3DependEX(i).reduce(_||_) -> io.forward(PriorityMux(src3DependEX(i).zipWithIndex.map{case(a,b)=>(a,b.U)})).wb.rfData, //io.forward.wb.rfData,
            src3DependWB(i).reduce(_||_) -> io.wb.WriteData(PriorityMux(src3DependWB(i).zipWithIndex.map{case(a,b)=>(a,b.U)})), //io.wb.rfData,
            true.B -> io.wb.ReadData3(i))
            )
        }
        io.wb.rfSrc3 := VecInit((0 to Issue_Num-1).map(i => rfSrc3(i)))
    }

    //logic of rfvec for instr0, instr1 ignored
    io.wb.rfSrcVec :=DontCare
    val vec_no  = PriorityMux((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfVector && !io.in(i).bits.ctrl.rfWen).zipWithIndex.map{case(a,b)=>(a,b.U)})
    val vec_num = 1.U(log2Up(NRReg).W) << io.in(vec_no).bits.ctrl.fuOpType(3,2)
    if(Polaris_Vector_LDST){
    val vec_ready = VecInit((0 to vector_wdata_width/XLEN -1).map(i => false.B))
    val SrcVec = rfSrcVec(vec_no)
    var l = List(0.U)
    for(i <- 0 to vector_wdata_width/XLEN -1){
        val res = WireInit(0.U(XLEN.W))
        val rfdest = SrcVec+i.U(log2Up(NRReg).W)
        io.wb.rfSrcVec(i) := rfdest
        when(i.U < vec_num){
            val DependEX = VecInit((0 to Forward_num-1).map(j => isLatestData(rfdest,io.forward(j).InstNo) && isDepend(rfdest, io.forward(j).wb.rfDest, forwardRfWen(j))))
            val DependWB = VecInit((0 to Commit_num-1).map(j => isLatestData(rfdest,io.wb.InstNo(j)) && isDepend(rfdest, io.wb.rfDest(j), io.wb.rfWen(j))))
            vec_ready(i) := !InstBoard.io.valid(rfdest) || DependEX.reduce(_||_) || DependWB.reduce(_||_)
            res := PriorityMux(Seq(
                DependEX.reduce(_||_) -> io.forward(PriorityMux(DependEX.zipWithIndex.map{case(a,b)=>(a,b.U)})).wb.rfData, //io.forward.wb.rfData,
                DependWB.reduce(_||_) -> io.wb.WriteData(PriorityMux(DependWB.zipWithIndex.map{case(a,b)=>(a,b.U)})), //io.wb.rfData,
                true.B -> io.wb.ReadDataVec(i))
            )
            Debug("!!!vec!!! %x rfdest %x InstBoardvalid %x DependEX %x DependWB %x\n",i.U,rfdest,InstBoard.io.valid(rfdest),DependEX.asUInt,DependWB.asUInt)
        }.otherwise{
            vec_ready(i) := true.B
        }
        l = List.concat(List(res) ,l)
    }
    io.out(vec_no).bits.data.src_vector := l.dropRight(1).reduce(Cat(_,_))
    (0 to Issue_Num-1).map(i => srcVecReady(i) := Mux(io.in(i).bits.ctrl.rfVector && !io.in(i).bits.ctrl.rfWen,Mux(i.U === vec_no,vec_ready.reduce(_&&_),false.B),true.B))
    //Debug("!!!vec!!! srcVecReady0 %x srcVecReady1 %x vec_ready %x SrcVec %x vec_no %x vec_num %x\n",srcVecReady(0),srcVecReady(1),vec_ready.asUInt,SrcVec,vec_no,vec_num)
    Debug("!!!vec!!! io.in(0).bits.ctrl.rfVector %x io.in(0).bits.ctrl.rfWen %x\n",io.in(0).bits.ctrl.rfVector,io.in(0).bits.ctrl.rfWen)
    //Debug("!!!vec!!! io.in(1).bits.ctrl.rfVector %x io.in(1).bits.ctrl.rfWen %x\n",io.in(1).bits.ctrl.rfVector,io.in(1).bits.ctrl.rfWen)
    }
    
    q.io.setnum := io.out.map(i => i.fire().asUInt).reduce(_+&_)
    q.io.flush  := io.flush
    q.io.clearnum:=io.num_enterwbu
    for(i <- 0 to Issue_Num-1){
        val newInstNo = q.io.HeadPtr + i.U
        val startNewQueue = newInstNo < q.io.HeadPtr
        io.out(i).bits.InstNo := q.io.HeadPtr + i.U
        io.out(i).bits.InstFlag:= Mux(startNewQueue,!q.io.Flag,q.io.Flag)
    }
    io.TailPtr := q.io.TailPtr

    InstBoard.io.Wen := 0.U.asTypeOf(InstBoard.io.Wen)
    InstBoard.io.WInstNo := 0.U.asTypeOf(InstBoard.io.WInstNo)
    InstBoard.io.clear := 0.U.asTypeOf(InstBoard.io.clear)
    InstBoard.io.flush   := io.flush

    for(i <- 0 to Issue_Num-1){
        when(io.out(i).fire() && rfWen(i)){
            InstBoard.io.Wen(rfDest(i)) := true.B
            InstBoard.io.WInstNo(rfDest(i)) := io.out(i).bits.InstNo
            if(Polaris_Vector_LDST){
                when(io.out(i).bits.ctrl.rfVector){
                    val vec_num_write = 1.U(log2Up(NRReg).W) << io.in(i).bits.ctrl.fuOpType(3,2)
                    for(j <- 1 to vector_rdata_width/XLEN -1){
                        when(j.U < vec_num_write){
                            val dest = rfDest(i)+j.U(log2Up(NRReg).W)
                            InstBoard.io.Wen(dest) := true.B
                            InstBoard.io.WInstNo(dest) := io.out(i).bits.InstNo
                        }
                    }
                }
            }
        }
    }
    for(i <- 0 to Commit_num-1){
        when(io.wb.rfWen(i) && io.wb.InstNo(i) === InstBoard.io.RInstNo(io.wb.rfDest(i))){
            InstBoard.io.clear(io.wb.rfDest(i)) := true.B
        }
    }
    if(Polaris_Vector_LDST){
        for(i <- 0 to vector_rdata_width/XLEN -1){
            when(io.wb.VecInstNo === InstBoard.io.RInstNo(io.wb.WriteDestVec(i))){
                InstBoard.io.clear(io.wb.WriteDestVec(i)) := true.B
            }
        }
    }
    
    for(i <- 0 to Issue_Num-1){
    Debug("[SIMD_ISU] issue %x valid %x rfSrc1 %x rfSrc2 %x rfdata1 %x rfdata2 %x rfsrc1ready %x rfsrc2ready %x\n", i.U,io.in(i).valid,rfSrc1(i),rfSrc2(i), io.out(i).bits.data.src1,io.out(i).bits.data.src2,src1Ready(i),src2Ready(i))
    Debug("[SIMD_ISU] issue %x outvalid %x InstNo %x pc %x inst %x futype %x futypeop %x src1ex %x src2ex %x src1wb %x src2wb %x \n",i.U,io.out(i).valid,io.out(i).bits.InstNo,io.in(i).bits.cf.pc,io.in(i).bits.cf.instr,io.in(i).bits.ctrl.fuType,io.in(i).bits.ctrl.fuOpType,src1DependEX(i).reduce(_|_),src2DependEX(i).reduce(_|_),src1DependWB(i).reduce(_|_),src2DependWB(i).reduce(_|_))
    Debug(io.out(i).fire(),"[SIMD_ISU] InstNo %x\n", io.out(i).bits.InstNo)
    }
    for(i <- 0 to Forward_num-1){
        Debug("[SIMD_ISU]futype %x rfdest %x rfwen %x wdata %x \n",i.U,io.forward(i).wb.rfDest,forwardRfWen(i),io.forward(i).wb.rfData)
    }
}
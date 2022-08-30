package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import difftest._

class SIMD_ISU(implicit val p:NutCoreConfig)extends NutCoreModule with HasRegFileParameter{
    val io = IO(new Bundle{
        val in = Vec(2,Flipped(Decoupled(new DecodeIO)))
        val out = Vec(Issue_Num,Decoupled(new DecodeIO))
        val wb = Flipped(new SIMD_WriteBackIO)
        val forward = Vec(Issue_Num,Flipped(new ForwardIO))
        val flush = Input(Bool())
    })
    for(i<-0 to Issue_Num-1){
        io.out(i):=DontCare
    }
    val rfSrc1 = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfSrc1))
    val rfSrc2 = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfSrc2))
    val rfDest = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfDest))
    val rfWen  = VecInit((0 to Issue_Num-1).map(i => io.in(i).bits.ctrl.rfWen ))

    def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool): Bool = (rfSrc =/= 0.U) && (rfSrc === rfDest) && wen
    def isCsrOp(i:Int):Bool = io.in(i).bits.ctrl.fuType === FuType.csr

    val forwardRfWen = VecInit((0 to Issue_Num-1).map(i => io.forward(i).wb.rfWen && io.forward(i).valid))
    val dontForward = VecInit((0 to Issue_Num-1).map(i => (io.forward(i).fuType =/= FuType.alu) && (io.forward(i).fuType =/= FuType.lsu)))
    val src1DependEX = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Issue_Num-1).map(j => isDepend(rfSrc1(i), io.forward(j).wb.rfDest, forwardRfWen(j))))))
    val src2DependEX = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Issue_Num-1).map(j => isDepend(rfSrc2(i), io.forward(j).wb.rfDest, forwardRfWen(j))))))
    val src1DependWB = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Issue_Num-1).map(j => isDepend(rfSrc1(i), io.wb.rfDest(j), io.wb.rfWen(j))))))
    val src2DependWB = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Issue_Num-1).map(j => isDepend(rfSrc2(i), io.wb.rfDest(j), io.wb.rfWen(j))))))

    val src1ForwardNextCycle = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Issue_Num-1).map(j => src1DependEX(i)(j) && !dontForward(j)))))
    val src2ForwardNextCycle = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Issue_Num-1).map(j => src2DependEX(i)(j) && !dontForward(j)))))

    val src1Forward = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Issue_Num-1).map(j => src1DependWB(i)(j) && !(src1DependEX(i).zip(dontForward).map{case (a,b) => a && b}.reduce(_||_))))))
    val src2Forward = VecInit((0 to Issue_Num-1).map(i=>VecInit((0 to Issue_Num-1).map(j => src2DependWB(i)(j) && !(src2DependEX(i).zip(dontForward).map{case (a,b) => a && b}.reduce(_||_))))))

    val sb = new ScoreBoard
    val src1Ready = VecInit((0 to Issue_Num-1).map(i => !sb.isBusy(rfSrc1(i))||src1ForwardNextCycle(i).reduce(_||_)||src1Forward(i).reduce(_||_)))
    val src2Ready = VecInit((0 to Issue_Num-1).map(i => !sb.isBusy(rfSrc2(i))||src2ForwardNextCycle(i).reduce(_||_)||src2Forward(i).reduce(_||_)))

    val RAWinIssue = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool())) 
                                                        for(j <- 0 to i-1){
                                                                raw(j) := io.in(j).valid && (isDepend(rfSrc1(i),rfDest(j),rfWen(j))||isDepend(rfSrc2(i),rfDest(j),rfWen(j)))
                                                        }
                                                        for(j <- i to Issue_Num-1){
                                                                raw(j) := false.B 
                                                        }
                                                        raw.reduce(_||_)}))
    val FightforSource = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool()))  
                                                            for(j <- 0 to i-1){
                                                                raw(j) := io.in(j).valid && (io.in(j).bits.ctrl.fuType === io.in(i).bits.ctrl.fuType)
                                                            }
                                                            for(j <- i to Issue_Num-1){
                                                                raw(j) := false.B 
                                                            }
                                                            raw.reduce(_||_)}))

    val FrontHasCsrOp = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool())) 
                                                            for(j <- 0 to i-1){
                                                                raw(j) := io.in(j).valid && io.in(j).bits.ctrl.fuType === FuType.csr
                                                            }
                                                            for(j <- i to Issue_Num-1){
                                                                raw(j) := false.B 
                                                            }
                                                            raw.reduce(_||_)}))
    val FrontisClear = VecInit((0 to Issue_Num-1).map(i => {val raw = Wire(Vec(Issue_Num,Bool())) 
                                                            for(j <- 0 to i-1){
                                                                raw(j) := !io.in(j).valid && io.out(j).ready
                                                            }
                                                            for(j <- i to Issue_Num-1){
                                                                raw(j) := false.B 
                                                            }
                                                            raw.reduce(_||_)}))

    for(i <- 0 to Issue_Num-1){
        if(i == 0){
            io.out(i).valid := io.in(i).valid && src1Ready(i) && src2Ready(i)
        }else{
            io.out(i).valid := io.in(i).valid && src1Ready(i) && src2Ready(i) && !RAWinIssue(i) && !FightforSource(i) && !FrontHasCsrOp(i) && !(isCsrOp(i) && !FrontisClear(i))
        }
    }

    io.in(0).ready := VecInit((0 to Issue_Num-1).map(i => !io.in(i).valid||io.out(i).fire())).reduce(_&&_)
    io.in(1).ready := false.B

    for(i <- 0 to Issue_Num-1){
        io.out(i).bits.data.src1 := Mux1H(List(
        (io.in(i).bits.ctrl.src1Type === SrcType.pc) -> SignExt(io.in(i).bits.cf.pc, AddrBits),
        src1ForwardNextCycle(i).reduce(_||_) -> io.forward(PriorityMux(src1ForwardNextCycle(i).zipWithIndex.map{case(a,b)=>(a,b.U)})).wb.rfData, //io.forward.wb.rfData,
        (src1Forward(i).reduce(_||_) && !src1ForwardNextCycle(i).reduce(_||_)) -> io.wb.WriteData(PriorityMux(src1Forward(i).zipWithIndex.map{case(a,b)=>(a,b.U)})), //io.wb.rfData,
        ((io.in(i).bits.ctrl.src1Type =/= SrcType.pc) && !src1ForwardNextCycle(i).reduce(_||_) && !src1Forward(i).reduce(_||_)) -> io.wb.ReadData1(i)
        ))
    }
    for(i <- 0 to Issue_Num-1){
        io.out(i).bits.data.src2 := Mux1H(List(
        (io.in(i).bits.ctrl.src2Type =/= SrcType.reg) -> io.in(i).bits.data.imm,
        src2ForwardNextCycle(i).reduce(_||_) -> io.forward(PriorityMux(src2ForwardNextCycle(i).zipWithIndex.map{case(a,b)=>(a,b.U)})).wb.rfData, //io.forward.wb.rfData,
        (src2Forward(i).reduce(_||_) && !src2ForwardNextCycle(i).reduce(_||_)) -> io.wb.WriteData(PriorityMux(src2Forward(i).zipWithIndex.map{case(a,b)=>(a,b.U)})), //io.wb.rfData,
        ((io.in(i).bits.ctrl.src2Type === SrcType.reg) && !src2ForwardNextCycle(i).reduce(_||_) && !src2Forward(i).reduce(_||_)) -> io.wb.ReadData2(i)
        ))
    }
    for(i <- 0 to Issue_Num-1){
        io.out(i).bits.data.imm  := io.in(i).bits.data.imm
        io.out(i).bits.cf <> io.in(i).bits.cf
        io.out(i).bits.ctrl := io.in(i).bits.ctrl
        io.out(i).bits.ctrl.isSrc1Forward := src1ForwardNextCycle(i).reduce(_||_)
        io.out(i).bits.ctrl.isSrc2Forward := src2ForwardNextCycle(i).reduce(_||_)
    }
    val wbClearMask = VecInit((0 to Issue_Num-1).map(i=>Mux(io.wb.rfWen(i) && !VecInit((0 to Issue_Num-1).map(j=>isDepend(io.wb.rfDest(i), io.forward(j).wb.rfDest, forwardRfWen(j)))).reduce(_||_), sb.mask(io.wb.rfDest(i)), 0.U(NRReg.W)))).reduce(_|_)
    val isuFireSetMask = VecInit((0 to Issue_Num-1).map(i=>Mux(io.out(i).fire()&&rfWen(i), sb.mask(rfDest(i)), 0.U))).reduce(_|_)
    when (io.flush) { sb.update(0.U, Fill(NRReg, 1.U(1.W)))}
    .otherwise { sb.update(isuFireSetMask, wbClearMask) }

    io.wb.rfSrc1 := VecInit((0 to Issue_Num-1).map(i => rfSrc1(i)))
    io.wb.rfSrc2 := VecInit((0 to Issue_Num-1).map(i => rfSrc2(i)))
}
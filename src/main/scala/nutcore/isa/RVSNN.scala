package nutcore

import chisel3._
import chisel3.util._

object RVSNNInstr extends HasInstrType{
    def NUP =   BitPat("b000000_?_?????_?????_000_?????_0001011")
    def BPO =   BitPat("b0000001_?????_?????_001_?????_0001011")
    def EXP =   BitPat("b0000000_00000_?????_010_?????_0001011")
    def TDR =   BitPat("b0000000_?????_?????_100_?????_0001011")
    def SUM =   BitPat("b000000_?_?????_?????_101_?????_0001011")
    def SVR =   BitPat("b0000011_?????_?????_111_?????_0001011")

    val table = Array(
        NUP         ->  List(InstrSNN, FuType.snnu, SNNOpType.nup),
        BPO         ->  List(InstrSNN, FuType.snnu, SNNOpType.bpo),
        EXP         ->  List(InstrSNN, FuType.snnu, SNNOpType.exp),
        TDR         ->  List(InstrSNN, FuType.snnu, SNNOpType.tdr),
        SUM         ->  List(InstrSNN, FuType.snnu, SNNOpType.sum),
        SVR         ->  List(InstrSNN, FuType.snnu, SNNOpType.svr)
    )
}
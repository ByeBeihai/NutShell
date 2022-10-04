package nutcore

import chisel3._
import chisel3.util._

object RVPInstr extends HasInstrType {
  def ADD16   = BitPat("b0100000_?????_?????_000_?????_1110111")
  
  val table = Array(
    ADD16       -> List(InstrP, FuType.simdu, LSUOpType.amomaxu)
  )
}
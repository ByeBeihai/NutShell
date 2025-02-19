package nutcore

import chisel3._
import chisel3.util._
import utils._
import nutcore.backend.fu.fpu.fudian._

object float {
  case class FType(expWidth: Int, sigWidth: Int) {
    val len = expWidth + sigWidth
  }

  val fp16 = FType(5, 11)
  val fp32 = FType(8, 24)
  val fp64 = FType(11, 53)

  def unbox(x: UInt, ftype: FType): UInt = {
    Mux(x.head(x.getWidth - ftype.len).andR,
      x.tail(ftype.len),
      FloatPoint.defaultNaNUInt(ftype.expWidth, ftype.sigWidth)
    )
  }

  def box(x: UInt, ftype: FType): UInt = {
    require(x.getWidth <= ftype.len)
    Cat(~0.U((ftype.len - x.getWidth).W), x)
  }
}

object FPUOpType extends FMAOpType with FCONVOpType with FDivSqrtOpType with FCOMPOpType {

}


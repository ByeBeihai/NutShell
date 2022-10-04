object SIMDUOpType {
  def add16  = "b1110111".U
}

class SIMDU_IO extends FunctionUnitIO {
  val flush = Input(Bool())
  val DecodeOut = new DecodeIO
  val DecodeIn = Flipped(new DecodeIO)
}
class ALU(hasBru: Boolean = false,NO1: Boolean = true) extends NutCoreModule {
  val io = IO(new SIMDU_IO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  //add16临时逻辑
  val add16_result = Wire(new MyBundle)
  add16_result.a := src1.asTypeOf(new MyBundle).a + src2.asTypeOf(new MyBundle).a
  add16_result.b := src1.asTypeOf(new MyBundle).b + src2.asTypeOf(new MyBundle).b
  add16_result.c := src1.asTypeOf(new MyBundle).c + src2.asTypeOf(new MyBundle).c
  add16_result.d := src1.asTypeOf(new MyBundle).d + src2.asTypeOf(new MyBundle).d

  val res = LookupTreeDefault(func, 0.U, List(
    SIMDUOpType.add16 -> add16_result
  ))
  io.out.bits := add16_result
  io.DecodeOut := io.DecodeIn
}
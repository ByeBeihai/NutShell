package nutcore

import chisel3._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import top.Settings
import difftest._

object CSA {
    def apply(src1: UInt, src2: UInt, src3: UInt) : (UInt, UInt) = {
        val t1 = src1 ^ src2
        val sum = t1 ^ src3
        val carry = VecInit((0 until t1.getWidth).map(i => Mux(t1(i), src3(i), src1(i)))).asUInt // Mux(t1, src3, src1)
        (sum, carry)
    }
}

object CP53 {
    def apply(src1: Bool, src2: Bool, src3: Bool, src4: Bool, cin: Bool) = {
        val t1 = src1 ^ src2
        val t2 = src3 ^ src4
        val cout = Mux(t1, src3, src1)
        val t3 = t1 ^ t2
        val sum = t3 ^ cin
        val carry = Mux(t3, cin, src4)
        Cat(sum, carry, cout)
    }
}

object CP42 {
    def apply(src1: UInt, src2: UInt, src3: UInt, src4: UInt) = {
        val Width = src1.getWidth
        val all =Wire(Vec(Width, UInt(3.W)))
        all(0) := CP53(src1(0),src2(0),src3(0),src4(0), false.B)
        (1 until Width).map(i => all(i) := CP53(src1(i), src2(i), src3(i), src4(i), all(i-1)(0)))
        
        (Cat(all(Width-1)(0), VecInit((0 until Width).map(all(_)(2))).asUInt),
          Cat(VecInit((0 until Width).map(all(_)(1))).asUInt, 0.U(1.W)))
    }
}

object EXT {
    def apply(src: UInt, p: Int, s: Int) = {
        if(p==0)
            if(s==0) src
            else     Cat(src, 0.U(s.W))
        else
            if(s==0) Cat(0.U(p.W), src)
            else     Cat(0.U(p.W), src, 0.U(s.W))
    }
}

class MulAddIO(val len: Int = 65) extends NutCoreBundle {
    val in = Flipped(DecoupledIO(new Bundle{
        val DecodeIn = new DecodeIO
        val srcs     = Vec(3, Output(UInt(len.W)))
    }))
    val out = Decoupled(new Bundle{
        val result = Output(UInt((2*len).W))
        val DecodeOut = new DecodeIO
        val OV     = Output(Bool())
    })
    //val sub = Input(Bool()) //mul-sub
    val flush = Input(Bool()) 
    val FirstStageFire = Output(Bool())
}

class MulAdd65(len: Int = 65) extends NutCoreModule {
    val io = IO(new MulAddIO(len)) // TODO: len only supports 65
    /* assume signed-unsigned is hangled by MDU*/
    // val X = io.in.bits.src1
    // val Y = io.in.bits.src2
    // val Z = io.in.bits.src3
    val valid = io.in.valid
    val X = io.in.bits.srcs(0)
    val Y = io.in.bits.srcs(1)
    val Z = io.in.bits.srcs(2)
    // X * Y + Z. X,Y,Z is both 65-bits, pre-processed by out-module
    // -(X * Y) + Z
    
    // 1st stage: generate partial
    // 2nd stage: compress
    // 3rd stage: add
    // 4th stage: neg
    val s1Fire = Wire(Bool())
    val s2Fire = Wire(Bool())
    val s3Fire = Wire(Bool())
    val s4Fire = Wire(Bool())
    val s1_valid = io.in.valid
    val s2_valid = RegEnable(false.B, init = false.B, s2Fire || io.flush)
    val s3_valid = RegEnable(false.B, init = false.B, s3Fire || io.flush)
    val s4_valid = RegEnable(false.B, init = false.B, s4Fire || io.flush)
    val s2_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s2Fire)
    val s3_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s3Fire)
    val s4_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s4Fire)
    val subS3 = Reg(Bool()) // for mul-sub, get into s4Stage
    when (s1Fire) { 
        s2_valid := true.B 
        s2_decodein := io.in.bits.DecodeIn
    }
    when (s2Fire) { 
        s3_valid := true.B
        s3_decodein := s2_decodein
    }
    when (s3Fire && subS3) { 
        s4_valid := true.B 
        s4_decodein :=s3_decodein
    }
    val s1_ready = !s1_valid || s1Fire
    val s2_ready = !s2_valid || s2Fire
    val s3_ready = !s3_valid || s3Fire
    val s4_ready = !s4_valid || s4Fire
    s1Fire := s1_valid && s2_ready
    s2Fire := s2_valid && s3_ready
    s3Fire := Mux(s3_valid && !subS3, s3_valid && io.out.ready, s3_valid && s4_ready)
    s4Fire := s4_valid && io.out.ready
    
    // first stage
    val lenOdd = len % 2 == 1
    assert(lenOdd)
    val Len = if(lenOdd) len+1 else len //
    val NBooth:Int = Len / 2 //
    
    val y_extend = if(lenOdd) Cat(Y(len-1), Y, 0.U(1.W)) else Cat(Y, 0.U(1.W))
    val y_part = Wire(Vec(NBooth, UInt(3.W))) // radix-4 booth
    (0 until NBooth).map(i => y_part(i) := y_extend(i*2+2, i*2))
    val booth_x = Wire(Vec(NBooth, UInt(Len.W)))
    (0 until NBooth).map(i => booth_x(i) := LookupTree(y_part(i), List(
        "b000".U  -> 0.U(Len.W),
        "b001".U  -> SignExt(X, Len),
        "b010".U  -> SignExt(X, Len),
        "b011".U  -> SignExt(X << 1.U, Len),
        "b100".U  -> SignExt(~(X << 1.U), Len),
        "b101".U  -> ~SignExt(X, Len),
        "b110".U  -> ~SignExt(X, Len),
        "b111".U  -> 0.U(Len.W)
    ))) // FIXME: handle the case that len is not odd, 2*X is hard to handle.
    
    val isSub = VecInit((0 until NBooth).map(i => y_part(i)(2).asBool && y_part(i) =/= "b111".U)).asUInt
    val isNeg = VecInit((0 until NBooth).map(i => booth_x(i)(Len-1))).asUInt
    
    val boothWire = Wire(Vec(NBooth, UInt((Len+3).W)))
    (1 until NBooth).map(i => boothWire(i) := Cat(1.U(1.W), ~isNeg(i), booth_x(i)(Len-2, 0), 0.U(1.W), isSub(i-1)))
    boothWire(0) := Cat(0.U(2.W), 1.U(1.W), ~isNeg(0), booth_x(0)(Len-2,0))
    
    // second stage
    val booth = RegEnable(boothWire, s1Fire)
    val Z_reg = RegEnable(Z, s1Fire)
    val subS2 = RegEnable(false.B, s1Fire)
    val Z_add = Mux(subS2, ~Z_reg, Z_reg)
    
    val last_booth = booth(NBooth-1)(Len+1, 0)
    
    // 33+1 => 32 + 2 => 16 + 2    => 8 + 2 =>
    // first layer, use 42-compressor
    val boothZ = Cat(0.U(1.W), ~Z_add(64), Z_add(64,0)) // 2+65 = 67
    val (s0_0, c0_0) = CP42(EXT(boothZ,4,0),    EXT(booth(0),2,0),  EXT(booth(1),2,0),  EXT(booth(2),0,2)) //72
    val (s0_1, c0_1) = CP42(EXT(booth(3),6,0),  EXT(booth(4),4,2),  EXT(booth(5),2,4),  EXT(booth(6),0,6)) //76
    val (s0_2, c0_2) = CP42(EXT(booth(7),6,0),  EXT(booth(8),4,2),  EXT(booth(9),2,4),  EXT(booth(10),0,6))
    val (s0_3, c0_3) = CP42(EXT(booth(11),6,0), EXT(booth(12),4,2), EXT(booth(13),2,4), EXT(booth(14),0,6))
    val (s0_4, c0_4) = CP42(EXT(booth(15),6,0), EXT(booth(16),4,2), EXT(booth(17),2,4), EXT(booth(18),0,6))
    val (s0_5, c0_5) = CP42(EXT(booth(19),6,0), EXT(booth(20),4,2), EXT(booth(21),2,4), EXT(booth(22),0,6))
    val (s0_6, c0_6) = CP42(EXT(booth(23),6,0), EXT(booth(24),4,2), EXT(booth(25),2,4), EXT(booth(26),0,6))
    val (s0_7, c0_7) = CP42(EXT(booth(27),6,0), EXT(booth(28),4,2), EXT(booth(29),2,4), EXT(booth(30),0,6))
    val add1_0 = s0_0(3,0)
    val add2_0 = c0_0(3,0)
    // second layer
    val (s1_0, c1_0) = CP42(EXT(s0_0(71,4),8,0), EXT(c0_0(71,4),8,0), s0_1, c0_1) // 77
    val (s1_1, c1_1) = CP42(EXT(s0_2,8,0), EXT(c0_2,8,0), EXT(s0_3,0,8), EXT(c0_3,0,8)) //85
    val (s1_2, c1_2) = CP42(EXT(s0_4,8,0), EXT(c0_4,8,0), EXT(s0_5,0,8), EXT(c0_5,0,8)) //85
    val (s1_3, c1_3) = CP42(EXT(s0_6,8,0), EXT(c0_6,8,0), EXT(s0_7,0,8), EXT(c0_7,0,8)) //85
    val add1_1 = s1_0(7,0)
    val add2_1 = c1_0(7,0)
    // third layer
    val (s2_0, c2_0) = CP42(EXT(s1_0(76,8),16,0), EXT(c1_0(76,8),16,0), s1_1, c1_1) // 86
    val (s2_1, c2_1) = CP42(EXT(s1_2,16,0), EXT(c1_2,16,0), EXT(s1_3,0,16), EXT(c1_3,0,16))// 102
    val add1_2 = s2_0(15,0)
    val add2_2 = c2_0(15,0)
    // fourth layer
    val (s3_0, c3_0) = CP42(EXT(s2_0(85,16),32,0), EXT(c2_0(85,16),32,0), s2_1, c2_1)//.map(_(101,0))
    val add1_3 = s3_0(31,0)
    val add2_3 = c3_0(31,0)
    // firth layer
    val (s4_0, c4_0) = CP42(s3_0(101,32), c3_0(101,32), EXT(booth(NBooth-2),1,0), EXT(last_booth,0,2))
    val add1_4 = s4_0(69,0)
    val add2_4 = c4_0(69,0)
    
    // third stage
    val add1 = RegEnable(Cat(add1_4, add1_3, add1_2, add1_1, add1_0), s2Fire)
    val add2 = RegEnable(Cat(add2_4, add2_3, add2_2, add2_1, add2_0), s2Fire)
    when(s2Fire) { subS3 := subS2 } // assign subS3 here
    val res = add1 + add2 + subS3
    
    // fourth stage
    val resS4 = RegEnable(res(len-1, 0), s3Fire)
    val resSub = ~resS4 + 1.U
    
    io.in.ready := s1_ready
    io.out.bits.result := Mux(s3_valid && !subS3, res(2*len-1,0), ZeroExt(resSub, 2*len))
    io.out.bits.DecodeOut := Mux(s3_valid && !subS3, s3_decodein, s4_decodein)
    io.out.valid := Mux(s3_valid && !subS3, s3_valid, s4_valid)
    io.out.bits.OV := false.B
    io.FirstStageFire := s1Fire
}

class MulAdd33(len: Int = 33) extends NutCoreModule {
    val io = IO(new MulAddIO(len)) // TODO: len only supports 65
    /* assume signed-unsigned is hangled by MDU*/
    // val X = io.in.bits.src1
    // val Y = io.in.bits.src2
    // val Z = io.in.bits.src3
    val valid = io.in.valid
    val X = io.in.bits.srcs(0)
    val Y = io.in.bits.srcs(1)
    val Z = io.in.bits.srcs(2)
    // X * Y + Z. X,Y,Z is both 65-bits, pre-processed by out-module
    // -(X * Y) + Z
    
    // 1st stage: generate partial
    // 2nd stage: compress
    // 3rd stage: add
    // 4th stage: neg
    val s1Fire = Wire(Bool())
    val s2Fire = Wire(Bool())
    val s3Fire = Wire(Bool())
    val s4Fire = Wire(Bool())
    val s1_valid = io.in.valid
    val s2_valid = RegEnable(false.B, init = false.B, s2Fire || io.flush)
    val s3_valid = RegEnable(false.B, init = false.B, s3Fire || io.flush)
    val s4_valid = RegEnable(false.B, init = false.B, s4Fire || io.flush)
    val s2_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s2Fire)
    val s3_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s3Fire)
    val s4_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s4Fire)
    val subS3 = Reg(Bool()) // for mul-sub, get into s4Stage
    when (s1Fire) { 
        s2_valid := true.B 
        s2_decodein := io.in.bits.DecodeIn
    }
    when (s2Fire) { 
        s3_valid := true.B
        s3_decodein := s2_decodein
    }
    when (s3Fire && subS3) { 
        s4_valid := true.B 
        s4_decodein :=s3_decodein
    }
    val s1_ready = !s1_valid || s1Fire
    val s2_ready = !s2_valid || s2Fire
    val s3_ready = !s3_valid || s3Fire
    val s4_ready = !s4_valid || s4Fire
    s1Fire := s1_valid && s2_ready
    s2Fire := s2_valid && s3_ready
    s3Fire := Mux(s3_valid && !subS3, s3_valid && io.out.ready, s3_valid && s4_ready)
    s4Fire := s4_valid && io.out.ready
    
    // First Stage
    val lenOdd = len % 2 == 1
    assert(lenOdd)
    val Len = if(lenOdd) len+1 else len //
    val NBooth:Int = Len / 2 //
    
    val y_extend = if(lenOdd) Cat(Y(len-1), Y, 0.U(1.W)) else Cat(Y, 0.U(1.W))
    val y_part = Wire(Vec(NBooth, UInt(3.W))) // radix-4 booth
    (0 until NBooth).map(i => y_part(i) := y_extend(i*2+2, i*2))
    val booth_x = Wire(Vec(NBooth, UInt(Len.W)))
    (0 until NBooth).map(i => booth_x(i) := LookupTree(y_part(i), List(
        "b000".U  -> 0.U(Len.W),
        "b001".U  -> SignExt(X, Len),
        "b010".U  -> SignExt(X, Len),
        "b011".U  -> SignExt(X << 1.U, Len),
        "b100".U  -> SignExt(~(X << 1.U), Len),
        "b101".U  -> ~SignExt(X, Len),
        "b110".U  -> ~SignExt(X, Len),
        "b111".U  -> 0.U(Len.W)
    ))) // FIXME: handle the case that len is not odd, 2*X is hard to handle.
    
    val isSub = VecInit((0 until NBooth).map(i => y_part(i)(2).asBool && y_part(i) =/= "b111".U)).asUInt
    val isNeg = VecInit((0 until NBooth).map(i => booth_x(i)(Len-1))).asUInt
    
    val boothWire = Wire(Vec(NBooth, UInt((Len+3).W)))
    (1 until NBooth).map(i => boothWire(i) := Cat(1.U(1.W), ~isNeg(i), booth_x(i)(Len-2, 0), 0.U(1.W), isSub(i-1)))
    boothWire(0) := Cat(0.U(2.W), 1.U(1.W), ~isNeg(0), booth_x(0)(Len-2,0))
    
    // second stage
    val booth = RegEnable(boothWire, s1Fire)
    val Z_reg = RegEnable(Z, s1Fire)
    val subS2 = RegEnable(false.B, s1Fire)
    val Z_add = Mux(subS2, ~Z_reg, Z_reg)
    
    val last_booth = booth(NBooth-1)(Len+1, 0)
    
    // 17+1
    // => 4 + 4 + 4 + 4 + 2
    // => 4 + 4 + 2
    // => 4 + 2 (or 3 + 3)
    // => 4
    // => 2
    val boothZ = Cat(0.U(1.W), ~Z_add(32), Z_add(32,0)) // 2+len=35
    val (s0_0, c0_0) = CP42(EXT(boothZ,4,0),    EXT(booth(0),2,0),  EXT(booth(1),2,0),  EXT(booth(2),0,2)) //40
    val (s0_1, c0_1) = CP42(EXT(booth(3),6,0),  EXT(booth(4),4,2),  EXT(booth(5),2,4),  EXT(booth(6),0,6)) //44
    val (s0_2, c0_2) = CP42(EXT(booth(7),6,0),  EXT(booth(8),4,2),  EXT(booth(9),2,4),  EXT(booth(10),0,6))
    val (s0_3, c0_3) = CP42(EXT(booth(11),6,0), EXT(booth(12),4,2), EXT(booth(13),2,4), EXT(booth(14),0,6)) //44
    val add1_0 = s0_0(3,0)
    val add2_0 = c0_0(3,0)
    // seconde layer
    val (s1_0, c1_0) = CP42(EXT(s0_0(39,4),8,0), EXT(c0_0(39,4),8,0), s0_1, c0_1) // 45
    val (s1_1, c1_1) = CP42(EXT(s0_2,8,0), EXT(c0_2,8,0), EXT(s0_3,0,8), EXT(c0_3,0,8)) //53
    val add1_1 = s1_0(7,0)
    val add2_1 = c1_0(7,0)
    // third layer
    val (s2_0, c2_0) = CP42(EXT(s1_0(44,8),16,0), EXT(c1_0(44,8),16,0), s1_1, c1_1) // 54
    val add1_2 = s2_0(15,0)
    val add2_2 = c2_0(15,0)
    // fouth layer
    // val (s3_0, c3_0) = cp42(s2_0(53,16), c2_0(53,16), s0_4, Cat(c0_4(c0_4.getWidth-2,0),0.U(1.W))) //39
    val (s3_0, c3_0) = CP42(s2_0(53,16), c2_0(53,16), EXT(booth(NBooth-2),1,0), EXT(last_booth,0,2))
    val add1_3 = s3_0(37,0)
    val add2_3 = c3_0(37,0)
    
    // Third stage
    // 4 + 8 + 16 + 38 => 66
    val add1 = RegEnable(Cat(add1_3, add1_2, add1_1, add1_0), s2Fire)
    val add2 = RegEnable(Cat(add2_3, add2_2, add2_1, add2_0), s2Fire)
    when(s2Fire) { subS3 := subS2 } // assign subS3 here
    val res = add1 + add2 + subS3
    
    // fourth stage
    val resS4 = RegEnable(res(len-1, 0), s3Fire)
    val resSub = ~resS4 + 1.U
    
    io.in.ready := s1_ready
    io.out.bits.result := Mux(s3_valid && !subS3, res(2*len-1,0), ZeroExt(resSub, 2*len))
    io.out.bits.DecodeOut := Mux(s3_valid && !subS3, s3_decodein, s4_decodein)
    io.out.valid := Mux(s3_valid && !subS3, s3_valid, s4_valid)
    io.out.bits.OV := false.B
    io.FirstStageFire := s1Fire
}

class MulAdd17(len: Int = 17) extends NutCoreModule {
    val io = IO(new MulAddIO(len)) // TODO: len only supports 17
    /* assume signed-unsigned is hangled by MDU*/
    // val X = io.in.bits.src1
    // val Y = io.in.bits.src2
    // val Z = io.in.bits.src3
    val valid = io.in.valid
    val X = io.in.bits.srcs(0)
    val Y = io.in.bits.srcs(1)
    val Z = io.in.bits.srcs(2)
    // X * Y + Z. X,Y,Z is both 65-bits, pre-processed by out-module
    // -(X * Y) + Z
    
    // 1st stage: generate partial
    // 2nd stage: compress
    // 3rd stage: add
    // 4th stage: neg
    val s1Fire = Wire(Bool())
    val s2Fire = Wire(Bool())
    val s3Fire = Wire(Bool())
    val s4Fire = Wire(Bool())
    val s1_valid = io.in.valid
    val s2_valid = RegEnable(false.B, init = false.B, s2Fire || io.flush)
    val s3_valid = RegEnable(false.B, init = false.B, s3Fire || io.flush)
    val s4_valid = RegEnable(false.B, init = false.B, s4Fire || io.flush)
    val s2_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s2Fire)
    val s3_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s3Fire)
    val s4_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s4Fire)
    val subS3 = Reg(Bool()) // for mul-sub, get into s4Stage
    when (s1Fire) { 
        s2_valid := true.B 
        s2_decodein := io.in.bits.DecodeIn
    }
    when (s2Fire) { 
        s3_valid := true.B
        s3_decodein := s2_decodein
    }
    when (s3Fire && subS3) { 
        s4_valid := true.B 
        s4_decodein :=s3_decodein
    }
    val s1_ready = !s1_valid || s1Fire
    val s2_ready = !s2_valid || s2Fire
    val s3_ready = !s3_valid || s3Fire
    val s4_ready = !s4_valid || s4Fire
    s1Fire := s1_valid && s2_ready
    s2Fire := s2_valid && s3_ready
    s3Fire := Mux(s3_valid && !subS3, s3_valid && io.out.ready, s3_valid && s4_ready)
    s4Fire := s4_valid && io.out.ready
    
    // First Stage
    val lenOdd = len % 2 == 1
    assert(lenOdd)
    val Len = if(lenOdd) len+1 else len //
    val NBooth:Int = Len / 2 //
    
    val y_extend = if(lenOdd) Cat(Y(len-1), Y, 0.U(1.W)) else Cat(Y, 0.U(1.W))
    val y_part = Wire(Vec(NBooth, UInt(3.W))) // radix-4 booth
    (0 until NBooth).map(i => y_part(i) := y_extend(i*2+2, i*2))
    val booth_x = Wire(Vec(NBooth, UInt(Len.W)))
    (0 until NBooth).map(i => booth_x(i) := LookupTree(y_part(i), List(
        "b000".U  -> 0.U(Len.W),
        "b001".U  -> SignExt(X, Len),
        "b010".U  -> SignExt(X, Len),
        "b011".U  -> SignExt(X << 1.U, Len),
        "b100".U  -> SignExt(~(X << 1.U), Len),
        "b101".U  -> ~SignExt(X, Len),
        "b110".U  -> ~SignExt(X, Len),
        "b111".U  -> 0.U(Len.W)
    ))) // FIXME: handle the case that len is not odd, 2*X is hard to handle.
    
    val isSub = VecInit((0 until NBooth).map(i => y_part(i)(2).asBool && y_part(i) =/= "b111".U)).asUInt
    val isNeg = VecInit((0 until NBooth).map(i => booth_x(i)(Len-1))).asUInt
    
    val boothWire = Wire(Vec(NBooth, UInt((Len+3).W)))
    (1 until NBooth).map(i => boothWire(i) := Cat(1.U(1.W), ~isNeg(i), booth_x(i)(Len-2, 0), 0.U(1.W), isSub(i-1)))
    boothWire(0) := Cat(0.U(2.W), 1.U(1.W), ~isNeg(0), booth_x(0)(Len-2,0))
    
    // Second Stage
    val booth = RegEnable(boothWire, s1Fire)
    val Z_reg = RegEnable(Z, s1Fire)
    val subS2 = RegEnable(false.B, s1Fire)
    val Z_add = Mux(subS2, ~Z_reg, Z_reg)
    
    val last_booth = booth(NBooth-1)(Len+1, 0)
    
    // 9 + 1 => 4 + 4 + 2 => 4 + 2 => 4 => 2
    // first layer, use 42-compressor
    val boothZ = Cat(0.U(1.W), ~Z_add(16), Z_add(16,0)) // 2+17=19
    val (s0_0, c0_0) = CP42(EXT(boothZ,4,0), EXT(booth(0),2,0), EXT(booth(1),2,0), EXT(booth(2),0,2)) //24
    val (s0_1, c0_1) = CP42(EXT(booth(3),6,0), EXT(booth(4),4,2), EXT(booth(5),2,4), EXT(booth(6),0,6)) //28
    val add1_0 = s0_0(3,0)
    val add2_0 = c0_0(3,0)
    // second layer
    val (s1_0, c1_0) = CP42(EXT(s0_0(23,4),8,0), EXT(c0_0(23,4),8,0), s0_1, c0_1) // 29
    val add1_1 = s1_0(7,0)
    val add2_1 = c1_0(7,0)
    // third layer
    val (s2_0, c2_0) = CP42(EXT(s1_0(28,8),1,0), EXT(c1_0(28,8),1,0), EXT(booth(NBooth-2),1,0), EXT(last_booth,0,2))
    val add1_2 = s2_0(21,0)
    val add2_2 = c2_0(21,0)
    
    // Third Stage
    // 4 + 8 + 22 => 34
    val add1 = RegEnable(Cat(add1_2, add1_1, add1_0), s2Fire)
    val add2 = RegEnable(Cat(add2_2, add2_1, add2_0), s2Fire)
    when(s2Fire) { subS3 := subS2 } // assign subS3 here
    val res = add1 + add2 + subS3
    
    // fourth stage
    val resS4 = RegEnable(res(len-1, 0), s3Fire)
    val resSub = ~resS4 + 1.U
    
    io.in.ready := s1_ready
    io.out.bits.result := Mux(s3_valid && !subS3, res(2*len-1,0), ZeroExt(resSub, 2*len))
    io.out.bits.DecodeOut := Mux(s3_valid && !subS3, s3_decodein, s4_decodein)
    io.out.valid := Mux(s3_valid && !subS3, s3_valid, s4_valid)
    io.out.bits.OV := false.B
    io.FirstStageFire := s1Fire
}

class MulAdd9(len: Int = 9) extends NutCoreModule {
    val io = IO(new MulAddIO(len)) // TODO: len only supports 65
    /* assume signed-unsigned is hangled by MDU*/
    // val X = io.in.bits.src1
    // val Y = io.in.bits.src2
    // val Z = io.in.bits.src3
    val valid = io.in.valid
    val X = io.in.bits.srcs(0)
    val Y = io.in.bits.srcs(1)
    val Z = io.in.bits.srcs(2)
    // X * Y + Z. X,Y,Z is both 65-bits, pre-processed by out-module
    // -(X * Y) + Z
    
    // 1st stage: generate partial
    // 2nd stage: compress
    // 3rd stage: add
    // 4th stage: neg
    val s1Fire = Wire(Bool())
    val s2Fire = Wire(Bool())
    val s3Fire = Wire(Bool())
    val s4Fire = Wire(Bool())
    val s1_valid = io.in.valid
    val s2_valid = RegEnable(false.B, init = false.B, s2Fire || io.flush)
    val s3_valid = RegEnable(false.B, init = false.B, s3Fire || io.flush)
    val s4_valid = RegEnable(false.B, init = false.B, s4Fire || io.flush)
    val s2_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s2Fire)
    val s3_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s3Fire)
    val s4_decodein = RegEnable(0.U.asTypeOf(new DecodeIO),init = 0.U.asTypeOf(new DecodeIO),s4Fire)
    val subS3 = Reg(Bool()) // for mul-sub, get into s4Stage
    when (s1Fire) { 
        s2_valid := true.B 
        s2_decodein := io.in.bits.DecodeIn
    }
    when (s2Fire) { 
        s3_valid := true.B
        s3_decodein := s2_decodein
    }
    when (s3Fire && subS3) { 
        s4_valid := true.B 
        s4_decodein :=s3_decodein
    }
    val s1_ready = !s1_valid || s1Fire
    val s2_ready = !s2_valid || s2Fire
    val s3_ready = !s3_valid || s3Fire
    val s4_ready = !s4_valid || s4Fire
    s1Fire := s1_valid && s2_ready
    s2Fire := s2_valid && s3_ready
    s3Fire := Mux(s3_valid && !subS3, s3_valid && io.out.ready, s3_valid && s4_ready)
    s4Fire := s4_valid && io.out.ready
    
    // First Stage
    val lenOdd = len % 2 == 1
    assert(lenOdd)
    val Len = if(lenOdd) len+1 else len //
    val NBooth = Len / 2 //
    
    val y_extend = if(lenOdd) Cat(Y(len-1), Y, 0.U(1.W)) else Cat(Y, 0.U(1.W))
    val y_part = Wire(Vec(NBooth, UInt(3.W))) // radix-4 booth
    (0 until NBooth).map(i => y_part(i) := y_extend(i*2+2, i*2))
    val booth_x = Wire(Vec(NBooth, UInt(Len.W)))
    (0 until NBooth).map(i => booth_x(i) := LookupTree(y_part(i), List(
        "b000".U  -> 0.U(Len.W),
        "b001".U  -> SignExt(X, Len),
        "b010".U  -> SignExt(X, Len),
        "b011".U  -> SignExt(X << 1.U, Len),
        "b100".U  -> SignExt(~(X << 1.U), Len),
        "b101".U  -> ~SignExt(X, Len),
        "b110".U  -> ~SignExt(X, Len),
        "b111".U  -> 0.U(Len.W)
    ))) // FIXME: handle the case that len is not odd, 2*X is hard to handle.
    
    val isSub = VecInit((0 until NBooth).map(i => y_part(i)(2).asBool && y_part(i) =/= "b111".U)).asUInt
    val isNeg = VecInit((0 until NBooth).map(i => booth_x(i)(Len-1))).asUInt
    
    val boothWire = Wire(Vec(NBooth, UInt((Len+3).W)))
    (1 until NBooth).map(i => boothWire(i) := Cat(1.U(1.W), ~isNeg(i), booth_x(i)(Len-2, 0), 0.U(1.W), isSub(i-1)))
    boothWire(0) := Cat(0.U(2.W), 1.U(1.W), ~isNeg(0), booth_x(0)(Len-2,0))
    
    // Second Stage
    val booth = RegEnable(boothWire, s1Fire)
    val Z_reg = RegEnable(Z, s1Fire)
    val subS2 = RegEnable(false.B, s1Fire)
    val Z_add = Mux(subS2, ~Z_reg, Z_reg)
    
    val last_booth = booth(NBooth-1)(Len+1, 0)
    
    // 5+1 => 4 + 2 => 4 => 2
    // first layer, use 42-compressor
    val boothZ = Cat(0.U(1.W), ~Z_add(8), Z_add(8,0)) // 2+9 = 11
    val (s0_0, c0_0) = CP42(EXT(boothZ,4,0), EXT(booth(0),2,0), EXT(booth(1),2,0), EXT(booth(2),0,2)) //16
    val add1_0 = s0_0(3,0)
    val add2_0 = c0_0(3,0)
    // second layer
    val (s1_0, c1_0) = CP42(EXT(s0_0(15,4),2,0), EXT(c0_0(15,4),2,0), EXT(booth(NBooth-2),1,0), EXT(last_booth,0,2))
    val add1_1 = s1_0(13,0)
    val add2_1 = c1_0(13,0)
    
    // Third Stage
    val add1 = RegEnable(Cat(add1_1, add1_0), s2Fire)
    val add2 = RegEnable(Cat(add2_1, add2_0), s2Fire)
    when(s2Fire) { subS3 := subS2 } // assign subS3 here
    val res = add1 + add2 + subS3
    
    // fourth stage
    val resS4 = RegEnable(res(len-1, 0), s3Fire)
    val resSub = ~resS4 + 1.U
    
    // io
    io.in.ready := s1_ready
    io.out.bits.result := Mux(s3_valid && !subS3, res(2*len-1,0), ZeroExt(resSub, 2*len))
    io.out.bits.DecodeOut := Mux(s3_valid && !subS3, s3_decodein, s4_decodein)
    io.out.valid := Mux(s3_valid && !subS3, s3_valid, s4_valid)
    io.out.bits.OV := false.B
    io.FirstStageFire := s1Fire
}


class PMDUIO extends PALUIO {
    val flush = Input(Bool())
    val FirstStageFire = Output(Bool())
}

class PMDU extends NutCoreModule {
    val io = IO(new PMDUIO)

    val valid = io.in.valid
    val src1  = io.in.bits.data.src1
    val src2  = io.in.bits.data.src2
    val src3  = io.in.bits.data.src3
    val func  = io.in.bits.ctrl.fuOpType

    def isMul_16(func:UInt)   = {func(2) === 0.U}
    def SrcSigned(func:UInt)  = {func(6,3) =/= "b1011".U}
    def Xsrc(func:UInt)       = {func(2,0) ===  "b001".U || func === "b1001011".U}
    def Saturating(func:UInt) = {func(1,0) ===  "b11".U}

    val MulAdd17_0 = Module(new MulAdd17)
    val MulAdd17_1 = Module(new MulAdd17)
    val MulAdd33_0 = Module(new MulAdd33)
    val MulAdd65_0 = Module(new MulAdd65)

    MulAdd17_0.io.in.bits := 0.U.asTypeOf(new MulAddIO(17).in.bits)
    MulAdd17_1.io.in.bits := 0.U.asTypeOf(new MulAddIO(17).in.bits)
    MulAdd33_0.io.in.bits := 0.U.asTypeOf(new MulAddIO(33).in.bits)
    MulAdd65_0.io.in.bits := 0.U.asTypeOf(new MulAddIO(65).in.bits)

    def extender(SrcSigned:Bool,src:UInt,width:Int)={
        val a = WireInit(0.U(width.W))
        when(SrcSigned){
            a := SignExt(src,width)
        }.otherwise{
            a := ZeroExt(src,width)
        }
        a
    }
    def SrcSetter(width:Int,src:UInt,Xsrc:Bool)={
        var l = List(0.U)
        for(i <- 0 until XLEN/width){
            val SrcClip = WireInit(src(i * width + width - 1, i * width))
            when(Xsrc){
                if(i%2 == 0 && XLEN/width > 1){
                    SrcClip :=src((i+1) * width + width - 1, (i+1) * width)
                }else if(i >= 1){
                    SrcClip :=src((i-1) * width + width - 1, (i-1) * width)
                }
            }
            l = List.concat(List(SrcClip) ,l)
        }
        l.dropRight(1).reduce(Cat(_,_))
    }

    if(XLEN == 64){
        when(isMul_16(func)){
            val width = 16
            val realSrc2 = SrcSetter(16,src2,Xsrc(func))
            MulAdd17_0.io.in.bits.srcs(0) := extender(SrcSigned(func),src1(0*width+width-1,0*width),17)
            MulAdd65_0.io.in.bits.srcs(0) := extender(SrcSigned(func),src1(1*width+width-1,1*width),65)
            MulAdd17_0.io.in.bits.srcs(1) := extender(SrcSigned(func),realSrc2(0*width+width-1,0*width),17)
            MulAdd65_0.io.in.bits.srcs(1) := extender(SrcSigned(func),realSrc2(1*width+width-1,1*width),65)
            MulAdd17_0.io.in.bits.srcs(2) := extender(SrcSigned(func),src3(0*width+width-1,0*width),17)
            MulAdd65_0.io.in.bits.srcs(2) := extender(SrcSigned(func),src3(1*width+width-1,1*width),65)
        }
    }

    MulAdd17_0.io.in.valid := io.in.valid
    MulAdd17_1.io.in.valid := io.in.valid
    MulAdd33_0.io.in.valid := io.in.valid
    MulAdd65_0.io.in.valid := io.in.valid
    MulAdd17_0.io.out.ready:= io.out.ready
    MulAdd17_1.io.out.ready:= io.out.ready
    MulAdd33_0.io.out.ready:= io.out.ready
    MulAdd65_0.io.out.ready:= io.out.ready
    MulAdd17_0.io.in.bits.DecodeIn := io.in.bits
    MulAdd17_1.io.in.bits.DecodeIn := io.in.bits
    MulAdd33_0.io.in.bits.DecodeIn := io.in.bits
    MulAdd65_0.io.in.bits.DecodeIn := io.in.bits
    MulAdd17_0.io.flush := io.flush
    MulAdd17_1.io.flush := io.flush
    MulAdd33_0.io.flush := io.flush
    MulAdd65_0.io.flush := io.flush

    io.out.valid := MulAdd65_0.io.out.valid
    io.in.ready  := MulAdd65_0.io.in.ready

    io.out.bits.DecodeOut := MulAdd65_0.io.out.bits.DecodeOut
    io.out.bits.result := 0.U
    io.out.bits.DecodeOut.pext.OV := false.B
    io.FirstStageFire := MulAdd65_0.io.FirstStageFire

    if(XLEN == 64){
        val func_out = io.out.bits.DecodeOut.ctrl.fuOpType
        when(isMul_16(func_out)){
            io.out.bits.result := {
                var l = List(0.U)
                val width = 16
                val src1_out = io.out.bits.DecodeOut.data.src1
                val src2_out = SrcSetter(16,io.out.bits.DecodeOut.data.src2,Xsrc(func_out))
                for(i <- 0 to 1){
                    val tmp = WireInit(0.U(32.W))
                    when(Saturating(func_out) && src2_out(i*width+width-1,i*width) === Cat(1.U,Fill(15,0.U)) && src1_out(i*width+width-1,i*width) === Cat(1.U,Fill(15,0.U))){
                        io.out.bits.DecodeOut.pext.OV := true.B
                        tmp := Cat(Fill(17,0.U),Fill(15,1.U))
                    }.otherwise{
                        if(i == 0){
                            tmp := MulAdd17_0.io.out.bits.result(31,0)
                        }else{
                            tmp := MulAdd65_0.io.out.bits.result(31,0)
                        }
                    }
                    l = List.concat(List(tmp),l)
                }
                l.dropRight(1).reduce(Cat(_,_))
            }
        }
    }
}
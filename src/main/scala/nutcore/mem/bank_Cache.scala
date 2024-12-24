/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import utils._
import top.Settings

case class MBCacheConfig (
  ro: Boolean = false,
  name: String = "cache",
  userBits: Int = 0,
  idBits: Int = 0,
  cacheLevel: Int = 1,

  totalSize: Int = 32, // Kbytes
  ways: Int = 4,

  databankNum: Int =4
)

sealed trait MBHasCacheConst {
  implicit val cacheConfig: MBCacheConfig

  val PAddrBits: Int
  val XLEN: Int

  val cacheName = cacheConfig.name
  val userBits = cacheConfig.userBits
  val idBits = cacheConfig.idBits

  val ro = cacheConfig.ro
  val hasCoh = !ro
  val hasCohInt = (if (hasCoh) 1 else 0)
  val hasPrefetch = cacheName == "l2cache"

	val databankNum = cacheConfig.databankNum
  val BankBits = log2Up(databankNum)

  val cacheLevel = cacheConfig.cacheLevel
  val TotalSize = cacheConfig.totalSize
  val Ways = cacheConfig.ways
  val LineSize = XLEN // byte
  val LineBeats = LineSize / 8 //DATA WIDTH 64
  val Sets = TotalSize * 1024 / LineSize / Ways
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits = PAddrBits - OffsetBits - IndexBits

  // def debugon = false.B 
  def debugon(addr: UInt) = addr === "h00806ccd48".U 


  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W) //8
    val wordIndex = UInt((WordIndexBits - BankBits).W) //1
    val bankIndex = UInt(BankBits.W) //2
    val byteOffset = UInt((if (XLEN == 64) 3 else 2).W) //3
  }

  def BankHitVec(addr: UInt) : UInt = {
    VecInit((0 until databankNum).map{m => m.U === getbankIdx(addr)}).asUInt
  }

  def CacheMetaArrayReadBus() = new SRAMReadBus(new MBMetaBundle, set = Sets, way = Ways)
  def CacheDataArrayReadBus() = new SRAMReadBus(new MBDataBundle, set = Sets * LineBeats / databankNum, way = Ways)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new MBMetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new MBDataBundle, set = Sets * LineBeats / databankNum, way = Ways)

  def getMetaIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(addr.asTypeOf(addrBundle).index, addr.asTypeOf(addrBundle).wordIndex)
  def getbankIdx(addr: UInt) = addr.asTypeOf(addrBundle).bankIndex


  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed abstract class MBCacheBundle(implicit cacheConfig: MBCacheConfig) extends Bundle with HasNutCoreParameter with MBHasCacheConst
sealed abstract class MBCacheModule(implicit cacheConfig: MBCacheConfig) extends Module with HasNutCoreParameter with MBHasCacheConst with HasNutCoreLog

sealed class MBMetaBundle(implicit val cacheConfig: MBCacheConfig) extends MBCacheBundle {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
  val dirty = Output(Bool())

  def apply(tag: UInt, valid: Bool, dirty: Bool) = {
    this.tag := tag
    this.valid := valid
    this.dirty := dirty
    this
  }
}

sealed class MBDataBundle(implicit val cacheConfig: MBCacheConfig) extends MBCacheBundle {
  val data = Output(UInt(DataBits.W))

  def apply(data: UInt) = {
    this.data := data
    this
  }
}

sealed class MBStage1IO(implicit val cacheConfig: MBCacheConfig) extends MBCacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
}

class MBCacheIO(implicit val cacheConfig: MBCacheConfig) extends Bundle with HasNutCoreParameter with MBHasCacheConst {
  val in = Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits))
  val flush = Input(UInt(2.W))
  val out = new SimpleBusC
  val mmio = new SimpleBusUC
  val empty = Output(Bool())
}
trait MBHasCacheIO {
  implicit val cacheConfig: MBCacheConfig
  val io = IO(new MBCacheIO)
}

// meta read
sealed class MBCacheStage1(implicit val cacheConfig: MBCacheConfig) extends MBCacheModule {
  class MBCacheStage1IO extends Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val out = Decoupled(new MBStage1IO)
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = Vec(databankNum, CacheDataArrayReadBus())
  }
  val io = IO(new MBCacheStage1IO)

  if (ro) when (io.in.fire()) { assert(!io.in.bits.isWrite()) }


  // read meta array and data array
  val readBusValid = io.in.fire
  // val readBusValid = io.in.valid && io.out.ready
  io.metaReadBus.apply(valid = readBusValid, setIdx = getMetaIdx(io.in.bits.addr))
  for (w <- 0 until databankNum) {
    io.dataReadBus(w).apply(valid = readBusValid && (w.U === getbankIdx(io.in.bits.addr)), setIdx = getDataIdx(io.in.bits.addr))
  }


  io.out.bits.req := io.in.bits
  io.out.valid := io.in.valid && io.metaReadBus.req.ready && VecInit(io.dataReadBus.map(_.req.ready)).asUInt.andR  
  io.in.ready := io.out.ready && io.metaReadBus.req.ready && VecInit(io.dataReadBus.map(_.req.ready)).asUInt.andR
  // io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadBus.req.ready && VecInit(io.dataReadBus.map(_.req.ready)).asUInt.andR

  when(debugon(io.in.bits.addr) && io.in.fire()){
    Debug( "\n\n "+ cacheName +"S1 addr in: %x, user: %x id: %x\n", io.in.bits.addr, io.in.bits.user.getOrElse(0.U), io.in.bits.id.getOrElse(0.U))
    Debug("in.ready = %d, in.valid = %d,addr = %x, cmd = %x\n", io.in.ready, io.in.valid,io.in.bits.addr, io.in.bits.cmd)    
    for (i <- 0 until databankNum) {
      Debug("i = %d dataReadBus.req.valid = %d dataReadBus(i).req.setIdx = %x \n",i.asUInt, io.dataReadBus(i).req.valid,io.dataReadBus(i).req.bits.setIdx)
    }
    Debug("metaReadbus valid:%x setMetaIdx:%x \n",readBusValid,getMetaIdx(io.in.bits.addr))
    Debug("dataReadbus valid:%x setDataIdx:%x \n",readBusValid,getDataIdx(io.in.bits.addr))
  }
}


sealed class MBCacheStage2(implicit val cacheConfig: MBCacheConfig) extends MBCacheModule {
  class MBCacheStage2IO extends Bundle {
    val in = Flipped(Decoupled(new MBStage1IO))
    val out = Decoupled(new SimpleBusRespBundle(userBits = userBits, idBits = idBits))
    val metaReadResp = Flipped(Vec(Ways, new MBMetaBundle))
    val dataReadResp = Flipped(Vec(databankNum, Vec(Ways, new MBDataBundle)))

    val isFinish = Output(Bool())
    val flush = Input(Bool())
    val dataReadBus = Vec(databankNum, CacheDataArrayReadBus())
    val dataWriteBus = Vec(databankNum, CacheDataArrayWriteBus())
    val metaWriteBus = CacheMetaArrayWriteBus()

    val mem = new SimpleBusUC
    val mmio = new SimpleBusUC
    val cohResp = Decoupled(new SimpleBusRespBundle)

    // use to distinguish prefetch request and normal request
    val dataReadRespToL1 = Output(Bool())
  }
  val io = IO(new MBCacheStage2IO)
  //chack hit
  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)

  val metaWay = io.metaReadResp
  val tagHitVec = VecInit(metaWay.map(m => m.valid && (m.tag === addr.tag) && io.in.valid)).asUInt
  val hit = io.in.valid && tagHitVec.orR

  // val victimWaymask = 1.U 
  val victimWaymask = RegEnable(if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1".U,io.in.fire())

  val invalidVec = VecInit(metaWay.map(m => !m.valid)).asUInt
  val hasInvalidWay = invalidVec.orR
  val refillInvalidWaymask = Mux(invalidVec >= 8.U, "b1000".U,
    Mux(invalidVec >= 4.U, "b0100".U,
    Mux(invalidVec >= 2.U, "b0010".U, "b0001".U)))  

  val waymask = Mux(hit, tagHitVec, Mux(hasInvalidWay, refillInvalidWaymask, victimWaymask))
  val wordMask = Mux(!ro.B && req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))
  
  val bankHitVec = BankHitVec(req.addr)
  val hitBank = Mux1H(bankHitVec, io.dataReadResp)
  val dataRead = Mux1H(waymask, hitBank).data.asUInt
  dontTouch(dataRead)
  val dataMasked = MaskData(dataRead, req.wdata, wordMask)
  val dataHitWriteBus = Wire(Vec(databankNum, CacheDataArrayWriteBus()))
  when(PopCount(waymask) > 1.U){
    metaWay.map(m => Debug("[ERROR] metaWay %x metat %x reqt %x\n", m.valid, m.tag, addr.tag))
    io.metaReadResp.map(m => Debug("[ERROR] metaReadResp %x metat %x reqt %x\n", m.valid, m.tag, addr.tag))
  }
  assert(!(io.in.valid && PopCount(waymask) > 1.U))

  io.in.ready := !io.in.valid || io.out.fire()

  //write back
  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  val dataWriteArb = Seq.fill(databankNum)(Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2)))

  val miss = io.in.valid && !hit
  val mmio = io.in.valid && AddressSpace.isMMIO(req.addr)
  val probe = io.in.valid && hasCoh.B && req.isProbe()
  val hitReadBurst = hit && req.isReadBurst()
  val meta = Mux1H(waymask, metaWay)
  assert(!(mmio && hit), "MMIO request should not hit in cache")

  // this is ugly
  if (cacheName == "dcache") {
    // BoringUtils.addSource(mmio, "lsuMMIO")
  }

  val writeL2BeatCnt = Counter(LineBeats)
  when(io.out.fire() && (req.cmd === SimpleBusCmd.writeBurst || req.isWriteLast())) {
    writeL2BeatCnt.inc()
  }
  
  val hitWrite = hit && req.isWrite()
  // for (w <- 0 until databankNum) {
  // dataHitWriteBus(w).apply(
  //   data = Wire(new MBDataBundle).apply(MaskData(dataRead.asUInt, req.wdata, wordMask)),
  //   valid = hitWrite && w.U === addr.bankIndex, setIdx = Cat(addr.index, Mux(req.cmd === SimpleBusCmd.writeBurst || req.isWriteLast(), writeL2BeatCnt.value, addr.bankIndex)), waymask = waymask)
  // }

  for (w <- 0 until databankNum) {
    dataHitWriteBus(w).apply(
      data = Wire(new MBDataBundle).apply(dataMasked),
      valid = hitWrite && w.U === addr.bankIndex, setIdx = Cat(addr.index, addr.wordIndex), waymask = waymask)
  }

  val metaHitWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = hitWrite && !meta.dirty, setIdx = getMetaIdx(req.addr), waymask = waymask,
    data = Wire(new MBMetaBundle).apply(tag = meta.tag, valid = true.B, dirty = (!ro).B)
  )

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_mmioReq :: s_mmioResp :: s_wait_resp :: s_release :: Nil = Enum(9)
  val state = RegInit(s_idle)
  val needFlush = RegInit(false.B)

  when (io.flush && (state =/= s_idle)) { needFlush := true.B }
  when (io.out.fire() && needFlush) { needFlush := false.B }

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats) 
  val read_bankidx = readBeatCnt.value % 4.U
  val read_wordidx = (readBeatCnt.value / 4.U)(0).asUInt
  val write_bankidx = writeBeatCnt.value % 4.U
  val write_wordidx = (writeBeatCnt.value / 4.U)(0).asUInt
  val set_refill_wordidx=Mux(addr.wordIndex === 1.U,~read_wordidx,read_wordidx)


  val s2_idle :: s2_dataReadWait :: s2_dataOK :: Nil = Enum(3)
  val state2 = RegInit(s2_idle)

  for (w <- 0 until databankNum) {
  io.dataReadBus(w).apply(valid = (state === s_memWriteReq && w.U === write_bankidx || state === s_release && w.U === read_bankidx) && (state2 === s2_idle) ,
    setIdx = Cat(addr.index, Mux(state === s_release, set_refill_wordidx.asUInt, write_wordidx.asUInt)))
  }

  val dataWay = VecInit(Seq.fill(databankNum)(Reg(CacheDataArrayReadBus().resp.data)))
  for (w <- 0 until databankNum) {
    dataWay(w) := RegEnable(io.dataReadBus(w).resp.data, state2 === s2_dataReadWait)
  }

  val hitBankdataWay = Mux(state === s_release ,Mux1H(UIntToOH(read_bankidx.asUInt), dataWay  ),Mux1H(UIntToOH(write_bankidx.asUInt), dataWay  ))
  // val hitBankdataWay = Mux1H(UIntToOH(write_bankidx.asUInt), dataWay)
  val dataHitWay = Mux1H(waymask, hitBankdataWay).data

  val datareadfire = (0 until databankNum).map(i =>{io.dataReadBus(i).req.valid && io.dataReadBus(i).req.ready }).reduce(_ || _)


  switch (state2) {
    is (s2_idle) {       
      when (datareadfire) { state2 := s2_dataReadWait } }
    is (s2_dataReadWait) { state2 := s2_dataOK }
    is (s2_dataOK) { when (io.mem.req.fire() || io.cohResp.fire() || hitReadBurst && io.out.ready) { 
      state2 := s2_idle } }
  }

  // critical word first read
  val raddr = (if (XLEN == 64) Cat(req.addr(PAddrBits-1,3), 0.U(3.W))
                          else Cat(req.addr(PAddrBits-1,2), 0.U(2.W)))
  // dirty block addr
  val waddr = Cat(meta.tag, addr.index, 0.U(OffsetBits.W))
  val cmd = Mux(state === s_memReadReq, SimpleBusCmd.readBurst,
    Mux((writeBeatCnt.value === (LineBeats - 1).U), SimpleBusCmd.writeLast, SimpleBusCmd.writeBurst))
  val mem_addr = Mux(state === s_memReadReq, raddr, waddr)
  io.mem.req.bits.apply(addr = mem_addr,
    cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U),
    wdata = dataHitWay, wmask = Fill(DataBytes, 1.U))

  io.mem.resp.ready := true.B
  io.mem.req.valid := (state === s_memReadReq) || ((state === s_memWriteReq) && (state2 === s2_dataOK))

  // mmio
  io.mmio.req.bits := req
  io.mmio.resp.ready := true.B
  io.mmio.req.valid := (state === s_mmioReq)

  val afterFirstRead = RegInit(false.B)
  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire())
  val readingFirst = !afterFirstRead && io.mem.resp.fire() && (state === s_memReadResp)
  val inRdataRegDemand = RegEnable(Mux(mmio, io.mmio.resp.bits.rdata, io.mem.resp.bits.rdata),
                                   Mux(mmio, state === s_mmioResp, readingFirst))

  // probe
  io.cohResp.valid := ((state === s_idle) && probe) ||
                      ((state === s_release) && (state2 === s2_dataOK))
  io.cohResp.bits.rdata := dataHitWay
  val releaseLast = Counter(state === s_release && io.cohResp.fire(), LineBeats)._2
  io.cohResp.bits.cmd := Mux(state === s_release, Mux(releaseLast, SimpleBusCmd.readLast, 0.U),
    Mux(hit, SimpleBusCmd.probeHit, SimpleBusCmd.probeMiss))

  val respToL1Fire = hitReadBurst && io.out.ready && state2 === s2_dataOK
  val respToL1Last = Counter((state === s_idle || state === s_release && state2 === s2_dataOK) && hitReadBurst && io.out.ready, LineBeats)._2


  switch (state) {
    is (s_idle) {
      afterFirstRead := false.B
      alreadyOutFire := false.B
      when (probe) {
        when (io.cohResp.fire()) {
          state := Mux(hit, s_release, s_idle)
          readBeatCnt.value := addr.bankIndex
        }
      } .elsewhen (hitReadBurst && io.out.ready) {
        state := s_release
        readBeatCnt.value := Mux(addr.bankIndex === (LineBeats - 1).U, 0.U, (addr.bankIndex + 1.U))
      } .elsewhen ((miss || mmio) && !io.flush) {
        state := Mux(mmio, s_mmioReq, Mux(!ro.B && meta.dirty, s_memWriteReq, s_memReadReq))
      }
    }

    is (s_mmioReq) { when (io.mmio.req.fire()) { state := s_mmioResp } }
    is (s_mmioResp) { when (io.mmio.resp.fire()) { state := s_wait_resp } }

    is (s_release) {
      when (io.cohResp.fire() || respToL1Fire) { readBeatCnt.inc() }
      when (probe && io.cohResp.fire() && releaseLast || respToL1Fire && respToL1Last) { state := s_idle }
    }

    is (s_memReadReq) { when (io.mem.req.fire()) {
      state := s_memReadResp
      // readBeatCnt.value := addr.wordIndex
      readBeatCnt.value := addr.bankIndex
    }}

    is (s_memReadResp) {
      when (io.mem.resp.fire()) {
        afterFirstRead := true.B
        readBeatCnt.inc()
        when (req.cmd === SimpleBusCmd.writeBurst) { writeL2BeatCnt.value := 0.U }
        when (io.mem.resp.bits.isReadLast()) { state := s_wait_resp }
      }
    }

    is (s_memWriteReq) {
      when (io.mem.req.fire()) { writeBeatCnt.inc() }
      when (io.mem.req.bits.isWriteLast() && io.mem.req.fire()) { state := s_memWriteResp }
    }

    is (s_memWriteResp) { when (io.mem.resp.fire()) { state := s_memReadReq } }
    is (s_wait_resp) { when (io.out.fire() || needFlush || alreadyOutFire) { state := s_idle } }
  }


  val dataRefill = MaskData(io.mem.resp.bits.rdata, req.wdata, Mux(readingFirst, wordMask, 0.U(DataBits.W)))
  val dataRefillWriteBus= Wire(Vec(databankNum, CacheDataArrayWriteBus()))
  for (w <- 0 until databankNum){
  dataRefillWriteBus(w).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && w.U === read_bankidx,
     setIdx = Cat(addr.index, set_refill_wordidx.asUInt),
    //  setIdx = Cat(addr.index, read_wordidx),
    // valid = (state === s_memReadResp) && io.mem.resp.fire() && w.U === addr.bankIndex , setIdx = Cat(addr.index, readBeatCnt.value),
    data = Wire(new MBDataBundle).apply(dataRefill), waymask = waymask)
  }

  for (w <- 0 until databankNum) {
  dataWriteArb(w).io.in(0) <> dataHitWriteBus(w).req
  dataWriteArb(w).io.in(1) <> dataRefillWriteBus(w).req
  io.dataWriteBus(w).req <> dataWriteArb(w).io.out
  }

  val metaRefillWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && io.mem.resp.bits.isReadLast(),
    data = Wire(new MBMetaBundle).apply(valid = true.B, tag = addr.tag, dirty = !ro.B && req.isWrite()),
    setIdx = getMetaIdx(req.addr), waymask = waymask
  )

  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  if (cacheLevel == 2) {
    when ((state === s_memReadResp) && io.mem.resp.fire() && req.isReadBurst()) {
      // readBurst request miss
      io.out.bits.rdata := dataRefill
      io.out.bits.cmd := Mux(io.mem.resp.bits.isReadLast(), SimpleBusCmd.readLast, SimpleBusCmd.readBurst)
    }.elsewhen (req.isWriteLast() || req.cmd === SimpleBusCmd.writeBurst) {
      // writeBurst/writeLast request, no matter hit or miss
      io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
      io.out.bits.cmd := DontCare
    }.elsewhen (hitReadBurst && state === s_release) {
      // readBurst request hit
      io.out.bits.rdata := dataHitWay
      io.out.bits.cmd := Mux(respToL1Last, SimpleBusCmd.readLast, SimpleBusCmd.readBurst)
    }.otherwise {
      io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
      io.out.bits.cmd := req.cmd
    }
  } else {
    io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
    io.out.bits.cmd := Mux(req.isRead(), SimpleBusCmd.readLast, Mux(req.isWrite(), SimpleBusCmd.writeResp, DontCare))//DontCare, added by lemover
  }
  io.out.bits.user.zip(req.user).map { case (o,i) => o := i }
  io.out.bits.id.zip(req.id).map { case (o,i) => o := i }

  io.out.valid := io.in.valid && Mux(req.isBurst() && (cacheLevel == 2).B,
    Mux(req.isWrite() && (hit || !hit && state === s_wait_resp), true.B, (state === s_memReadResp && io.mem.resp.fire() && req.cmd === SimpleBusCmd.readBurst)) || (respToL1Fire && respToL1Last && state === s_release),
    Mux(probe, false.B, Mux(hit, true.B, Mux(req.isWrite() || mmio, state === s_wait_resp, afterFirstRead && !alreadyOutFire)))
  )

  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request really ends.
  io.isFinish := Mux(probe, io.cohResp.fire() && Mux(miss, state === s_idle, (state === s_release) && releaseLast),
    Mux(hit || req.isWrite(), io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire))
  )

  io.mem.req.bits.vector := DontCare
  io.out.bits.vector := DontCare
  io.cohResp.bits.vector := DontCare

  io.in.ready := io.out.ready && (state === s_idle && !hitReadBurst) && !miss && !probe
  io.dataReadRespToL1 := hitReadBurst && (state === s_idle && io.out.ready || state === s_release && state2 === s2_dataOK)
  // io.in.ready := io.out.ready && acquireReady && releaseReady && !miss
 
  assert(!(!ro.B && io.flush), "only allow to flush icache")
  for (w <- 0 until databankNum ) {
    assert(!(metaHitWriteBus.req.valid && metaRefillWriteBus.req.valid))
    assert(!(dataHitWriteBus(w).req.valid && dataRefillWriteBus(w).req.valid))
  }

  when(debugon(req.addr) ||  (debugon(io.mem.req.bits.addr) && io.mem.req.valid) || ((getMetaIdx(req.addr) === getMetaIdx("h807331e8".U)) && metaHitWriteBus.req.fire() && meta.tag === "h807331e8".U.asTypeOf(addrBundle).tag ))
  {
    //Debug("\n S3 !! addr= %x metaidx=%x dataidx=%x tag= %x index= %x wordIndex= %x \n",req.addr, getMetaIdx(req.addr), getDataIdx(req.addr), req.addr.asTypeOf(addrBundle).tag,req.addr.asTypeOf(addrBundle).index,req.addr.asTypeOf(addrBundle).wordIndex)
    //Debug(" metaread idx %x addr.tag %x  metas valid dirty:tag %x%x:%x %x%x:%x %x%x:%x %x%x:%x \n", getMetaIdx(req.addr),req.addr.asTypeOf(addrBundle).tag, metaWay(3).valid, metaWay(3).dirty, metaWay(3).tag, metaWay(2).valid, metaWay(2).dirty, metaWay(2).tag, metaWay(1).valid, metaWay(1).dirty, metaWay(1).tag, metaWay(0).valid, metaWay(0).dirty, metaWay(0).tag)
    //Debug("waymask = %b, victimWaymask = %b \n", waymask.asUInt ,victimWaymask)
    //Debug("hit = %d, hitvec = %b \n",hit,tagHitVec)
    //Debug( io.metaWriteBus.req.fire(), "%d: [" + cacheName + " S3]: metawrite idx %x wmask %b meta %x%x:%x\n", GTimer(), io.metaWriteBus.req.bits.setIdx, io.metaWriteBus.req.bits.waymask.get, io.metaWriteBus.req.bits.data.valid, io.metaWriteBus.req.bits.data.dirty, io.metaWriteBus.req.bits.data.tag)
    //Debug(" ioin in.ready = %d, in.valid = %d, hit = %x, state = %d, addr = %x cmd:%d probe:%d isFinish:%d\n", io.in.ready, io.in.valid, hit, state, req.addr, req.cmd, probe, io.isFinish)
    //Debug(" ioout out.valid:%d rdata:%x cmd:%d user:%x id:%x \n", io.out.valid, io.out.bits.rdata, io.out.bits.cmd, io.out.bits.user.getOrElse(0.U), io.out.bits.id.getOrElse(0.U))
  for (w <- 0 until databankNum ) {
    //Debug(io.dataWriteBus(w).req.fire()," DHW: (%d, %d), data:%x setIdx:%x MHW:(%d, %d)\n", dataHitWriteBus(w).req.valid, dataHitWriteBus(w).req.ready, dataHitWriteBus(w).req.bits.data.asUInt, dataHitWriteBus(w).req.bits.setIdx, metaHitWriteBus.req.valid, metaHitWriteBus.req.ready)
    //Debug(io.dataWriteBus(w).req.fire(), "[WB] waymask: %b data:%x setIdx:%x\n", io.dataWriteBus(w).req.bits.waymask.get.asUInt, io.dataWriteBus(w).req.bits.data.asUInt, io.dataWriteBus(w).req.bits.setIdx)
    //Debug(io.dataWriteBus(w).req.fire(), "[WB] waymask: %b data:%x setIdx:%x\n", io.dataWriteBus(w).req.bits.waymask.get.asUInt, io.dataWriteBus(w).req.bits.data.asUInt, io.dataWriteBus(w).req.bits.setIdx)
  }
  //Debug((state === s_memWriteReq) && io.mem.req.fire(), "[COUTW] cnt %x addr %x data %x cmd %x size %x wmask %x tag %x idx %x waymask %b \n", writeBeatCnt.value, io.mem.req.bits.addr, io.mem.req.bits.wdata, io.mem.req.bits.cmd, io.mem.req.bits.size, io.mem.req.bits.wmask, addr.tag, getMetaIdx(req.addr), waymask)
  //Debug((state === s_memReadReq) && io.mem.req.fire(), "[COUTR] addr %x tag %x idx %x waymask %b \n", io.mem.req.bits.addr, addr.tag, getMetaIdx(req.addr), waymask)
  //Debug((state === s_memReadResp) && io.mem.resp.fire(), "[COUTR] cnt %x data %x tag %x idx %x waymask %b \n", readBeatCnt.value, io.mem.resp.bits.rdata, addr.tag, getMetaIdx(req.addr), waymask)
  }

}

class MBCache(implicit val cacheConfig: MBCacheConfig) extends MBCacheModule with MBHasCacheIO {
  // cpu pipeline
  val s1 = Module(new MBCacheStage1)
  val s2 = Module(new MBCacheStage2)

  val metaArray = Module(new DataSRAMTemplateWithArbiter(nRead = 1, new MBMetaBundle, set = Sets, way = Ways, shouldReset = true))
  //SRAMTemplateWithArbiter
  val dataArray = Array.fill(databankNum) {
    Module(new DataSRAMTemplateWithArbiter(
      nRead = 2,
      new MBDataBundle,
      set = Sets * LineBeats / databankNum,
      way = Ways
    ))
  }

  if (cacheName == "icache") {
    // flush icache when executing fence.i
    val flushICache = WireInit(false.B)
    BoringUtils.addSink(flushICache, "MOUFlushICache")
    metaArray.reset := reset.asBool || flushICache
  }

  val arb = Module(new Arbiter(new SimpleBusReqBundle(userBits = userBits, idBits = idBits), hasCohInt + 1))
  arb.io.in(hasCohInt + 0) <> io.in.req

  s1.io.in <> arb.io.out
  PipelineConnect(s1.io.out, s2.io.in, s2.io.isFinish, io.flush(1))
  io.in.resp <> s2.io.out
  s2.io.flush := io.flush(1)
  io.out.mem <> s2.io.mem
  io.mmio <> s2.io.mmio
  io.empty := !s2.io.in.valid

  io.in.resp.valid := Mux(s2.io.out.valid && s2.io.out.bits.isPrefetch(), false.B, s2.io.out.valid || s2.io.dataReadRespToL1)

  if (hasCoh) {
    val cohReq = io.out.coh.req.bits
    // coh does not have user signal, any better code?
    val coh = Wire(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))
    coh.apply(addr = cohReq.addr, cmd = cohReq.cmd, size = cohReq.size, wdata = cohReq.wdata, wmask = cohReq.wmask)
    arb.io.in(0).bits := coh
    arb.io.in(0).valid := io.out.coh.req.valid
    io.out.coh.req.ready := arb.io.in(0).ready
    io.out.coh.resp <> s2.io.cohResp
    coh.vector := DontCare
  } else {
    io.out.coh.req.ready := true.B
    io.out.coh.resp := DontCare
    io.out.coh.resp.valid := false.B
    s2.io.cohResp.ready := true.B
  }

  metaArray.io.r(0) <> s1.io.metaReadBus
  for (w <- 0 until databankNum) {
  dataArray(w).io.r(0) <> s1.io.dataReadBus(w)
  dataArray(w).io.r(1) <> s2.io.dataReadBus(w)
  dataArray(w).io.w <> s2.io.dataWriteBus(w)
  }
  for (w <- 0 until databankNum) {
  s2.io.dataReadResp(w) := s1.io.dataReadBus(w).resp.data
  }
  metaArray.io.w <> s2.io.metaWriteBus

  s2.io.metaReadResp := s1.io.metaReadBus.resp.data

}


class MBCache_fake(implicit val cacheConfig: MBCacheConfig) extends MBCacheModule with MBHasCacheIO {
  val s_idle :: s_memReq :: s_memResp :: s_mmioReq :: s_mmioResp :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)

  val ismmio = AddressSpace.isMMIO(io.in.req.bits.addr)
  val ismmioRec = RegEnable(ismmio, io.in.req.fire())
  if (cacheConfig.name == "dcache") {
    BoringUtils.addSource(ismmio, "lsuMMIO")
  }

  val needFlush = RegInit(false.B)
  when (io.flush(0) && (state =/= s_idle)) { needFlush := true.B }
  when (state === s_idle && needFlush) { needFlush := false.B }

  val alreadyOutFire = RegEnable(true.B, init = false.B, io.in.resp.fire())

  switch (state) {
    is (s_idle) {
      alreadyOutFire := false.B
      when (io.in.req.fire() && !io.flush(0)) { state := Mux(ismmio, s_mmioReq, s_memReq) }
    }
    is (s_memReq) {
      when (io.out.mem.req.fire()) { state := s_memResp }
    }
    is (s_memResp) {
      when (io.out.mem.resp.fire()) { state := s_wait_resp }
    }
    is (s_mmioReq) {
      when (io.mmio.req.fire()) { state := s_mmioResp }
    }
    is (s_mmioResp) {
      when (io.mmio.resp.fire() || alreadyOutFire) { state := s_wait_resp }
    }
    is (s_wait_resp) {
      when (io.in.resp.fire() || needFlush || alreadyOutFire) { state := s_idle }
    }
  }

  val reqaddr = RegEnable(io.in.req.bits.addr, io.in.req.fire())
  val cmd = RegEnable(io.in.req.bits.cmd, io.in.req.fire())
  val size = RegEnable(io.in.req.bits.size, io.in.req.fire())
  val wdata = RegEnable(io.in.req.bits.wdata, io.in.req.fire())
  val wmask = RegEnable(io.in.req.bits.wmask, io.in.req.fire())

  io.in.req.ready := (state === s_idle)
  io.in.resp.valid := (state === s_wait_resp) && (!needFlush)

  val mmiordata = RegEnable(io.mmio.resp.bits.rdata, io.mmio.resp.fire())
  val mmiocmd = RegEnable(io.mmio.resp.bits.cmd, io.mmio.resp.fire())
  val memrdata = RegEnable(io.out.mem.resp.bits.rdata, io.out.mem.resp.fire())
  val memcmd = RegEnable(io.out.mem.resp.bits.cmd, io.out.mem.resp.fire())

  io.in.resp.bits.rdata := Mux(ismmioRec, mmiordata, memrdata)
  io.in.resp.bits.cmd := Mux(ismmioRec, mmiocmd, memcmd)

  val memuser = RegEnable(io.in.req.bits.user.getOrElse(0.U), io.in.req.fire())
  io.in.resp.bits.user.zip(if (userBits > 0) Some(memuser) else None).map { case (o,i) => o := i }

  io.out.mem.req.bits.apply(addr = reqaddr,
    cmd = cmd, size = size,
    wdata = wdata, wmask = wmask)
  io.out.mem.req.valid := (state === s_memReq)
  io.out.mem.resp.ready := true.B
  
  io.mmio.req.bits.apply(addr = reqaddr,
    cmd = cmd, size = size,
    wdata = wdata, wmask = wmask)
  io.mmio.req.valid := (state === s_mmioReq)
  io.mmio.resp.ready := true.B

  io.empty := false.B
  io.out.coh := DontCare

  // Debug(io.in.req.fire(), p"in.req: ${io.in.req.bits}\n")
  // Debug(io.out.mem.req.fire(), p"out.mem.req: ${io.out.mem.req.bits}\n")
  // Debug(io.out.mem.resp.fire(), p"out.mem.resp: ${io.out.mem.resp.bits}\n")
  // Debug(io.in.resp.fire(), p"in.resp: ${io.in.resp.bits}\n")
}

class MBCache_dummy(implicit val cacheConfig: MBCacheConfig) extends MBCacheModule with MBHasCacheIO {

  val needFlush = RegInit(false.B)
  when (io.flush(0)) {
    needFlush := true.B
  }
  when (io.in.req.fire() && !io.flush(0)) {
    needFlush := false.B
  }

  io.in.req.ready := io.out.mem.req.ready
  io.in.resp.valid := (io.out.mem.resp.valid && !needFlush) || io.flush(0)

  io.in.resp.bits.rdata := io.out.mem.resp.bits.rdata
  io.in.resp.bits.cmd := io.out.mem.resp.bits.cmd
  val memuser = RegEnable(io.in.req.bits.user.getOrElse(0.U), io.in.req.fire())
  io.in.resp.bits.user.zip(if (userBits > 0) Some(memuser) else None).map { case (o,i) => o := i }

  io.out.mem.req.bits.apply( 
    addr = io.in.req.bits.addr,
    cmd = io.in.req.bits.cmd,
    size = io.in.req.bits.size,
    wdata = io.in.req.bits.wdata,
    wmask = io.in.req.bits.wmask
  )
  io.out.mem.req.valid := io.in.req.valid
  io.out.mem.resp.ready := io.in.resp.ready

  io.empty := false.B
  io.mmio := DontCare
  io.out.coh := DontCare
}

object MBCache {
  def apply(in: SimpleBusUC, mmio: Seq[SimpleBusUC], flush: UInt, empty: Bool, enable: Boolean = true)(implicit cacheConfig: MBCacheConfig) = {
    val cache = if (enable) Module(new MBCache) 
                else (if (Settings.get("IsRV32")) 
                        (if (cacheConfig.name == "dcache") Module(new MBCache_fake) else Module(new MBCache_dummy)) 
                      else 
                        (Module(new MBCache_fake)))
    cache.io.flush := flush
    cache.io.in <> in
    mmio(0) <> cache.io.mmio
    empty := cache.io.empty
    cache.io.out
  }
}


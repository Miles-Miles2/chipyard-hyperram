package hyperram


import chisel3._
import chisel3.util._
import chisel3.experimental.{ExtModule, ChiselEnum, IntParam, BaseModule, Analog}
import cmu.external.axi._
import cmu.mem.{SPMemBundle, SPMemBBox}
import cmu.util.{IOBUF}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.axi4._

case object HyperRAMKey extends Field[Option[HyperRamParams]](None)

class HyperRAMParams(
  addr: BigInt = 0x20000000, // claude, something to do with how the hyperram controller masks the axi address to be turned into the chips address
  size: BigInt = 1<<24, // using 128 Mb chip
  hyperRamAddrBits: int = 24, // using a 128 Mb chip so 24 addr bits 
  hyperBusWidth: int = 8 //we're using a hyperram ic with 8 data pins
)

class HyperRAMFront(val params: HyperRamParams, val beatBytes: Int /*from sbus*/)(implicit p: Parameters) extends ClockSinkDomain {
  val dev = SimpleDevice("hyperram", "ucb-bar,hyperram") // <- no idea

  val node = AXI4SlaveNode(
    Seq(AXI4SlavePortParameters(
        slaves = Seq(
          AXI4SlaveParameters(
            address = AddressSet(params.addr, params.size-1)
            resources = dev.reg
            regionType = RegionType.UNCACHED
            executable = true
            supportsWrite = TransferSizes(1, beatBytes) // 1: assumumes you can do partial transfers??
            supportsRead = TransferSizes(1, beatBytes)
            interleavedID = Some(0) // ?
            )
          )
        beatBytes = beatBytes
        )
      )
    )

  // < claude, investigate further - how properly punch out IO?
  val ioNode  = BundleBridgeSource(() => new HyperBusIO(params.hyperBusWidth))
  val hbusPin = InModuleBody { ioNode.makeIO() }
  // >
  lazy val module = new AXIHyperRamImpl

  class AXIHyperRamImpl extends impl {
    val (in, edge) = node.in(0)

    val controller = Module(
      new AXIHyperRAM(edge.bundle.dataBits, params.hyperRamAddrBits, params.hyperBusWidth, edge.bundle.idBits))
    
    val axi = controller.io.axi   // short alias, so the lines below stay readable

    // -- Claude --
    // ---- AR (read address): master -> slave ----
    axi.ar.valid      := in.ar.valid
    in.ar.ready       := axi.ar.ready
    axi.ar.bits.id    := in.ar.bits.id
    axi.ar.bits.addr  := in.ar.bits.addr     // narrower := wider is OK, zero-extends to 32
    axi.ar.bits.len   := in.ar.bits.len
    axi.ar.bits.size  := in.ar.bits.size
    axi.ar.bits.burst := in.ar.bits.burst
    axi.ar.bits.lock  := in.ar.bits.lock
    axi.ar.bits.cache := in.ar.bits.cache
    axi.ar.bits.prot  := in.ar.bits.prot
    axi.ar.bits.qos   := in.ar.bits.qos

    // ---- AW (write address): master -> slave ----
    axi.aw.valid      := in.aw.valid
    in.aw.ready       := axi.aw.ready
    axi.aw.bits.id    := in.aw.bits.id
    axi.aw.bits.addr  := in.aw.bits.addr
    axi.aw.bits.len   := in.aw.bits.len
    axi.aw.bits.size  := in.aw.bits.size
    axi.aw.bits.burst := in.aw.bits.burst
    axi.aw.bits.lock  := in.aw.bits.lock
    axi.aw.bits.cache := in.aw.bits.cache
    axi.aw.bits.prot  := in.aw.bits.prot
    axi.aw.bits.qos   := in.aw.bits.qos

    // ---- W (write data): master -> slave ----
    axi.w.valid       := in.w.valid
    in.w.ready        := axi.w.ready
    axi.w.bits.data   := in.w.bits.data
    axi.w.bits.strb   := in.w.bits.strb
    axi.w.bits.last   := in.w.bits.last

    // ---- B (write response): slave -> master ----
    in.b.valid        := axi.b.valid
    axi.b.ready       := in.b.ready
    in.b.bits.id      := axi.b.bits.id
    in.b.bits.resp    := axi.b.bits.resp

    // ---- R (read data): slave -> master ----
    in.r.valid        := axi.r.valid
    axi.r.ready       := in.r.ready
    in.r.bits.id      := axi.r.bits.id
    in.r.bits.data    := axi.r.bits.data
    in.r.bits.resp    := axi.r.bits.resp
    in.r.bits.last    := axi.r.bits.last

    // ---- Controller config: tie off for now ---- //need to look into how to handle control registers for chip, need separate system to connect up to pbus
    controller.io.ctrl.reg.valid := false.B   // never write a HyperRAM register
    controller.io.ctrl.reg.bits  := 0.U
    controller.io.ctrl.config.wrLatency0     := 6.U   // PLACEHOLDERS: take from your chip's datasheet
    controller.io.ctrl.config.wrLatency1     := 12.U  // (they depend on the latency count in CR0
    controller.io.ctrl.config.rwrLatency     := 6.U   //  and on the clock frequency)
    controller.io.ctrl.config.writeQFullWait := false.B

    // ---- HyperBus pins: hand to the outside world (see the note below) ----
    ioNode.out(0)._1 <> controller.io.hbus // punches out physical hyperam pins
    //end claude
  }
}

// mixin
trait CanHavePeripheryHyperRAM { this: BaseSubsystem => 
  sbus = locateTLBusWrapper(SBUS)

  val hyperram = p(HyperRAMKey) match {
    case Some(hyperRamParams) => {
      val front = LazyModule(new HyperRAMFront(hyperRamParams, sbus.beatBytes))

      sbus.coupleTo("hyperram-port") {
        front.node := AXI4Buffer() := TLtoAXI4() := TLWidthWidget(sbus.beatBytes) := TLFragmenter(sbus.beatBytes, sbus.blockBytes, holdFirstDeny = true) := _
      }

      Some(front)
    }
    case None => None
  }
}

//config
class WithHyperRAM(val addr: BigInt = 0x20000000) extends Config((site, here up) => {
  case HyperRAMKey => Some(HyperRAMParams(addr = addr))
})
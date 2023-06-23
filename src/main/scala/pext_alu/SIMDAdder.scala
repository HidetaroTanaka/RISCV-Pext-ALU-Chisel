package pext_alu

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import common.Functions

class SIMDAdder(xprlen: Int) extends Module {
  /**
   * UIntを8bitSIMDベクタに変換する
   * divideUIntToSimd8(0x0123456789ABCDEF) => Vec(0x01, 0x23, 0x34, ..., 0xEF)
   *
   * @param uint 元の値（64bitまたは32bit）
   * @return uintを下位から8bitごとに分割してVec[UInt]にしたもの
   */
  def divideUIntToSimd8(uint: UInt): Vec[UInt] = {
    val divided = uint.asBools.grouped(8).map(x => Cat(x.reverse)).toSeq.reverse
    VecInit(divided)
  }
  def saturate(uint: UInt, signed: Bool, addsub: Bool): MixedVec[UInt] = {
    val width = uint.getWidth - 1
    val signed_max = Functions.signed_maximum(width)
    val signed_min = Functions.signed_minimum(width)
    val unsigned_max = Functions.unsigned_maximum(width)
    val unsigned_min = Functions.unsigned_minimum(width)

    MuxCase(MixedVecInit(Seq(false.B.asUInt, uint(width-1, 0))), Seq(
      // KADD, KSUB
      (signed && (uint.asSInt > signed_max.asSInt)) -> MixedVecInit(Seq(true.B.asUInt, signed_max)),
      // KADD, KSUB
      (signed && (uint.asSInt < signed_min.asSInt)) -> MixedVecInit(Seq(true.B.asUInt, signed_min)),
      // UKADD
      (!signed && !addsub && (uint > unsigned_max)) -> MixedVecInit(Seq(true.B.asUInt, unsigned_max)),
      // UKSUB
      (!signed && addsub && (uint.asSInt < unsigned_min.asSInt)) -> MixedVecInit(Seq(true.B.asUInt, unsigned_min)),
    ))
  }
  def halving(uint: UInt): UInt = {
    uint(uint.getWidth-1, 1)
  }

  val io = IO(new Bundle {
    val rs1_value = Input(UInt(xprlen.W))
    val rs2_value = Input(UInt(xprlen.W))
    val elen = Input(UInt(2.W)) // 00 -> 8bit, 01 -> 16bit, 10 -> 32bit
    /**
     * false.B -> add
     *
     * true.B -> sub
     */
    val addsub = Input(Bool())
    val signed = Input(Bool())
    val saturating = Input(Bool())
    val halving = Input(Bool())
    val out = Output(UInt(xprlen.W))
    val overflow = Output(Bool())

    val debug_e8_rs1_vec = Output(UInt(xprlen.W))
    val debug_e8_rs2_vec = Output(UInt(xprlen.W))
  })

  assume(!(io.saturating && io.halving), "Error: Saturating and halving flag both true.")

  val e8_simd_len = xprlen / 8
  val e16_simd_len = xprlen / 16
  val e32_simd_len = xprlen / 32
  val e8_rs1_vec = divideUIntToSimd8(io.rs1_value)
  val e8_rs2_vec = divideUIntToSimd8(io.rs2_value)
  io.debug_e8_rs1_vec := Cat(e8_rs1_vec)
  io.debug_e8_rs2_vec := Cat(e8_rs2_vec)

  val rawResult_e8_vec = e8_rs1_vec.zip(e8_rs2_vec).map(x => {
    val (rs1, rs2) = x
    val rs1_ext = Functions.ext1(rs1, io.signed)
    val rs2_ext = Functions.ext1(rs2, io.signed)
    Mux(io.addsub, rs1_ext - rs2_ext, rs1_ext + rs2_ext)
  })

  val e8_out_vec = rawResult_e8_vec.map(x => MuxCase(MixedVecInit(Seq(false.B.asUInt, x(x.getWidth - 2, 0))), Seq(
    io.saturating -> saturate(x, io.signed, io.addsub),
    io.halving -> MixedVecInit(Seq(false.B.asUInt, halving(x)))
  )))

  /*
  val e8_out_vec: Seq[MixedVec[UInt]] = e8_rs1_vec.zip(e8_rs2_vec).map(x => {
    val (rs1, rs2) = x
    val rs1_ext = Functions.ext1(rs1, io.signed)
    val rs2_ext = Functions.ext1(rs2, io.signed)
    val rawResult = Mux(io.addsub, rs1_ext - rs2_ext, rs1_ext + rs2_ext)
    MuxCase(MixedVecInit(Seq(false.B.asUInt, rawResult(rawResult.getWidth-2, 0))), Seq(
      io.saturating -> saturate(rawResult, io.signed),
      io.halving -> MixedVecInit(Seq(false.B.asUInt, halving(rawResult)))
    ))
  })
   */

  io.out := Cat(e8_out_vec.map(_(1)))
  io.overflow := e8_out_vec.map(_(0).asBool).reduce(_ || _)
}

object SIMDAdder extends App {
  def apply(xprlen: Int): SIMDAdder = if (xprlen == 32 || xprlen == 64) new SIMDAdder(xprlen) else throw new Exception("xprlen is not 32 or 64\n")
  (new ChiselStage).emitSystemVerilog(apply(64))
}
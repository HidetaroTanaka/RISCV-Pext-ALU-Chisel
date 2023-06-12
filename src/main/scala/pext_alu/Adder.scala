package pext_alu

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

class Adder(xprlen: Int) extends Module {
  val io = IO(new Bundle {
    val rs1_value = Input(UInt(xprlen.W))
    val rs2_value = Input(UInt(xprlen.W))
    val elen = Input(UInt(2.W)) // 00 -> 8bit, 01 -> 16bit, 10 -> 32bit
    val out = Output(UInt(xprlen.W))
  })

  val e8_simd_len = xprlen / 8
  val e16_simd_len = xprlen / 16
  val e32_simd_len = xprlen / 32
  val e8_rs1_vec = Wire(Vec(e8_simd_len, UInt(8.W)))
  val e8_rs2_vec = Wire(Vec(e8_simd_len, UInt(8.W)))
  val e8_out_with_carry_vec = Wire(Vec(e8_simd_len, UInt(9.W)))

  // 0x0123_4567_89AB_CDEF => {0x01, 0x23, ..., 0xEF}
  for ((d, i) <- e8_rs1_vec.zipWithIndex) {
    d := io.rs1_value((8-i) * 8 - 1, (7-i) * 8)
  }
  for ((d, i) <- e8_rs2_vec.zipWithIndex) {
    d := io.rs2_value((8-i) * 8 - 1, (7-i) * 8)
  }

  // No readability but looks cool
  // e8_rs1_vec.zipWithIndex.foreach(e => e._1 := io.rs1_value((8-e._2) * 8 - 1, (7-e._2) * 8))
  // e8_rs2_vec.zipWithIndex.foreach(e => e._1 := io.rs2_value((8-e._2) * 8 - 1, (7-e._2) * 8))

  val e8 = (io.elen === "b00".U)
  val e16 = (io.elen === "b01".U)

  for ((d, i) <- e8_out_with_carry_vec.zipWithIndex) {
    d := Cat(false.B, e8_rs1_vec(i)) + Cat(false.B, e8_rs2_vec(i))
  }

  val e16_out_with_carry_vec = Wire(Vec(e16_simd_len, UInt(17.W)))
  for((d,i) <- e16_out_with_carry_vec.zipWithIndex) {
    d := Cat(e8_out_with_carry_vec(i*2+1) + e8_out_with_carry_vec(i*2)(8), e8_out_with_carry_vec(i*2)(7,0))
  }

  val e32_out_with_carry_vec = Wire(Vec(e32_simd_len, UInt(33.W)))
  for((d,i) <- e32_out_with_carry_vec.zipWithIndex) {
    d := Cat(e16_out_with_carry_vec(i*2+1) + e16_out_with_carry_vec(i*2)(16), e16_out_with_carry_vec(i*2)(15,0))
  }

  when(e8) {
    io.out := Cat(e8_out_with_carry_vec.map(i => i(7,0)))
  } .elsewhen(e16) {
    io.out := Cat(e16_out_with_carry_vec.map(i => i(15,0)))
  } .otherwise {
    io.out := (if(xprlen != 64) 0.U(xprlen.W) else Cat(e32_out_with_carry_vec.map(i => i(31,0))))
  }
}

object Adder extends App {
  def apply(xprlen: Int): Adder = if(xprlen == 32 || xprlen == 64) new Adder(xprlen) else throw new Exception("xprlen is not 32 or 64\n")
  (new ChiselStage).emitVerilog(apply(64))
}

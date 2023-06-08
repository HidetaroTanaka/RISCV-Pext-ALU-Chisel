package pext_alu

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

class Adder(xprlen: Int) extends Module {
  val io = IO(new Bundle {
    val alu_baseIO = ALU_IO(xprlen)
  })

  val e8_simd_len = xprlen / 8
  val e16_simd_len = xprlen / 16
  val e32_simd_len = xprlen / 32
  val e8_rs1_vec = Wire(Vec(e8_simd_len, UInt(8.W)))
  val e8_rs2_vec = Wire(Vec(e8_simd_len, UInt(8.W)))
  val e8_out_with_carry_vec = Wire(Vec(e8_simd_len, UInt(9.W)))

  for ((d, i) <- e8_rs1_vec.zipWithIndex) {
    d := io.alu_baseIO.rs1_value((i + 1) * 8 - 1, i * 8)
  }
  for ((d, i) <- e8_rs2_vec.zipWithIndex) {
    d := io.alu_baseIO.rs2_value((i + 1) * 8 - 1, i * 8)
  }

  val e8 = (io.alu_baseIO.funct3 === "b000".U) && (io.alu_baseIO.funct7(2,1) === "b10".U)
  val e16 = (io.alu_baseIO.funct3 === "b000".U) && (io.alu_baseIO.funct7(2,1) === "b00".U)
  val e32 = (io.alu_baseIO.funct3 === "b010".U) && (io.alu_baseIO.funct7(2,1) === "b00".U)
  val e64 = (io.alu_baseIO.funct3 === "b001".U) && (io.alu_baseIO.funct7(2,1) === "b00".U) && (xprlen == 64).asBool

  for ((d, i) <- e8_out_with_carry_vec.zipWithIndex) {
    d := Cat(false.B, e8_rs1_vec(i)) + Cat(false.B, e8_rs2_vec(i))
  }

  val e16_out_with_carry_vec = Wire(Vec(e16_simd_len, UInt(16.W)))
  for((d,i) <- e16_out_with_carry_vec.zipWithIndex) {
    d := Cat(e8_out_with_carry_vec(i*2-1)(7,0), 0.U(8.W)) + Cat(0.U(7.W), e8_out_with_carry_vec(i*2))
  }

  when(e8) {
    io.alu_baseIO.out := Cat(e8_out_with_carry_vec.map(i => i(7,0)))
  } elsewhen(e16) {

  }
}

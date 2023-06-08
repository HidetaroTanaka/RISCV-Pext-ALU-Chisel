package pext_alu

import chisel3._
import chisel3.util._

class ALU_IO(xprlen: Int) extends Bundle {
  val rs1_value = Input(UInt(xprlen.W))
  val rs2_value = Input(UInt(xprlen.W))
  val funct7 = Input(UInt(7.W))
  val funct3 = Input(UInt(3.W))
  val out = Output(UInt(xprlen.W))
}

object ALU_IO {
  def apply(xprlen: Int): ALU_IO = {
    if(xprlen == 32 || xprlen == 64) {
      new ALU_IO(xprlen)
    } else {
      throw new Exception(s"ALU_IO xprlen is ${xprlen}, this must be 32 or 64.")
    }
  }
}

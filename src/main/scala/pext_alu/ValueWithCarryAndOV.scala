package pext_alu

import chisel3._
import chisel3.util._

class ValueWithCarryAndOV(width: Int) extends Bundle {
  val bits = UInt(width.W)
  val carry = Bool()
  val overflow = Bool()

  def sign: Bool = bits(width - 1)
  def +(that: ValueWithCarryAndOV): ValueWithCarryAndOV = {
    val w = Wire(new ValueWithCarryAndOV((if(this.width > that.width) this.width else that.width) + 1))
    val result = Cat(false.B, this.bits) + that.bits
    w.bits := this.bits + that.bits
    w
  }
}

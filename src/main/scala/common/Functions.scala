package common

import chisel3._
import chisel3.util._

object Functions {
  def max(in1: Int, in2: Int): Int = {
    if(in1 > in2) in1 else in2
  }
  def ext1(uint: UInt, signed: Bool): UInt = {
    require(uint.widthKnown, s"Couldn't get width of $uint.")
    Cat(uint(uint.getWidth - 1) && signed, uint)
  }
  def unsigned_maximum(width: Int): UInt = Fill(width, true.B)
  def signed_maximum(width: Int): UInt = Cat(false.B, unsigned_maximum(width-1))
  def unsigned_minimum(width: Int): UInt = Fill(width, false.B)
  def signed_minimum(width: Int): UInt = Cat(true.B, unsigned_minimum(width-1))
}

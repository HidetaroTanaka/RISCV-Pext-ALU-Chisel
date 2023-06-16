package pext_alu

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import common.Functions

import scala.collection.LazyZip3

// TODO: I think Saturating (K) and Halving (R) can share this Adder

class Adder(xprlen: Int) extends Module {
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
  })

  /**
   * 例：{0, 0, 1, 1, 0, 0, 1, 1} => 8'b00110011
   * @param seq BoolのSeq配列
   * @return 入力配列を結合してUIntにしたもの
   */
  def SeqBoolToUInt(seq: Seq[Bool]): UInt = {
    Cat(seq.reverse)
  }

  /**
   * UIntを8bitSIMDベクタに変換する
   * @param uint 元の値（64bitまたは32bit）
   * @return uintを下位から8bitごとに分割してVec[UInt]にしたもの
   */
  def divideUIntToSimd8(uint: UInt): Vec[UInt] = {
    val simd_len = uint.getWidth / 8
    val simd8 = Wire(Vec(simd_len, UInt(8.W)))
    // 0x01234567 => Seq(0x01, 0x23, 0x45, 0x67)
    val int8_array = uint.asBools.grouped(8).toSeq
    for((d, i) <- simd8.zipWithIndex) {
      d := SeqBoolToUInt(int8_array(i))
    }
    simd8
  }
  def add(in1: UInt, in2: UInt, signed: Bool): UInt = {
    require(in1.getWidth == in2.getWidth)
    Functions.ext1(in1, signed) + Functions.ext1(in2, signed)
  }
  def sub(in1: UInt, in2: UInt, signed: Bool): UInt = {
    require(in1.getWidth == in2.getWidth)
    Functions.ext1(in1, signed) - Functions.ext1(in2, signed)
  }
  def saturated_add(in1: UInt, in2: UInt, signed: Bool): (UInt, Bool) = {
    require(in1.getWidth == in2.getWidth)
    val result = add(in1, in2, signed)
    val signed_max = Functions.signed_maximum(in1.getWidth)
    val signed_min = Functions.signed_minimum(in1.getWidth)
    val unsigned_max = Functions.unsigned_maximum(in1.getWidth)
    val overflow = Wire(Bool())
    overflow := false.B
    val answer = Mux(signed,
      MuxCase(result(in1.getWidth - 1, 0), Seq(
        (result.asSInt > Functions.ext1(signed_max, signed = true.B).asSInt) -> {
          overflow := true.B
          signed_max
        },
        (result.asSInt < Functions.ext1(signed_min, signed = true.B).asSInt) -> {
          overflow := true.B
          signed_min
        },
      )),
      MuxCase(result, Seq(
        (result > Functions.ext1(unsigned_max, signed = false.B)) -> {
          overflow := true.B
          unsigned_max
        },
      ))
    )
    (answer, overflow)
  }

  def saturated_sub(in1: UInt, in2: UInt, signed: Bool): (UInt, Bool) = {
    require(in1.getWidth == in2.getWidth)
    val result = sub(in1, in2, signed)
    val signed_max = Functions.signed_maximum(in1.getWidth)
    val signed_min = Functions.signed_minimum(in1.getWidth)
    val unsigned_min = Functions.unsigned_minimum(in1.getWidth)
    val overflow = Wire(Bool())
    overflow := false.B
    val answer = Mux(signed,
      MuxCase(result(in1.getWidth - 1, 0), Seq(
        (result.asSInt > Functions.ext1(signed_max, signed = true.B).asSInt) -> {
          overflow := true.B
          signed_max
        },
        (result.asSInt < Functions.ext1(signed_min, signed = true.B).asSInt) -> {
          overflow := true.B
          signed_min
        },
      )),
      MuxCase(result, Seq(
        (result < Functions.ext1(unsigned_min, signed = false.B)) -> {
          overflow := true.B
          unsigned_min
        },
      ))
    )
    (answer, overflow)
  }
  def halving_add(in1: UInt, in2: UInt, signed: Bool): UInt = {
    require(in1.getWidth == in2.getWidth)
    add(in1, in2, signed)(8,1)
  }
  def halving_sub(in1: UInt, in2: UInt, signed: Bool): UInt = {
    require(in1.getWidth == in2.getWidth)
    sub(in1, in2, signed)(8, 1)
  }

  val e8_simd_len = xprlen / 8
  val e16_simd_len = xprlen / 16
  val e32_simd_len = xprlen / 32
  val e8_rs1_vec = divideUIntToSimd8(io.rs1_value)
  val e8_rs2_vec = divideUIntToSimd8(io.rs2_value)
  val e8_out_as_result9 = Wire(Vec(e8_simd_len, UInt(9.W)))
  val zippedInputsAndOutput: LazyZip3[UInt, UInt, UInt, e8_rs1_vec.type] = e8_rs1_vec.lazyZip(e8_rs2_vec).lazyZip(e8_out_as_result9)

  val e8 = (io.elen === "b00".U)
  val e16 = (io.elen === "b01".U)

  for((in1, in2, res) <- zippedInputsAndOutput) {
    res := Mux(!io.signed || (!io.halving && !io.saturating),
      // Unsigned (ADD, URADD, UKADD)
      Mux(io.addsub,
        Cat(false.B, in1) - Cat(false.B, in2),
        Cat(false.B, in1) + Cat(false.B, in2)
      ),
      // Signed
      Mux(io.addsub,
        Cat(in1.tail(0), in1) - Cat(in2.tail(0), in2),
        Cat(in1.tail(0), in1) + Cat(in2.tail(0), in2)
      )
    )
  }

  val e16_out_with_carry_vec = Wire(Vec(e16_simd_len, UInt(17.W)))
  for((d,i) <- e16_out_with_carry_vec.zipWithIndex) {
    d := Mux(io.addsub,
      Cat(e8_out_as_result9(i*2+1) - e8_out_as_result9(i*2)(8), e8_out_as_result9(i*2)(7,0)),
      Cat(e8_out_as_result9(i*2+1) + e8_out_as_result9(i*2)(8), e8_out_as_result9(i*2)(7,0))
    )
  }

  val e32_out_with_carry_vec = Wire(Vec(e32_simd_len, UInt(33.W)))
  for((d,i) <- e32_out_with_carry_vec.zipWithIndex) {
    d := Mux(io.addsub,
      Cat(e16_out_with_carry_vec(i*2+1) - e16_out_with_carry_vec(i*2)(16), e16_out_with_carry_vec(i*2)(15,0)),
      Cat(e16_out_with_carry_vec(i*2+1) + e16_out_with_carry_vec(i*2)(16), e16_out_with_carry_vec(i*2)(15,0))
    )
  }

  when(e8) {
    io.out := Cat(e8_out_as_result9.map(i => i(7,0)).reverse)
    // io.carry := e8_out_with_carry_vec.map(i => i(8)).reduce(_ || _)
  } .elsewhen(e16) {
    io.out := Cat(e16_out_with_carry_vec.map(i => i(15,0)).reverse)
    // io.carry := e16_out_with_carry_vec.map(i => i(16)).reduce(_ || _)
  } .otherwise {
    io.out := (if(xprlen != 64) 0.U(xprlen.W) else Cat(e32_out_with_carry_vec.map(i => i(31,0)).reverse))
    // io.carry := (if(xprlen != 64) false.B else e32_out_with_carry_vec.map(i => i(32)).reduce(_ || _))
  }
}

object Adder extends App {
  def apply(xprlen: Int): Adder = if(xprlen == 32 || xprlen == 64) new Adder(xprlen) else throw new Exception("xprlen is not 32 or 64\n")
  (new ChiselStage).emitVerilog(apply(64))
}

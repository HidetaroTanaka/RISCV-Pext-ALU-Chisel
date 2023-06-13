package pext_alu

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import jdk.jfr.Unsigned

import scala.util.Random

class AdderSpec extends AnyFreeSpec with ChiselScalatestTester {
  def Int64ToInt8Vec(int64: Long): IndexedSeq[Byte] = {
    (0 until 8).map(i => ((int64 >>> (i*8)) & 0xFF).toByte)
  }
  def Int8Vec_Add(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): IndexedSeq[Byte] = {
    require((vec1.length == 8) && (vec2.length == 8), "no")
    (0 until 8).map(i => (vec1(i) + vec2(i)).toByte)
  }
  def Int64ToBigInt(int64: Long): BigInt = {
    (BigInt(int64 >>> 1) << 1) + (int64 & 0x1)
  }
  def Int8Vec_Concatenate(int8vec: IndexedSeq[Byte]): Long = {
    require(int8vec.length == 8)
    int8vec.zipWithIndex.map(e => (e._1.toLong & 0xFF) << (e._2 * 8)).sum
  }

  "SIMD Adder should not act sussy" in {
    test(Adder(xprlen = 64)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      Random.setSeed(0)
      val testValues_in1: Seq[Long] = (0 until 32).map(_ => Random.nextLong)
      val testValues_in2: Seq[Long] = (0 until 32).map(_ => Random.nextLong)
      val testValues_out: Seq[Long] = (0 until 32).map(
        i => Int8Vec_Concatenate(Int8Vec_Add(Int64ToInt8Vec(testValues_in1(i)), Int64ToInt8Vec(testValues_in2(i))))
      )

      println(testValues_in1.map(x => x.toHexString.toUpperCase).mkString("(", ", ", ")"))
      println(testValues_in2.map(x => x.toHexString.toUpperCase).mkString("(", ", ", ")"))
      println(testValues_out.map(x => x.toHexString.toUpperCase).mkString("(", ", ", ")"))

      val testValues8_inputValue1 = testValues_in1.map(x => Int64ToBigInt(x).U(64.W))
      val testValues8_inputValue2 = testValues_in2.map(x => Int64ToBigInt(x).U(64.W))

      dut.io.elen.poke("b00".U)
      for(i <- 0 until 32) {
        dut.io.rs1_value.poke(testValues8_inputValue1(i))
        dut.io.rs2_value.poke(testValues8_inputValue2(i))
        dut.clock.step()
      }
    }
  }
}

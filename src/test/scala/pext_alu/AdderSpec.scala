package pext_alu

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

import scala.reflect.runtime.universe._

class AdderSpec extends AnyFreeSpec with ChiselScalatestTester {
  def Int64ToSimdVec[T: TypeTag](int64: Long): IndexedSeq[T] = {
    val convertToByte: (Long => Byte) = (x => x.toByte)
    val convertToShort: (Long => Short) = (x => x.toShort)
    val convertToInt: (Long => Int) = (x => x.toInt)

    val (elen, converter): (Int, Long => T) = typeOf[T] match {
      case c if c == typeOf[Byte] => (8, convertToByte.asInstanceOf[Long => T])
      case c if c == typeOf[Short] => (16, convertToShort.asInstanceOf[Long => T])
      case c if c == typeOf[Int] => (32, convertToInt.asInstanceOf[Long => T])
      case _ => throw new Exception(s"${typeOf[T]} can't be element of 64bit SIMD register.")
    }

    (0 until 64/elen).map(i => converter((int64 >>> (i*elen)) & ((1 << elen) - 1)))
  }
  def Int64ToInt8Vec(int64: Long): IndexedSeq[Byte] = {
    (0 until 8).map(i => ((int64 >>> (i*8)) & 0xFF).toByte)
  }
  def Int64ToInt16Vec(int64: Long): IndexedSeq[Short] = {
    (0 until 4).map(i => ((int64 >>> (i*16)) & 0xFFFF).toShort)
  }
  def Int64ToInt32Vec(int64: Long): IndexedSeq[Int] = {
    (0 until 2).map(i => ((int64 >>> (i*32)) & 0xFFFFFFFF).toInt)
  }
  def Int8Vec_ApplyOp(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte], op: (Byte, Byte) => Byte): IndexedSeq[Byte] = {
    require((vec1.length == 8) && (vec2.length == 8), "no")
    (0 until 8).map(i => op(vec1(i), vec2(i)))
  }
  def Int8Vec_Add(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): IndexedSeq[Byte] = {
    Int8Vec_ApplyOp(vec1, vec2, (x, y) => (x + y).toByte)
  }
  def Int8Vec_Sub(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): IndexedSeq[Byte] = {
    Int8Vec_ApplyOp(vec1, vec2, (x, y) => (x - y).toByte)
  }
  def Int16Vec_ApplyOp(vec1: IndexedSeq[Short], vec2: IndexedSeq[Short], op: (Short, Short) => Short): IndexedSeq[Short] = {
    require((vec1.length == 4) && (vec2.length == 4), "no")
    (0 until 4).map(i => op(vec1(i), vec2(i)))
  }
  def Int16Vec_Add(vec1: IndexedSeq[Short], vec2: IndexedSeq[Short]): IndexedSeq[Short] = {
    Int16Vec_ApplyOp(vec1, vec2, (x, y) => (x + y).toShort)
  }
  def Int16Vec_Sub(vec1: IndexedSeq[Short], vec2: IndexedSeq[Short]): IndexedSeq[Short] = {
    Int16Vec_ApplyOp(vec1, vec2, (x, y) => (x - y).toShort)
  }
  def Int32Vec_ApplyOp(vec1: IndexedSeq[Int], vec2: IndexedSeq[Int], op: (Int, Int) => Int): IndexedSeq[Int] = {
    require((vec1.length == 2) && (vec2.length == 2), "no")
    (0 until 2).map(i => op(vec1(i), vec2(i)))
  }
  def Int32Vec_Add(vec1: IndexedSeq[Int], vec2: IndexedSeq[Int]): IndexedSeq[Int] = {
    require((vec1.length == 2) && (vec2.length == 2), "no")
    (0 until 2).map(i => vec1(i) + vec2(i))
  }
  def Int32Vec_Sub(vec1: IndexedSeq[Int], vec2: IndexedSeq[Int]): IndexedSeq[Int] = {
    require((vec1.length == 2) && (vec2.length == 2), "no")
    (0 until 2).map(i => vec1(i) - vec2(i))
  }
  def Int64ToBigInt(int64: Long): BigInt = {
    (BigInt(int64 >>> 1) << 1) + (int64 & 0x1)
  }
  def Int8Vec_Concatenate(int8vec: IndexedSeq[Byte]): Long = {
    require(int8vec.length == 8)
    int8vec.zipWithIndex.map(e => (e._1.toLong & 0xFF) << (e._2 * 8)).sum
  }
  def Int16Vec_Concatenate(int16vec: IndexedSeq[Short]): Long = {
    require(int16vec.length == 4)
    int16vec.zipWithIndex.map(e => (e._1.toLong & 0xFFFF) << (e._2 * 16)).sum
  }
  def Int32Vec_Concatenate(int32vec: IndexedSeq[Int]): Long = {
    require(int32vec.length == 2)
    int32vec.zipWithIndex.map(e => (e._1.toLong & 0x00000000FFFFFFFFL) << (e._2 * 32)).sum
  }

  "SIMD Adder should not act sussy" in {
    test(Adder(xprlen = 64)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      Random.setSeed(0)
      val testValues_in1 = (0 until 32).map(_ => Random.nextLong)
      val testValues_in2 = (0 until 32).map(_ => Random.nextLong)
      val testValues_out = (0 until 32).map(
        i => Int8Vec_Concatenate(Int8Vec_Add(Int64ToSimdVec[Byte](testValues_in1(i)), Int64ToSimdVec[Byte](testValues_in2(i))))
      )
      val testValues16bit_out = (0 until 32).map(
        i => Int16Vec_Concatenate(Int16Vec_Add(Int64ToInt16Vec(testValues_in1(i)), Int64ToInt16Vec(testValues_in2(i))))
      )
      val testValues32bit_out = (0 until 32).map(
        i => Int32Vec_Concatenate(Int32Vec_Add(Int64ToInt32Vec(testValues_in1(i)), Int64ToInt32Vec(testValues_in2(i))))
      )
      val testValuesSub8bit_out = (0 until 32).map(
        i => Int8Vec_Concatenate(Int8Vec_Sub(Int64ToInt8Vec(testValues_in1(i)), Int64ToInt8Vec(testValues_in2(i))))
      )
      val testValuesSub16bit_out = (0 until 32).map(
        i => Int16Vec_Concatenate(Int16Vec_Sub(Int64ToInt16Vec(testValues_in1(i)), Int64ToInt16Vec(testValues_in2(i))))
      )
      val testValuesSub32bit_out = (0 until 32).map(
        i => Int32Vec_Concatenate(Int32Vec_Sub(Int64ToInt32Vec(testValues_in1(i)), Int64ToInt32Vec(testValues_in2(i))))
      )

      // println(testValues_in1.map(x => x.toHexString.toUpperCase).mkString("(", ", ", ")"))
      // println(testValues_in2.map(x => x.toHexString.toUpperCase).mkString("(", ", ", ")"))
      // println(testValues_out.map(x => x.toHexString.toUpperCase).mkString("(", ", ", ")"))

      val int64ToChiselUInt64W: Long => UInt = x => Int64ToBigInt(x).U(64.W)
      val testValues_inputValue1 = testValues_in1.map(int64ToChiselUInt64W)
      val testValues_inputValue2 = testValues_in2.map(int64ToChiselUInt64W)
      val testValues8_outputValue = testValues_out.map(int64ToChiselUInt64W)
      val testValues16_outputValue = testValues16bit_out.map(int64ToChiselUInt64W)
      val testValues32_outputValue = testValues32bit_out.map(int64ToChiselUInt64W)
      val testValuesSub8_outputValue = testValuesSub8bit_out.map(int64ToChiselUInt64W)
      val testValuesSub16_outputValue = testValuesSub16bit_out.map(int64ToChiselUInt64W)
      val testValuesSub32_outputValue = testValuesSub32bit_out.map(int64ToChiselUInt64W)

      println("start of 8bit addition test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(false.B)
      for(i <- 0 until 32) {
        dut.io.rs1_value.poke(testValues_inputValue1(i))
        dut.io.rs2_value.poke(testValues_inputValue2(i))
        dut.io.out.expect(testValues8_outputValue(i))
        dut.clock.step()
      }

      dut.io.rs1_value.poke(0.U(64.W))
      dut.io.rs2_value.poke(0.U(64.W))
      dut.io.addsub.poke(false.B)
      dut.clock.step(8)

      println("start of 8bit subtraction test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(true.B)
      for (i <- 0 until 32) {
        dut.io.rs1_value.poke(testValues_inputValue1(i))
        dut.io.rs2_value.poke(testValues_inputValue2(i))
        dut.io.out.expect(testValuesSub8_outputValue(i))
        dut.clock.step()
      }

      dut.io.rs1_value.poke(0.U(64.W))
      dut.io.rs2_value.poke(0.U(64.W))
      dut.io.addsub.poke(false.B)
      dut.clock.step(8)

      println("start of 16bit addition test:")
      dut.io.elen.poke("b01".U)
      dut.io.addsub.poke(false.B)
      for (i <- 0 until 32) {
        dut.io.rs1_value.poke(testValues_inputValue1(i))
        dut.io.rs2_value.poke(testValues_inputValue2(i))
        dut.io.out.expect(testValues16_outputValue(i))
        dut.clock.step()
      }

      println("start of 16bit subtraction test:")
      dut.io.elen.poke("b01".U)
      dut.io.addsub.poke(true.B)
      for (i <- 0 until 32) {
        dut.io.rs1_value.poke(testValues_inputValue1(i))
        dut.io.rs2_value.poke(testValues_inputValue2(i))
        dut.io.out.expect(testValuesSub16_outputValue(i))
        dut.clock.step()
      }

      dut.io.rs1_value.poke(0.U(64.W))
      dut.io.rs2_value.poke(0.U(64.W))
      dut.io.addsub.poke(false.B)
      dut.clock.step(8)

      println("start of 32bit addition test:")
      dut.io.elen.poke("b10".U)
      for (i <- 0 until 32) {
        dut.io.rs1_value.poke(testValues_inputValue1(i))
        dut.io.rs2_value.poke(testValues_inputValue2(i))
        dut.io.out.expect(testValues32_outputValue(i))
        dut.clock.step()
      }

      dut.io.rs1_value.poke(0.U(64.W))
      dut.io.rs2_value.poke(0.U(64.W))
      dut.io.addsub.poke(false.B)
      dut.clock.step(8)

      println("start of 32bit addition test:")
      dut.io.elen.poke("b10".U)
      dut.io.addsub.poke(true.B)
      for (i <- 0 until 32) {
        dut.io.rs1_value.poke(testValues_inputValue1(i))
        dut.io.rs2_value.poke(testValues_inputValue2(i))
        dut.io.out.expect(testValuesSub32_outputValue(i))
        dut.clock.step()
      }
    }
  }
}

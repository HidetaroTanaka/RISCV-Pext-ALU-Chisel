package pext_alu

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

import scala.reflect.runtime.universe._

class AdderSpec extends AnyFreeSpec with ChiselScalatestTester {
  def getElenOf[T: TypeTag]: Int = {
    typeOf[T] match {
      case c if c == typeOf[Byte] => 8
      case c if c == typeOf[Short] => 16
      case c if c == typeOf[Int] => 32
      case _ => throw new Exception(s"${typeOf[T]} can't be element of 64bit SIMD register.")
    }
  }
  def Int64ToSimdVec[T: TypeTag](int64: Long): IndexedSeq[T] = {
    val convertToByte: (Long => Byte) = (x => x.toByte)
    val convertToShort: (Long => Short) = (x => x.toShort)
    val convertToInt: (Long => Int) = (x => x.toInt)

    val elen = getElenOf[T]
    val converter: Long => T = elen match {
      case 8 => convertToByte.asInstanceOf[Long => T]
      case 16 => convertToShort.asInstanceOf[Long => T]
      case 32 => convertToInt.asInstanceOf[Long => T]
      case _ => throw new Exception("It can't happen here")
    }
    for(i <- 0 until 64/elen) yield {
      converter((int64 >>> (i*elen)) & ((1L << elen) - 1))
    }
  }
  def SimdVec_ApplyOp[T: TypeTag](vec1: IndexedSeq[T], vec2: IndexedSeq[T], op: (T, T) => T): IndexedSeq[T] = {
    val elen = getElenOf[T]
    require((vec1.length == 64/elen) && (vec2.length == 64/elen), "no")
    for((val1, val2) <- (vec1 zip vec2)) yield {
      op(val1, val2)
    }
  }
  def Int8Vec_Add(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): IndexedSeq[Byte] = {
    SimdVec_ApplyOp[Byte](vec1, vec2, (x, y) => (x + y).toByte)
  }
  def Int8Vec_Sub(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): IndexedSeq[Byte] = {
    SimdVec_ApplyOp[Byte](vec1, vec2, (x, y) => (x - y).toByte)
  }
  def Int16Vec_Add(vec1: IndexedSeq[Short], vec2: IndexedSeq[Short]): IndexedSeq[Short] = {
    SimdVec_ApplyOp[Short](vec1, vec2, (x, y) => (x + y).toShort)
  }
  def Int16Vec_Sub(vec1: IndexedSeq[Short], vec2: IndexedSeq[Short]): IndexedSeq[Short] = {
    SimdVec_ApplyOp[Short](vec1, vec2, (x, y) => (x - y).toShort)
  }
  def Int32Vec_Add(vec1: IndexedSeq[Int], vec2: IndexedSeq[Int]): IndexedSeq[Int] = {
    SimdVec_ApplyOp[Int](vec1, vec2, (x, y) => x + y)
  }
  def Int32Vec_Sub(vec1: IndexedSeq[Int], vec2: IndexedSeq[Int]): IndexedSeq[Int] = {
    SimdVec_ApplyOp[Int](vec1, vec2, (x, y) => x - y)
  }
  def Int64ToBigInt(int64: Long): BigInt = {
    (BigInt(int64 >>> 1) << 1) + (int64 & 0x1)
  }
  def SimdVec_Concatenate[T: TypeTag](simdVec: IndexedSeq[T]): Long = {
    val convertFromByte: (Byte => Long) = (x => x.toLong)
    val convertFromShort: (Short => Long) = (x => x.toLong)
    val convertFromInt: (Int => Long) = (x => x.toLong)

    val elen = getElenOf[T]
    require(simdVec.length == 64/elen)
    val converter: T => Long = elen match {
      case 8 => convertFromByte.asInstanceOf[T => Long]
      case 16 => convertFromShort.asInstanceOf[T => Long]
      case 32 => convertFromInt.asInstanceOf[T => Long]
      case _ => throw new Exception("It can't happen here")
    }
    (for((element_value, element_index) <- simdVec.zipWithIndex) yield {
      (converter(element_value) & ((1L << elen) - 1)) << (element_index * elen)
    }).sum
  }

  "SIMD Adder should not act sussy" in {
    test(Adder(xprlen = 64)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      Random.setSeed(0)
      val testValues_in1 = IndexedSeq.fill(32)(Random.nextLong)
      val testValues_in2 = IndexedSeq.fill(32)(Random.nextLong)

      val testValues8bit_out = for((in1, in2) <- (testValues_in1 zip testValues_in2)) yield {
        SimdVec_Concatenate[Byte](Int8Vec_Add(Int64ToSimdVec[Byte](in1), Int64ToSimdVec[Byte](in2)))
      }
      val testValues16bit_out = for((in1, in2) <- (testValues_in1 zip testValues_in2)) yield {
        SimdVec_Concatenate[Short](Int16Vec_Add(Int64ToSimdVec[Short](in1), Int64ToSimdVec[Short](in2)))
      }
      val testValues32bit_out = for ((in1, in2) <- (testValues_in1 zip testValues_in2)) yield {
        SimdVec_Concatenate[Int](Int32Vec_Add(Int64ToSimdVec[Int](in1), Int64ToSimdVec[Int](in2)))
      }
      val testValuesSub8bit_out = for ((in1, in2) <- (testValues_in1 zip testValues_in2)) yield {
        SimdVec_Concatenate[Byte](Int8Vec_Sub(Int64ToSimdVec[Byte](in1), Int64ToSimdVec[Byte](in2)))
      }
      val testValuesSub16bit_out = for ((in1, in2) <- (testValues_in1 zip testValues_in2)) yield {
        SimdVec_Concatenate[Short](Int16Vec_Sub(Int64ToSimdVec[Short](in1), Int64ToSimdVec[Short](in2)))
      }
      val testValuesSub32bit_out = for ((in1, in2) <- (testValues_in1 zip testValues_in2)) yield {
        SimdVec_Concatenate[Int](Int32Vec_Sub(Int64ToSimdVec[Int](in1), Int64ToSimdVec[Int](in2)))
      }

      val int64ToChiselUInt64W: Long => UInt = x => Int64ToBigInt(x).U(64.W)
      val testValues_inputValue1 = testValues_in1.map(int64ToChiselUInt64W)
      val testValues_inputValue2 = testValues_in2.map(int64ToChiselUInt64W)
      val testValues8_outputValue = testValues8bit_out.map(int64ToChiselUInt64W)
      val testValues16_outputValue = testValues16bit_out.map(int64ToChiselUInt64W)
      val testValues32_outputValue = testValues32bit_out.map(int64ToChiselUInt64W)
      val testValuesSub8_outputValue = testValuesSub8bit_out.map(int64ToChiselUInt64W)
      val testValuesSub16_outputValue = testValuesSub16bit_out.map(int64ToChiselUInt64W)
      val testValuesSub32_outputValue = testValuesSub32bit_out.map(int64ToChiselUInt64W)

      def doTest(expect_vector: IndexedSeq[UInt]): Unit = {
        for (i <- expect_vector.indices) {
          dut.io.rs1_value.poke(testValues_inputValue1(i))
          dut.io.rs2_value.poke(testValues_inputValue2(i))
          dut.io.out.expect(expect_vector(i))
          dut.clock.step()
        }
        dut.io.rs1_value.poke(0.U(64.W))
        dut.io.rs2_value.poke(0.U(64.W))
        dut.io.addsub.poke(false.B)
        dut.clock.step(8)
      }

      println("start of 8bit addition test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(false.B)
      dut.io.signed.poke(false.B)
      dut.io.saturating.poke(false.B)
      dut.io.halving.poke(false.B)
      doTest(testValues8_outputValue)

      println("start of 8bit subtraction test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(true.B)
      doTest(testValuesSub8_outputValue)

      println("start of 16bit addition test:")
      dut.io.elen.poke("b01".U)
      dut.io.addsub.poke(false.B)
      doTest(testValues16_outputValue)

      println("start of 16bit subtraction test:")
      dut.io.elen.poke("b01".U)
      dut.io.addsub.poke(true.B)
      doTest(testValuesSub16_outputValue)

      println("start of 32bit addition test:")
      dut.io.elen.poke("b10".U)
      dut.io.addsub.poke(false.B)
      doTest(testValues32_outputValue)

      println("start of 32bit addition test:")
      dut.io.elen.poke("b10".U)
      dut.io.addsub.poke(true.B)
      doTest(testValuesSub32_outputValue)
    }
  }
}

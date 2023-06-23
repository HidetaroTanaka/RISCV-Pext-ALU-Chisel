package pext_alu

import chisel3.{UInt, _}
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random
import scala.reflect.runtime.universe._

class SIMDAdderSpec extends AnyFreeSpec with ChiselScalatestTester {
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
    for (i <- 0 until 64 / elen) yield {
      converter((int64 >>> (i * elen)) & ((1L << elen) - 1))
    }
  }

  def SimdVec_ApplyOpWithFlag[T: TypeTag](vec1: IndexedSeq[T], vec2: IndexedSeq[T], op: (T, T) => (T, Boolean)): (IndexedSeq[T], Boolean) = {
    val elen = getElenOf[T]
    require((vec1.length == 64 / elen) && (vec2.length == 64 / elen), "no")
    val res_array: IndexedSeq[(T, Boolean)] = (vec1 zip vec2).map{ case (val1, val2) => op(val1, val2) }
    (res_array.map(x => x._1), res_array.map(x => x._2).reduce(_ || _))
  }

  def ADD8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => ((x + y).toByte, false))
  }

  def SUB8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => ((x - y).toByte, false))
  }

  def ADD16(vec1: IndexedSeq[Short], vec2: IndexedSeq[Short]): (IndexedSeq[Short], Boolean) = {
    SimdVec_ApplyOpWithFlag[Short](vec1, vec2, (x, y) => ((x + y).toShort, false))
  }

  def SUB16(vec1: IndexedSeq[Short], vec2: IndexedSeq[Short]): (IndexedSeq[Short], Boolean) = {
    SimdVec_ApplyOpWithFlag[Short](vec1, vec2, (x, y) => ((x - y).toShort, false))
  }

  def ADD32(vec1: IndexedSeq[Int], vec2: IndexedSeq[Int]): (IndexedSeq[Int], Boolean) = {
    SimdVec_ApplyOpWithFlag[Int](vec1, vec2, (x, y) => (x + y, false))
  }

  def SUB32(vec1: IndexedSeq[Int], vec2: IndexedSeq[Int]): (IndexedSeq[Int], Boolean) = {
    SimdVec_ApplyOpWithFlag[Int](vec1, vec2, (x, y) => (x - y, false))
  }

  def ByteSaturate(value: Int): (Byte, Boolean) = {
    if(value > Byte.MaxValue) {
      (Byte.MaxValue, true)
    } else if (value < Byte.MinValue) {
      (Byte.MinValue, true)
    } else {
      (value.toByte, false)
    }
  }
  def UByteSaturate(value: Int): (Byte, Boolean) = {
    if(value > 0xFF) {
      (0xFF.toByte, true)
    } else if(value < 0) {
      (0.toByte, true)
    } else {
      (value.toByte, false)
    }
  }
  def KADD8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => ByteSaturate(x + y))
  }
  def KSUB8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => ByteSaturate(x - y))
  }
  def UKADD8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => UByteSaturate((x & 0xFF) + (y & 0xFF)))
  }
  def UKSUB8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => UByteSaturate((x & 0xFF) - (y & 0xFF)))
  }
  def RADD8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => (((x + y) >> 1).toByte, false))
  }
  def RSUB8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => (((x - y) >> 1).toByte, false))
  }
  def URADD8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => ((((x & 0xFF) + (y & 0xFF)) >> 1).toByte, false))
  }
  def URSUB8(vec1: IndexedSeq[Byte], vec2: IndexedSeq[Byte]): (IndexedSeq[Byte], Boolean) = {
    SimdVec_ApplyOpWithFlag[Byte](vec1, vec2, (x, y) => ((((x & 0xFF) - (y & 0xFF)) >> 1).toByte, false))
  }
  def Int64ToBigInt(int64: Long): BigInt = {
    (BigInt(int64 >>> 1) << 1) + (int64 & 0x1)
  }

  def SimdVec_Concatenate[T: TypeTag](simdVec: IndexedSeq[T]): Long = {
    val convertFromByte: (Byte => Long) = (x => x.toLong)
    val convertFromShort: (Short => Long) = (x => x.toLong)
    val convertFromInt: (Int => Long) = (x => x.toLong)

    val elen = getElenOf[T]
    require(simdVec.length == 64 / elen)
    val converter: T => Long = elen match {
      case 8 => convertFromByte.asInstanceOf[T => Long]
      case 16 => convertFromShort.asInstanceOf[T => Long]
      case 32 => convertFromInt.asInstanceOf[T => Long]
      case _ => throw new Exception("It can't happen here")
    }
    (for ((element_value, element_index) <- simdVec.zipWithIndex) yield {
      (converter(element_value) & ((1L << elen) - 1)) << (element_index * elen)
    }).sum
  }

  "SIMD Adder should not act sussy" in {
    test(SIMDAdder(xprlen = 64)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      Random.setSeed(0)
      val testValues_in1 = IndexedSeq.fill(32)(Random.nextLong)
      val testValues_in2 = IndexedSeq.fill(32)(Random.nextLong)

      val testValues_ADD8: IndexedSeq[(Long, Boolean)] = for((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = ADD8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_SUB8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = SUB8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_KADD8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = KADD8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_KSUB8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = KSUB8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_UKADD8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = UKADD8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_UKSUB8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = UKSUB8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_RADD8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = RADD8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_RSUB8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = RSUB8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_URADD8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = URADD8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }
      val testValues_URSUB8 = for ((rs1, rs2) <- testValues_in1 zip testValues_in2) yield {
        val ans = URSUB8(Int64ToSimdVec[Byte](rs1), Int64ToSimdVec[Byte](rs2))
        (SimdVec_Concatenate[Byte](ans._1), ans._2)
      }

      val int64ToChiselUInt64W: Long => UInt = x => Int64ToBigInt(x).U(64.W)
      val tupleWithInt64AndBooleanToChiselType: ((Long, Boolean)) => (UInt, Bool) = e => (int64ToChiselUInt64W(e._1), e._2.B)
      val testValues_inputValue1 = testValues_in1.map(int64ToChiselUInt64W)
      val testValues_inputValue2 = testValues_in2.map(int64ToChiselUInt64W)
      val testValues_outputADD8 = testValues_ADD8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputSUB8 = testValues_SUB8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputKADD8 = testValues_KADD8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputKSUB8 = testValues_KSUB8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputUKADD8 = testValues_UKADD8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputUKSUB8 = testValues_UKSUB8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputRADD8 = testValues_RADD8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputRSUB8 = testValues_RSUB8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputURADD8 = testValues_URADD8.map(tupleWithInt64AndBooleanToChiselType)
      val testValues_outputURSUB8 = testValues_URSUB8.map(tupleWithInt64AndBooleanToChiselType)

      def doTest_withOverflow(expect_vector: IndexedSeq[(UInt, Bool)]): Unit = {
        print("pass: ")
        for (i <- testValues_in1.indices) {
          dut.io.rs1_value.poke(testValues_inputValue1(i))
          dut.io.rs2_value.poke(testValues_inputValue2(i))
          dut.io.out.expect(expect_vector(i)._1)
          dut.io.overflow.expect(expect_vector(i)._2)
          dut.clock.step()
          print(s"$i, ")
        }
        println("\b\b")
        dut.io.rs1_value.poke(0.U(64.W))
        dut.io.rs2_value.poke(0.U(64.W))
        dut.io.addsub.poke(false.B)
        dut.io.signed.poke(false.B)
        dut.io.saturating.poke(false.B)
        dut.io.halving.poke(false.B)
        dut.clock.step(8)
      }

      println("start of 8bit addition test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(false.B)
      dut.io.signed.poke(false.B)
      dut.io.saturating.poke(false.B)
      dut.io.halving.poke(false.B)
      doTest_withOverflow(testValues_outputADD8)

      println("start of 8bit subtraction test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(true.B)
      dut.io.signed.poke(false.B)
      dut.io.saturating.poke(false.B)
      dut.io.halving.poke(false.B)
      doTest_withOverflow(testValues_outputSUB8)

      println("start of 8bit signed saturating addition test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(false.B)
      dut.io.signed.poke(true.B)
      dut.io.saturating.poke(true.B)
      dut.io.halving.poke(false.B)
      doTest_withOverflow(testValues_outputKADD8)

      println("start of 8bit signed saturating subtraction test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(true.B)
      dut.io.signed.poke(true.B)
      dut.io.saturating.poke(true.B)
      dut.io.halving.poke(false.B)
      doTest_withOverflow(testValues_outputKSUB8)

      println("start of 8bit unsigned saturating addition test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(false.B)
      dut.io.signed.poke(false.B)
      dut.io.saturating.poke(true.B)
      dut.io.halving.poke(false.B)
      doTest_withOverflow(testValues_outputUKADD8)

      println("start of 8bit unsigned saturating subtraction test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(true.B)
      dut.io.signed.poke(false.B)
      dut.io.saturating.poke(true.B)
      dut.io.halving.poke(false.B)
      doTest_withOverflow(testValues_outputUKSUB8)

      println("start of 8bit signed halving addition test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(false.B)
      dut.io.signed.poke(true.B)
      dut.io.saturating.poke(false.B)
      dut.io.halving.poke(true.B)
      doTest_withOverflow(testValues_outputRADD8)

      println("start of 8bit signed saturating subtraction test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(true.B)
      dut.io.signed.poke(true.B)
      dut.io.saturating.poke(false.B)
      dut.io.halving.poke(true.B)
      doTest_withOverflow(testValues_outputRSUB8)

      println("start of 8bit unsigned saturating addition test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(false.B)
      dut.io.signed.poke(false.B)
      dut.io.saturating.poke(false.B)
      dut.io.halving.poke(true.B)
      doTest_withOverflow(testValues_outputURADD8)

      println("start of 8bit unsigned saturating subtraction test:")
      dut.io.elen.poke("b00".U)
      dut.io.addsub.poke(true.B)
      dut.io.signed.poke(false.B)
      dut.io.saturating.poke(false.B)
      dut.io.halving.poke(true.B)
      doTest_withOverflow(testValues_outputURSUB8)
    }
  }
}

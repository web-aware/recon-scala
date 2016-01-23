package recon

import basis.collections._
import basis.util._
import org.scalatest._

class FormSpec extends FreeSpec with Matchers {
  override def suiteName = "Form typeclasses"

  "should mold numbers from Byte values" in {
    Value(42.toByte) should equal (recon"42")
  }

  "should cast numbers to Byte values" in {
    recon"42".coerce[Byte] should equal (42.toByte)
    recon"-1".coerce[Byte] should equal (-1.toByte)
    recon"257".coerce[Byte] should equal (1.toByte)
    recon"2.5".coerce[Byte] should equal (2.toByte)
  }

  "should cast numeric strings to Byte values" in {
    recon"[42]".coerce[Byte] should equal (42.toByte)
    recon"[-1]".coerce[Byte] should equal (-1.toByte)
    recon"[257]".coerce[Byte] should equal (1.toByte)
    recon"[2.5]".coerce[Byte] should equal (2.toByte)
  }

  "should cast attributed numbers to Byte values" in {
    recon"@byte 42".coerce[Byte] should equal (42.toByte)
  }

  "should mold numbers from Short values" in {
    Value(42.toShort) should equal (recon"42")
  }

  "should cast numbers to Short values" in {
    recon"42".coerce[Short] should equal (42.toShort)
    recon"-1".coerce[Short] should equal (-1.toShort)
    recon"65537".coerce[Short] should equal (1.toShort)
    recon"2.5".coerce[Short] should equal (2.toShort)
  }

  "should cast numeric strings to Short values" in {
    recon"[42]".coerce[Short] should equal (42.toShort)
    recon"[-1]".coerce[Short] should equal (-1.toShort)
    recon"[65537]".coerce[Short] should equal (1.toShort)
    recon"[2.5]".coerce[Short] should equal (2.toShort)
  }

  "should cast attributed numbers to Short values" in {
    recon"@short 42".coerce[Short] should equal (42.toShort)
  }

  "should mold numbers from Int values" in {
    Value(42) should equal (recon"42")
  }

  "should cast numbers to Int values" in {
    recon"42".coerce[Int] should equal (42)
    recon"-1".coerce[Int] should equal (-1)
    recon"4294967297".coerce[Int] should equal (1)
    recon"2.5".coerce[Int] should equal (2)
  }

  "should cast numeric strings to Int values" in {
    recon"[42]".coerce[Int] should equal (42)
    recon"[-1]".coerce[Int] should equal (-1)
    recon"[4294967297]".coerce[Int] should equal (1)
    recon"[2.5]".coerce[Int] should equal (2)
  }

  "should cast attributed numbers to Int values" in {
    recon"@int 42".coerce[Int] should equal (42.toInt)
  }

  "should mold numbers from Long values" in {
    Value(42L) should equal (recon"42")
  }

  "should cast numbers to Long values" in {
    recon"42".coerce[Long] should equal (42L)
    recon"-1".coerce[Long] should equal (-1L)
    recon"2.5".coerce[Long] should equal (2L)
  }

  "should cast numeric strings to Long values" in {
    recon"[42]".coerce[Long] should equal (42L)
    recon"[-1]".coerce[Long] should equal (-1L)
    recon"[2.5]".coerce[Long] should equal (2L)
  }

  "should cast attributed numbers to Long values" in {
    recon"@long 42".coerce[Long] should equal (42)
  }

  "should mold numbers from Float values" in {
    Value(2.5F) should equal (recon"2.5")
  }

  "should cast numbers to Float values" in {
    recon"42".coerce[Float] should equal (42.0F)
    recon"-1".coerce[Float] should equal (-1.0F)
    recon"2.5".coerce[Float] should equal (2.5F)
  }

  "should cast numeric strings to Float values" in {
    recon"[42]".coerce[Float] should equal (42.0F)
    recon"[-1]".coerce[Float] should equal (-1.0F)
    recon"[2.5]".coerce[Float] should equal (2.5F)
  }

  "should cast attributed numbers to Float values" in {
    recon"@float 42".coerce[Float] should equal (42.0F)
  }

  "should mold numbers from Double values" in {
    Value(2.5) should equal (recon"2.5")
  }

  "should cast numbers to Double values" in {
    recon"42".coerce[Double] should equal (42.0)
    recon"-1".coerce[Double] should equal (-1.0)
    recon"2.5".coerce[Double] should equal (2.5)
  }

  "should cast numeric strings to Double values" in {
    recon"[42]".coerce[Double] should equal (42.0)
    recon"[-1]".coerce[Double] should equal (-1.0)
    recon"[2.5]".coerce[Double] should equal (2.5)
  }

  "should cast attributed numbers to Double values" in {
    recon"@double 42".coerce[Double] should equal (42.0)
  }

  "should mold boolean identifiers from Boolean values" in {
    Value(true) should equal (recon"true")
    Value(false) should equal (recon"false")
  }

  "should cast boolean identifiers to Boolean values" in {
    recon"true".coerce[Boolean] should equal (true)
    recon"false".coerce[Boolean] should equal (false)
  }

  "should cast attributed boolean identifiers to Boolean values" in {
    recon"@bool true".coerce[Boolean] should equal (true)
    recon"@bool false".coerce[Boolean] should equal (false)
  }

  "should mold text from String values" in {
    Value("") should equal (recon""" "" """)
    Value("test") should equal (recon""" "test" """)
  }

  "should cast text to String values" in {
    recon""" "" """.coerce[String] should equal ("")
    recon""" "test" """.coerce[String] should equal ("test")
  }

  "should cast numbers to String values" in {
    recon"42".coerce[String] should equal ("42")
    recon"-1".coerce[String] should equal ("-1")
    recon"2.5".coerce[String] should equal ("2.5")
  }

  "should cast attributed text to String values" in {
    recon"@string [test]".coerce[String] should equal ("test")
  }

  "should mold records from arrays" in {
    Value(Array(1, 2, 3)) should equal (recon"{ 1, 2, 3 }")
    Value(Array(true, false)) should equal (recon"{ true, false }")
  }

  "should cast records to arrays" in {
    recon"{ 1, [2], 3 }".coerce[Array[Int]] should equal (Array(1, 2, 3))
    recon"{ 1, [2], 3 }".coerce[Array[Double]] should equal (Array(1.0, 2.0, 3.0))
    recon"{ 1, [2], 3 }".coerce[Array[String]] should equal (Array("1", "2", "3"))
  }

  "should cast primitive values to unary arrays" in {
    recon"1".coerce[Array[Int]] should equal (Array(1))
    recon"test".coerce[Array[String]] should equal (Array("test"))
  }

  "should mold records from containers" in {
    Value(Seq(1, 2, 3)) should equal (recon"{ 1, 2, 3 }")
    Value(Set(1, 2, 3)) should equal (recon"{ 1, 2, 3 }")
  }

  "should cast records to containers" in {
    recon"{ 1, [2], 3 }".coerce[Seq[Int]] should equal (Seq(1, 2, 3))
    recon"{ 1, [2], 3 }".coerce[Set[Int]] should equal (Set(1, 2, 3))
    recon"{ 1, [2], 3 }".coerce[Seq[Double]] should equal (Seq(1.0, 2.0, 3.0))
    recon"{ 1, [2], 3 }".coerce[Set[Double]] should equal (Set(1.0, 2.0, 3.0))
    recon"{ 1, [2], 3 }".coerce[Seq[String]] should equal (Seq("1", "2", "3"))
    recon"{ 1, [2], 3 }".coerce[Set[String]] should equal (Set("1", "2", "3"))
  }

  "should cast primitive values to unary containers" in {
    recon"1".coerce[Seq[Int]] should equal (Seq(1))
    recon"1".coerce[Set[Int]] should equal (Set(1))
    recon"test".coerce[Seq[String]] should equal (Seq("test"))
    recon"test".coerce[Set[String]] should equal (Set("test"))
  }

  "should mold records from string maps" in {
    Value(Map("a" -> 1, "b" -> 2, "c" -> 3)) should equal (recon"{ a: 1, b: 2, c: 3 }")
  }

  "should cast records to string maps" in {
    recon"{a:1;b:[2];c:3}".coerce[Map[String, Int]] should equal (Map("a" -> 1, "b" -> 2, "c" -> 3))
    recon"{a:1;b:[2];c:3}".coerce[Map[String, Double]] should equal (Map("a" -> 1.0, "b" -> 2.0, "c" -> 3.0))
    recon"{a:1;b:[2];c:3}".coerce[Map[String, String]] should equal (Map("a" -> "1", "b" -> "2", "c" -> "3"))
  }
}

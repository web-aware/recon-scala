package recon

import basis._
import basis.collections._
import basis.collections.generic._
import basis.data._
import basis.text._
import basis.util._
import scala.reflect._

trait Mold[@specialized(Mold.Specialized) T] {
  def unit: T

  def form(recon: Recon)(value: T): recon.Value

  def cast(recon: Recon)(value: recon.Value): Maybe[T]

  def norm(recon: Recon)(value: recon.Value): recon.Value = value
}

object Mold {
  def apply[T](implicit mold: Mold[T]): mold.type = mold

  def Byte(unit: Byte): Mold[Byte] = new ByteMold(unit)
  def Short(unit: Short): Mold[Short] = new ShortMold(unit)
  def Int(unit: Int): Mold[Int] = new IntMold(unit)
  def Long(unit: Long): Mold[Long] = new LongMold(unit)
  def Float(unit: Float): Mold[Float] = new FloatMold(unit)
  def Double(unit: Double): Mold[Double] = new DoubleMold(unit)
  def Boolean(unit: Boolean): Mold[Boolean] = new BooleanMold(unit)
  def String(unit: String): Mold[String] = new StringMold(unit)

  implicit lazy val Byte: Mold[Byte] = new ByteMold(0)
  implicit lazy val Short: Mold[Short] = new ShortMold(0)
  implicit lazy val Int: Mold[Int] = new IntMold(0)
  implicit lazy val Long: Mold[Long] = new LongMold(0L)
  implicit lazy val Float: Mold[Float] = new FloatMold(0.0F)
  implicit lazy val Double: Mold[Double] = new DoubleMold(0.0)
  implicit lazy val Boolean: Mold[Boolean] = new BooleanMold(false)
  implicit lazy val Unit: Mold[Unit] = new UnitMold()
  implicit lazy val String: Mold[String] = new StringMold("")

  implicit def Data[Data <: Loader](implicit Data: DataFactory[Data]): Mold[Data] =
    new DataMold[Data]()(Data)

  implicit def Array[A](implicit A: Mold[A], ATag: ClassTag[A]): Mold[Array[A]] =
    new ArrayMold[A]()(A, ATag)

  implicit def Container[CC[X] <: Container[X], A](implicit CC: CollectionFactory[CC], A: Mold[A]): Mold[CC[A]] =
    new ContainerMold[CC, A]()(CC, A)

  implicit def Map[CC[X, Y] <: Map[X, Y], A, T](implicit CC: MapFactory[CC], A: Mold[A], T: Mold[T]): Mold[CC[A, T]] =
    new MapMold[CC, A, T]()(CC, A, T)

  protected[recon] final val Specialized =
    new Specializable.Group((scala.Int, scala.Long, scala.Float, scala.Double))
}

private[recon] final class ByteMold(override val unit: Byte) extends Mold[Byte] {
  override def form(recon: Recon)(value: Byte): recon.Value = recon.Number(value)

  override def cast(recon: Recon)(value: recon.Value): Maybe[Byte] =
    if (value.isNumber) Bind(value.asNumber.toByte)
    else if (value.isText)
      try Bind(recon.Number(value.asText.toUString.toString).toByte)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isNumber) value
    else if (value.isText)
      try recon.Number(value.asText.toUString.toString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Mold"~'.'~"Byte"
    if (unit != 0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class ShortMold(override val unit: Short) extends Mold[Short] {
  override def form(recon: Recon)(value: Short): recon.Value = recon.Number(value)

  override def cast(recon: Recon)(value: recon.Value): Maybe[Short] =
    if (value.isNumber) Bind(value.asNumber.toShort)
    else if (value.isText)
      try Bind(recon.Number(value.asText.toUString.toString).toShort)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isNumber) value
    else if (value.isText)
      try recon.Number(value.asText.toUString.toString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Mold"~'.'~"Short"
    if (unit != 0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class IntMold(override val unit: Int) extends Mold[Int] {
  override def form(recon: Recon)(value: Int): recon.Value = recon.Number(value)

  override def cast(recon: Recon)(value: recon.Value): Maybe[Int] =
    if (value.isNumber) Bind(value.asNumber.toInt)
    else if (value.isText)
      try Bind(recon.Number(value.asText.toUString.toString).toInt)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isNumber) value
    else if (value.isText)
      try recon.Number(value.asText.toUString.toString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Mold"~'.'~"Int"
    if (unit != 0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class LongMold(override val unit: Long) extends Mold[Long] {
  override def form(recon: Recon)(value: Long): recon.Value = recon.Number(value)

  override def cast(recon: Recon)(value: recon.Value): Maybe[Long] =
    if (value.isNumber) Bind(value.asNumber.toLong)
    else if (value.isText)
      try Bind(recon.Number(value.asText.toUString.toString).toLong)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isNumber) value
    else if (value.isText)
      try recon.Number(value.asText.toUString.toString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Mold"~'.'~"Long"
    if (unit != 0L) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class FloatMold(override val unit: Float) extends Mold[Float] {
  override def form(recon: Recon)(value: Float): recon.Value = recon.Number(value)

  override def cast(recon: Recon)(value: recon.Value): Maybe[Float] =
    if (value.isNumber) Bind(value.asNumber.toFloat)
    else if (value.isText)
      try Bind(recon.Number(value.asText.toUString.toString).toFloat)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isNumber) value
    else if (value.isText)
      try recon.Number(value.asText.toUString.toString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Mold"~'.'~"Float"
    if (unit != 0.0F) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class DoubleMold(override val unit: Double) extends Mold[Double] {
  override def form(recon: Recon)(value: Double): recon.Value = recon.Number(value)

  override def cast(recon: Recon)(value: recon.Value): Maybe[Double] =
    if (value.isNumber) Bind(value.asNumber.toFloat)
    else if (value.isText)
      try Bind(recon.Number(value.asText.toUString.toString).toDouble)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isNumber) value
    else if (value.isText)
      try recon.Number(value.asText.toUString.toString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Mold"~'.'~"Double"
    if (unit != 0.0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class BooleanMold(override val unit: Boolean) extends Mold[Boolean] {
  override def form(recon: Recon)(value: Boolean): recon.Value =
    if (value) recon.True else recon.False

  override def cast(recon: Recon)(value: recon.Value): Maybe[Boolean] =
    if (value.isText) value.asText.toUString.toString match {
      case "true" => basis.True
      case "false" => basis.False
      case _ => Trap
    }
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def toString: String = {
    val s = String.Builder~"Mold"~'.'~"Boolean"
    if (unit) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class UnitMold extends Mold[Unit] {
  override def unit: Unit = ()

  override def form(recon: Recon)(value: Unit): recon.Value = recon.Absent

  override def cast(recon: Recon)(value: recon.Value): Maybe[Unit] = Trap

  override def toString: String = (String.Builder~"Mold"~'.'~"Unit").state
}

private[recon] final class StringMold(override val unit: String) extends Mold[String] {
  override def form(recon: Recon)(value: String): recon.Value = recon.Text(value)

  override def cast(recon: Recon)(value: recon.Value): Maybe[String] =
    if (value.isText) Bind(value.asText.toUString.toString)
    else if (value.isNumber) Bind(value.asNumber.toDecimalString)
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isText) value
    else if (value.isNumber) recon.Text(value.asNumber.toDecimalString)
    else value

  override def toString: String = {
    val s = String.Builder~"Mold"~'.'~"String"
    if (unit.length > 0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class DataMold[Data <: Loader](implicit Data: DataFactory[Data]) extends Mold[Data] {
  override def unit: Data = Data.empty

  override def form(recon: Recon)(value: Data): recon.Value =
    recon.Data.from(value)

  override def cast(recon: Recon)(value: recon.Value): Maybe[Data] =
    if (value.isData) Bind(Data.from(value.asData))
    else if (value.isRecord) cast(recon)(value.target)
    else Trap

  override def toString: String = (String.Builder~"Mold"~'.'~"Data"~'('~>Data~')').state
}

private[recon] final class ArrayMold[A](implicit A: Mold[A], ATag: ClassTag[A]) extends Mold[Array[A]] {
  private[this] var empty: Array[A] = _

  override def unit: Array[A] = {
    if (empty eq null) empty = ATag.newArray(0)
    empty
  }

  override def form(recon: Recon)(value: Array[A]): recon.Value =
    value.map(A.form(recon)(_))(recon.RecordBuilder)

  override def cast(recon: Recon)(value: recon.Value): Maybe[Array[A]] =
    if (value.isRecord) {
      var i = 0
      val n = value.asRecord.length
      val xs = ATag.newArray(n)
      val iter = value.asRecord.iterator
      while (!iter.isEmpty) {
        A.cast(recon)(iter.head.value).foreach { x =>
          xs(i) = x
          i += 1
        }
        iter.step()
      }
      if (i == n) Bind(xs)
      else {
        val ys = ATag.newArray(i)
        java.lang.System.arraycopy(xs, 0, ys, 0, i)
        Bind(ys)
      }
    }
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isRecord) value.asRecord.map { item =>
      if (item.isAttr) recon.Attr(item.asAttr.key, A.norm(recon)(item.value))
      else if (item.isSlot) recon.Slot(item.key, A.norm(recon)(item.value))
      else A.norm(recon)(item.value)
    } (recon.RecordBuilder)
    else recon.Record.empty

  override def toString: String = (String.Builder~"Mold"~'.'~"Array"~'('~>A~", "~>ATag~')').state
}

private[recon] final class ContainerMold[CC[X] <: Container[X], A](
    implicit CC: CollectionFactory[CC], A: Mold[A])
  extends Mold[CC[A]] {

  override def unit: CC[A] = CC.empty[A]

  override def form(recon: Recon)(value: CC[A]): recon.Value =
    value.map(A.form(recon)(_))(recon.RecordBuilder)

  override def cast(recon: Recon)(value: recon.Value): Maybe[CC[A]] =
    if (value.isRecord) Bind(value.asRecord.flatMap { item =>
      A.cast(recon)(item.value)
    } (CC.Builder[A]))
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isRecord) value.asRecord.map { item =>
      if (item.isAttr) recon.Attr(item.asAttr.key, A.norm(recon)(item.value))
      else if (item.isSlot) recon.Slot(item.key, A.norm(recon)(item.value))
      else A.norm(recon)(item.value)
    } (recon.RecordBuilder)
    else recon.Record.empty

  override def toString: String = (String.Builder~"Mold"~'.'~"Container"~'('~>CC~", "~>A~')').state
}

private[recon] final class MapMold[CC[X, Y] <: Map[X, Y], A, T](
    implicit CC: MapFactory[CC], A: Mold[A], T: Mold[T])
  extends Mold[CC[A, T]] {

  override def unit: CC[A, T] = CC.empty[A, T]

  override def form(recon: Recon)(value: CC[A, T]): recon.Value =
    value.map(field => recon.Slot(A.form(recon)(field._1), T.form(recon)(field._2)))(recon.RecordBuilder)

  override def cast(recon: Recon)(value: recon.Value): Maybe[CC[A, T]] =
    if (value.isRecord) Bind(value.asRecord.flatMap { item =>
      if (item.isField) {
        val maybeKey = A.cast(recon)(item.key)
        val maybeValue = T.cast(recon)(item.value)
        if (maybeKey.canBind && maybeValue.canBind) Bind(maybeKey.bind -> maybeValue.bind) else Trap
      }
      else Trap
    } (CC.Builder[A, T]))
    else Trap

  override def norm(recon: Recon)(value: recon.Value): recon.Value =
    if (value.isRecord) value.asRecord.map { item =>
      if (item.isAttr) recon.Attr(item.asAttr.key, T.norm(recon)(item.value))
      else if (item.isSlot) recon.Slot(A.norm(recon)(item.key), T.norm(recon)(item.value))
      else T.norm(recon)(item.value)
    } (recon.RecordBuilder)
    else recon.Record.empty

  override def toString: String = (String.Builder~"Mold"~'.'~"Map"~'('~>CC~", "~>A~", "~>T~')').state
}

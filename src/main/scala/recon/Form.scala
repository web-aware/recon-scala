package recon

import basis._
import basis.collections._
import basis.collections.generic._
import basis.data._
import basis.net._
import basis.text._
import basis.util._
import scala.reflect._

trait Form[@specialized(Form.Specialized) T] {
  def unit: T

  def mold(value: T): Value

  def cast(value: Value): Maybe[T]

  def norm(value: Value): Value = value
}

object Form {
  def apply[T](implicit form: Form[T]): form.type = form

  def Byte(unit: Byte): Form[Byte] = new ByteForm(unit)
  def Short(unit: Short): Form[Short] = new ShortForm(unit)
  def Int(unit: Int): Form[Int] = new IntForm(unit)
  def Long(unit: Long): Form[Long] = new LongForm(unit)
  def Float(unit: Float): Form[Float] = new FloatForm(unit)
  def Double(unit: Double): Form[Double] = new DoubleForm(unit)
  def Boolean(unit: Boolean): Form[Boolean] = new BooleanForm(unit)
  def String(unit: String): Form[String] = new StringForm(unit)
  def Uri(unit: Uri): Form[Uri] = new UriForm(unit)

  implicit lazy val Byte: Form[Byte] = new ByteForm(0)
  implicit lazy val Short: Form[Short] = new ShortForm(0)
  implicit lazy val Int: Form[Int] = new IntForm(0)
  implicit lazy val Long: Form[Long] = new LongForm(0L)
  implicit lazy val Float: Form[Float] = new FloatForm(0.0F)
  implicit lazy val Double: Form[Double] = new DoubleForm(0.0)
  implicit lazy val Boolean: Form[Boolean] = new BooleanForm(false)
  implicit lazy val String: Form[String] = new StringForm("")
  implicit lazy val Uri: Form[Uri] = new UriForm(basis.net.Uri.empty)
  implicit lazy val Unit: Form[Unit] = new UnitForm()

  implicit def Data[Data <: Loader](implicit Data: DataFactory[Data]): Form[Data] =
    new DataForm[Data]()(Data)

  implicit def Array[A](implicit A: Form[A], ATag: ClassTag[A]): Form[Array[A]] =
    new ArrayForm[A]()(A, ATag)

  implicit def Container[CC[X] <: Container[X], A](implicit CC: CollectionFactory[CC], A: Form[A]): Form[CC[A]] =
    new ContainerForm[CC, A]()(CC, A)

  implicit def Map[CC[X, Y] <: Map[X, Y], A, T](implicit CC: MapFactory[CC], A: Form[A], T: Form[T]): Form[CC[A, T]] =
    new MapForm[CC, A, T]()(CC, A, T)

  protected[recon] final val Specialized =
    new Specializable.Group((scala.Int, scala.Long, scala.Float, scala.Double))
}

private[recon] final class ByteForm(override val unit: Byte) extends Form[Byte] {
  override def mold(value: Byte): Value = Number(value)

  override def cast(value: Value): Maybe[Byte] =
    if (value.isNumber) Bind(value.asNumber.toByte)
    else if (value.isText)
      try Bind(Number(value.asString).toByte)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(value.target)
    else Trap

  override def norm(value: Value): Value =
    if (value.isNumber) value
    else if (value.isText)
      try Number(value.asString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"Byte"
    if (unit != 0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class ShortForm(override val unit: Short) extends Form[Short] {
  override def mold(value: Short): Value = Number(value)

  override def cast(value: Value): Maybe[Short] =
    if (value.isNumber) Bind(value.asNumber.toShort)
    else if (value.isText)
      try Bind(Number(value.asString).toShort)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(value.target)
    else Trap

  override def norm(value: Value): Value =
    if (value.isNumber) value
    else if (value.isText)
      try Number(value.asString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"Short"
    if (unit != 0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class IntForm(override val unit: Int) extends Form[Int] {
  override def mold(value: Int): Value = Number(value)

  override def cast(value: Value): Maybe[Int] =
    if (value.isNumber) Bind(value.asNumber.toInt)
    else if (value.isText)
      try Bind(Number(value.asString).toInt)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(value.target)
    else Trap

  override def norm(value: Value): Value =
    if (value.isNumber) value
    else if (value.isText)
      try Number(value.asString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"Int"
    if (unit != 0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class LongForm(override val unit: Long) extends Form[Long] {
  override def mold(value: Long): Value = Number(value)

  override def cast(value: Value): Maybe[Long] =
    if (value.isNumber) Bind(value.asNumber.toLong)
    else if (value.isText)
      try Bind(Number(value.asString).toLong)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(value.target)
    else Trap

  override def norm(value: Value): Value =
    if (value.isNumber) value
    else if (value.isText)
      try Number(value.asString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"Long"
    if (unit != 0L) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class FloatForm(override val unit: Float) extends Form[Float] {
  override def mold(value: Float): Value = Number(value)

  override def cast(value: Value): Maybe[Float] =
    if (value.isNumber) Bind(value.asNumber.toFloat)
    else if (value.isText)
      try Bind(Number(value.asString).toFloat)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(value.target)
    else Trap

  override def norm(value: Value): Value =
    if (value.isNumber) value
    else if (value.isText)
      try Number(value.asString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"Float"
    if (unit != 0.0F) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class DoubleForm(override val unit: Double) extends Form[Double] {
  override def mold(value: Double): Value = Number(value)

  override def cast(value: Value): Maybe[Double] =
    if (value.isNumber) Bind(value.asNumber.toFloat)
    else if (value.isText)
      try Bind(Number(value.asString).toDouble)
      catch { case _: NumberFormatException => Trap }
    else if (value.isRecord) cast(value.target)
    else Trap

  override def norm(value: Value): Value =
    if (value.isNumber) value
    else if (value.isText)
      try Number(value.asString)
      catch { case _: NumberFormatException => value }
    else value

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"Double"
    if (unit != 0.0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class BooleanForm(override val unit: Boolean) extends Form[Boolean] {
  override def mold(value: Boolean): Value = if (value) True else False

  override def cast(value: Value): Maybe[Boolean] =
    if (value.isText) value.asString match {
      case "true" => basis.True
      case "false" => basis.False
      case _ => Trap
    }
    else if (value.isRecord) cast(value.target)
    else Trap

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"Boolean"
    if (unit) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class StringForm(override val unit: String) extends Form[String] {
  override def mold(value: String): Value = Text(value)

  override def cast(value: Value): Maybe[String] =
    if (value.isText) Bind(value.asString)
    else if (value.isNumber) Bind(value.asNumber.toDecimalString)
    else if (value.isRecord) cast(value.target)
    else Trap

  override def norm(value: Value): Value =
    if (value.isText) value
    else if (value.isNumber) Text(value.asNumber.toDecimalString)
    else value

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"String"
    if (unit.length > 0) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class UriForm(override val unit: Uri) extends Form[Uri] {
  override def mold(value: Uri): Value = Text(value.toUriString)

  override def cast(value: recon.Value): Maybe[Uri] =
    if (value.isText)
      try Bind(Uri(value.asString))
      catch { case _: UriException => Trap }
    else Trap

  override def toString: String = {
    val s = String.Builder~"Form"~'.'~"Uri"
    if (unit.isDefined) s~'('~>unit~')'
    s.state
  }
}

private[recon] final class UnitForm extends Form[Unit] {
  override def unit: Unit = ()

  override def mold(value: Unit): Value = Absent

  override def cast(value: Value): Maybe[Unit] = Trap

  override def toString: String = (String.Builder~"Form"~'.'~"Unit").state
}

private[recon] final class DataForm[Data <: Loader](implicit Data: DataFactory[Data]) extends Form[Data] {
  override def unit: Data = Data.empty

  override def mold(value: Data): Value = recon.Data.from(value)

  override def cast(value: Value): Maybe[Data] =
    if (value.isData) Bind(Data.from(value.asData))
    else if (value.isRecord) cast(value.target)
    else Trap

  override def toString: String = (String.Builder~"Form"~'.'~"Data"~'('~>Data~')').state
}

private[recon] final class ArrayForm[A](implicit A: Form[A], ATag: ClassTag[A]) extends Form[Array[A]] {
  private[this] var empty: Array[A] = _

  override def unit: Array[A] = {
    if (empty eq null) empty = ATag.newArray(0)
    empty
  }

  override def mold(value: Array[A]): Value =
    value.map(A.mold)(Record.Builder)

  override def cast(value: Value): Maybe[Array[A]] =
    if (value.isRecord) {
      var i = 0
      val n = value.asRecord.length
      val xs = ATag.newArray(n)
      val iter = value.asRecord.iterator
      while (!iter.isEmpty) {
        A.cast(iter.head.value).foreach { x =>
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
    else {
      val x = A.cast(value)
      if (x.canBind) {
        val xs = ATag.newArray(1)
        xs(0) = x.bind
        Bind(xs)
      }
      else Trap
    }

  override def norm(value: Value): Value =
    if (value.isRecord) value.asRecord.map { item =>
      if (item.isAttr) Attr(item.asAttr.key, A.norm(item.value))
      else if (item.isSlot) Slot(item.key, A.norm(item.value))
      else A.norm(item.value)
    } (Record.Builder)
    else Record.empty

  override def toString: String = (String.Builder~"Form"~'.'~"Array"~'('~>A~", "~>ATag~')').state
}

private[recon] final class ContainerForm[CC[X] <: Container[X], A](
    implicit CC: CollectionFactory[CC], A: Form[A])
  extends Form[CC[A]] {

  override def unit: CC[A] = CC.empty[A]

  override def mold(value: CC[A]): Value =
    value.map(A.mold)(Record.Builder)

  override def cast(value: Value): Maybe[CC[A]] =
    if (value.isRecord) Bind(value.asRecord.flatMap { item =>
      A.cast(item.value)
    } (CC.Builder[A]))
    else {
      val x = A.cast(value)
      if (x.canBind) Bind(CC(x.bind))
      else Trap
    }

  override def norm(value: Value): Value =
    if (value.isRecord) value.asRecord.map { item =>
      if (item.isAttr) Attr(item.asAttr.key, A.norm(item.value))
      else if (item.isSlot) Slot(item.key, A.norm(item.value))
      else A.norm(item.value)
    } (Record.Builder)
    else Record.empty

  override def toString: String = (String.Builder~"Form"~'.'~"Container"~'('~>CC~", "~>A~')').state
}

private[recon] final class MapForm[CC[X, Y] <: Map[X, Y], A, T](
    implicit CC: MapFactory[CC], A: Form[A], T: Form[T])
  extends Form[CC[A, T]] {

  override def unit: CC[A, T] = CC.empty[A, T]

  override def mold(value: CC[A, T]): Value =
    value.map(field => Slot(A.mold(field._1), T.mold(field._2)))(Record.Builder)

  override def cast(value: Value): Maybe[CC[A, T]] =
    if (value.isRecord) Bind(value.asRecord.flatMap { item =>
      if (item.isField) {
        val maybeKey = A.cast(item.key)
        val maybeValue = T.cast(item.value)
        if (maybeKey.canBind && maybeValue.canBind) Bind(maybeKey.bind -> maybeValue.bind) else Trap
      }
      else Trap
    } (CC.Builder[A, T]))
    else Trap

  override def norm(value: Value): Value =
    if (value.isRecord) value.asRecord.map { item =>
      if (item.isAttr) Attr(item.asAttr.key, T.norm(item.value))
      else if (item.isSlot) Slot(A.norm(item.key), T.norm(item.value))
      else T.norm(item.value)
    } (Record.Builder)
    else Record.empty

  override def toString: String = (String.Builder~"Form"~'.'~"Map"~'('~>CC~", "~>A~", "~>T~')').state
}

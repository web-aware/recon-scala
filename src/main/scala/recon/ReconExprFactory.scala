package recon

import basis._
import basis.collections._
import basis.data._
import basis.text._
import scala.reflect.macros._

private[recon] class ReconExprFactory[C <: blackbox.Context](val c: C)
  extends ReconFactory with ReconParser {

  import c.{ mirror, Expr, Tree, WeakTypeTag }
  import c.universe._

  override type Item = Expr[recon.Item]
  override type Field = Expr[recon.Field]
  override type Attr = Expr[recon.Attr]
  override type Slot = Expr[recon.Slot]
  override type Value = Expr[recon.Value]
  override type Record = Expr[recon.Record]
  override type Text = Expr[recon.Text]
  override type Data = Expr[recon.Data]
  override type Number = Expr[recon.Number]
  override type Extant = Expr[recon.Extant.type]
  override type Absent = Expr[recon.Absent.type]

  implicit protected def ItemTag = WeakTypeTag[recon.Item](mirror.staticClass("recon.Item").toType)
  implicit protected def FieldTag = WeakTypeTag[recon.Field](mirror.staticClass("recon.Field").toType)
  implicit protected def AttrTag = WeakTypeTag[recon.Attr](mirror.staticClass("recon.Attr").toType)
  implicit protected def SlotTag = WeakTypeTag[recon.Slot](mirror.staticClass("recon.Slot").toType)
  implicit protected def ValueTag = WeakTypeTag[recon.Value](mirror.staticClass("recon.Value").toType)
  implicit protected def RecordTag = WeakTypeTag[recon.Record](mirror.staticClass("recon.Record").toType)
  implicit protected def TextTag = WeakTypeTag[recon.Text](mirror.staticClass("recon.Text").toType)
  implicit protected def DataTag = WeakTypeTag[recon.Data](mirror.staticClass("recon.Data").toType)
  implicit protected def NumberTag = WeakTypeTag[recon.Number](mirror.staticClass("recon.Number").toType)
  implicit protected def ExtantTag = WeakTypeTag[recon.Extant.type](mirror.staticModule("recon.Extant").moduleClass.asClass.toType)
  implicit protected def AbsentTag = WeakTypeTag[recon.Absent.type](mirror.staticModule("recon.Absent").moduleClass.asClass.toType)

  override def Attr(key: Expr[recon.Text], value: Expr[recon.Value]) =
    Expr[recon.Attr](q"_root_.recon.Attr($key, $value)")
  override def Attr(key: Expr[recon.Text]) =
    Expr[recon.Attr](q"_root_.recon.Attr($key)")

  override def Slot(key: Expr[recon.Value], value: Expr[recon.Value]) =
    Expr[recon.Slot](q"_root_.recon.Slot($key, $value)")
  override def Slot(key: Expr[recon.Value]) =
    Expr[recon.Slot](q"_root_.recon.Slot($key)")

  override def ValueBuilder: ItemBuilder with State[Expr[recon.Value]] = new ValueBuilder()
  override def RecordBuilder: ItemBuilder with State[Expr[recon.Record]] = new RecordBuilder()
  override def TextBuilder: StringBuilder with State[Expr[recon.Text]] = new TextBuilder()
  override def DataFramer: Framer with State[Expr[recon.Data]] = new DataFramer()

  override def Number(value: String) =
    Expr[recon.Number](q"_root_.recon.Number(${NumberLiteral(value)})")

  override def True = Expr[recon.Value](q"_root_.recon.True")
  override def False = Expr[recon.Value](q"_root_.recon.False")

  override def Extant = Expr[recon.Extant.type](q"_root_.recon.Extant")
  override def Absent = Expr[recon.Absent.type](q"_root_.recon.Absent")

  private final class ValueBuilder extends ItemBuilder with State[Expr[recon.Value]] {
    private[this] var builder: ItemBuilder with basis.State[Expr[recon.Record]] = null
    private[this] var value: Expr[recon.Value] = null

    override def appendField(item: Expr[recon.Field]): Unit = {
      if (builder ne null) builder.appendField(item)
      else {
        builder = RecordBuilder
        if (value ne null) {
          builder.appendValue(value)
          value = null
        }
        builder.appendField(item)
      }
    }

    override def appendValue(item: Expr[recon.Value]): Unit = {
      if (builder ne null) builder.appendValue(item)
      else if (value eq null) value = item
      else {
        builder = RecordBuilder
        builder.appendValue(value)
        value = null
        builder.appendValue(item)
      }
    }

    override def state: Expr[recon.Value] = {
      if (value ne null) value
      else if (builder ne null) builder.state
      else Absent
    }
  }

  private final class RecordBuilder extends ItemBuilder with State[Expr[recon.Record]] {
    private[this] val self = mutable.ArrayBuffer.empty[Expr[recon.Item]]
    override def appendField(item: Expr[recon.Field]): Unit = self.append(item)
    override def appendValue(item: Expr[recon.Value]): Unit = self.append(item)
    override def state = Expr[recon.Record] {
      if (self.isEmpty) q"_root_.recon.Record.empty"
      else {
        val builder = self.foldLeft(q"_root_.recon.Record.Builder": Tree)((b, e) => q"$b += $e")
        q"$builder.state"
      }
    }
  }

  private final class TextBuilder extends StringBuilder with State[Expr[recon.Text]] {
    private[this] val self: StringBuilder with basis.State[String] = String.Builder
    override def append(c: Int): Unit = self.append(c)
    override def append(cs: CharSequence): Unit = self.append(cs)
    override def clear(): Unit = self.clear()
    override def expect(count: Int): this.type = { self.expect(count); this }
    override def state: Expr[recon.Text] = {
      val text = self.state
      Expr[recon.Text](if (text.length == 0) q"_root_.recon.Text.empty" else q"_root_.recon.Text($text)")
    }
  }

  private final class DataFramer extends Framer with State[Data] {
    private[this] val self = ArrayData.Framer
    override def endian: Endianness = self.endian
    override def isEOF: Boolean = self.isEOF
    override def writeByte(value: Byte): Unit = self.writeByte(value)
    override def writeShort(value: Short): Unit = self.writeShort(value)
    override def writeInt(value: Int): Unit = self.writeInt(value)
    override def writeLong(value: Long): Unit = self.writeLong(value)
    override def writeFloat(value: Float): Unit = self.writeFloat(value)
    override def writeDouble(value: Double): Unit = self.writeDouble(value)
    override def writeData(data: Loader): Unit = self.writeData(data)
    override def clear(): Unit = self.clear()
    override def expect(count: Long): this.type = { self.expect(count); this }
    override def state: Expr[recon.Data] = {
      val data = self.state
      Expr[recon.Data] {
        if (data.size == 0L) q"_root_.recon.Data.empty"
        else q"new _root_.basis.data.DataFactoryOps(_root_.recon.Data).fromBase64(${data.toBase64})"
      }
    }
  }

  protected def NumberLiteral(value: String): Tree =
    try Literal(Constant(java.lang.Integer.parseInt(value)))
    catch {
      case _: NumberFormatException =>
        try Literal(Constant(java.lang.Long.parseLong(value)))
        catch {
          case _: NumberFormatException =>
            try Literal(Constant(java.lang.Double.parseDouble(value)))
            catch {
              case _: NumberFormatException =>
                Literal(Constant(value))
            }
        }
    }
}

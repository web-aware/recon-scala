package recon

import basis._
import basis.collections._
import basis.data._
import basis.text._
import scala.reflect.macros._

private[recon] class ReconExprFactory[C <: blackbox.Context, R <: Recon]
    (val c: C)(_recon: C#Expr[R])
  extends ReconFactory with ReconParser {

  import c.{ Expr, Tree, WeakTypeTag }
  import c.universe._
  import c.universe.internal._

  protected val recon: Expr[R] = _recon.asInstanceOf[Expr[R]]

  override type Item = Expr[R#Item]
  override type Field = Expr[R#Field]
  override type Attr = Expr[R#Attr]
  override type Slot = Expr[R#Slot]
  override type Value = Expr[R#Value]
  override type Record = Expr[R#Record]
  override type Text = Expr[R#Text]
  override type Data = Expr[R#Data]
  override type Number = Expr[R#Number]
  override type Extant = Expr[R#Extant]
  override type Absent = Expr[R#Absent]

  implicit protected def ItemTag = WeakTypeTag[R#Item](ReconType("Item"))
  implicit protected def FieldTag = WeakTypeTag[R#Field](ReconType("Field"))
  implicit protected def AttrTag = WeakTypeTag[R#Attr](ReconType("Attr"))
  implicit protected def SlotTag = WeakTypeTag[R#Slot](ReconType("Slot"))
  implicit protected def ValueTag = WeakTypeTag[R#Value](ReconType("Value"))
  implicit protected def RecordTag = WeakTypeTag[R#Record](ReconType("Record"))
  implicit protected def TextTag = WeakTypeTag[R#Text](ReconType("Text"))
  implicit protected def DataTag = WeakTypeTag[R#Data](ReconType("Data"))
  implicit protected def NumberTag = WeakTypeTag[R#Number](ReconType("Number"))
  implicit protected def ExtantTag = WeakTypeTag[R#Extant](ReconType("Extant"))
  implicit protected def AbsentTag = WeakTypeTag[R#Absent](ReconType("Absent"))

  override def Attr(key: Expr[R#Text], value: Expr[R#Value]) = Expr[R#Attr](q"$recon.Attr($key, $value)")
  override def Attr(key: Expr[R#Text]) = Expr[R#Attr](q"$recon.Attr($key)")

  override def Slot(key: Expr[R#Value], value: Expr[R#Value]) = Expr[R#Slot](q"$recon.Slot($key, $value)")
  override def Slot(key: Expr[R#Value]) = Expr[R#Slot](q"$recon.Slot($key)")

  override def ValueBuilder: ItemBuilder with State[Expr[R#Value]] = new ValueBuilder()
  override def RecordBuilder: ItemBuilder with State[Expr[R#Record]] = new RecordBuilder()
  override def TextBuilder: StringBuilder with State[Expr[R#Text]] = new TextBuilder()
  override def DataFramer: Framer with State[Expr[R#Data]] = new DataFramer()

  override def Number(value: String) = Expr[R#Number](q"$recon.Number(${NumberLiteral(value)})")

  override def True = Expr[R#Value](q"$recon.True")
  override def False = Expr[R#Value](q"$recon.False")

  override def Extant = Expr[R#Extant](q"$recon.Extant")
  override def Absent = Expr[R#Absent](q"$recon.Absent")

  private final class ValueBuilder extends ItemBuilder with State[Expr[R#Value]] {
    private[this] var builder: ItemBuilder with basis.State[Expr[R#Record]] = null
    private[this] var value: Expr[R#Value] = null

    override def appendField(item: Expr[R#Field]): Unit = {
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

    override def appendValue(item: Expr[R#Value]): Unit = {
      if (builder ne null) builder.appendValue(item)
      else if (value eq null) value = item
      else {
        builder = RecordBuilder
        builder.appendValue(value)
        value = null
        builder.appendValue(item)
      }
    }

    override def state: Expr[R#Value] = {
      if (value ne null) value
      else if (builder ne null) builder.state
      else Absent
    }
  }

  private final class RecordBuilder extends ItemBuilder with State[Expr[R#Record]] {
    private[this] val self = mutable.ArrayBuffer.empty[Expr[R#Item]]
    override def appendField(item: Expr[R#Field]): Unit = self.append(item)
    override def appendValue(item: Expr[R#Value]): Unit = self.append(item)
    override def state = Expr[R#Record] {
      if (self.isEmpty) q"$recon.Record.empty"
      else {
        val builder = self.foldLeft(q"$recon.RecordBuilder": Tree)((b, e) => q"$b += $e")
        q"$builder.state"
      }
    }
  }

  private final class TextBuilder extends StringBuilder with State[Expr[R#Text]] {
    private[this] val self: StringBuilder with basis.State[String] = String.Builder
    override def append(c: Int): Unit = self.append(c)
    override def append(cs: CharSequence): Unit = self.append(cs)
    override def clear(): Unit = self.clear()
    override def expect(count: Int): this.type = { self.expect(count); this }
    override def state: Expr[R#Text] = {
      val text = self.state
      Expr[R#Text](if (text.length == 0) q"$recon.Text.empty" else q"$recon.Text($text)")
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
    override def state: Expr[R#Data] = {
      val data = self.state
      Expr[R#Data] {
        if (data.size == 0L) q"$recon.Data.empty"
        else q"new _root_.basis.data.DataFactoryOps($recon.Data).fromBase64(${data.toBase64})"
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

  protected def ReconType(name: String): Type =
    typeRef(recon.staticType, recon.staticType.member(TypeName(name)), Nil).dealias
}

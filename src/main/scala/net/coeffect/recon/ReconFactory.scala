package net.coeffect.recon

import basis._
import basis.data._
import basis.text._

private[recon] trait ReconFactory { ReconFactory =>
  private[recon] type Item
  private[recon] type Field <: Item
  private[recon] type Attr <: Field
  private[recon] type Slot <: Field
  private[recon] type Value <: Item
  private[recon] type Record <: Value
  private[recon] type Text <: Value
  private[recon] type Data <: Value
  private[recon] type Number <: Value
  private[recon] type Bool <: Value
  private[recon] type Extant <: Value
  private[recon] type Absent <: Value

  private[recon] def Attr(name: String, value: Value): Attr
  private[recon] def Attr(name: String): Attr

  private[recon] def Slot(name: String, value: Value): Slot
  private[recon] def Slot(name: String): Slot

  private[recon] def ValueBuilder: ItemBuilder with State[Value]
  private[recon] def RecordBuilder: ItemBuilder with State[Record]
  private[recon] def TextBuilder: StringBuilder with State[Text]
  private[recon] def DataBuilder: StringBuilder with State[Data] = new DataBuilder(DataFramer)
  private[recon] def DataFramer: Framer with State[Data]

  private[recon] def Number(value: String): Number

  private[recon] def True: Bool
  private[recon] def False: Bool

  private[recon] def Extant: Extant
  private[recon] def Absent: Absent

  private[recon] trait ItemBuilder extends State[Any] {
    def appendField(field: Field): Unit
    def appendValue(value: Value): Unit
    def state: State
  }

  private[recon] final class DataBuilder(self: Framer with State[Data]) extends StringBuilder with State[Data] {
    private[this] var p: Int = 0
    private[this] var q: Int = 0
    private[this] var r: Int = 0
    private[this] var s: Int = 0

    protected def decodeDigit(c: Int): Int = {
      if (c >= 'A' && c <= 'Z') c - 'A'
      else if (c >= 'a' && c <= 'z') c + (26 - 'a')
      else if (c >= '0' && c <= '9') c + (52 - '0')
      else if (c == '+' || c == '-') 62
      else if (c == '/' || c == '_') 63
      else throw new IllegalArgumentException
    }

    protected def decodeQuantum(): Unit = {
      val x = decodeDigit(p)
      val y = decodeDigit(q)
      if (r != '=') {
        val z = decodeDigit(r)
        if (s != '=') {
          val w = decodeDigit(s)
          self.writeByte(((x << 2) | (y >>> 4)).toByte)
          self.writeByte(((y << 4) | (z >>> 2)).toByte)
          self.writeByte(((z << 6) | w).toByte)
        }
        else {
          self.writeByte(((x << 2) | (y >>> 4)).toByte)
          self.writeByte(((y << 4) | (z >>> 2)).toByte)
        }
      }
      else {
        if (s != '=') throw new IllegalArgumentException
        self.writeByte(((x << 2) | (y >>> 4)).toByte)
      }
    }

    override def append(c: Int): Unit = {
      if (p == 0) p = c
      else if (q == 0) q = c
      else if (r == 0) r = c
      else {
        s = c
        decodeQuantum()
        s = 0
        r = 0
        q = 0
        p = 0
      }
    }

    override def clear(): Unit = {
      self.clear()
      s = 0
      r = 0
      q = 0
      p = 0
    }

    override def expect(count: Int): this.type = this

    override def state: Data = self.state

    override def toString: String = (String.Builder~ReconFactory.toString~'.'~"DataBuilder").state
  }
}

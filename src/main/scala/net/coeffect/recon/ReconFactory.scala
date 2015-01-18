package net.coeffect.recon

import basis._
import basis.text._

private[recon] trait ReconFactory {
  private[recon] type Item
  private[recon] type Field <: Item
  private[recon] type Attr <: Field
  private[recon] type Slot <: Field
  private[recon] type Value <: Item
  private[recon] type Record <: Value
  private[recon] type Text <: Value
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
}

package net.coeffect

import basis._
import basis.collections._
import basis.text._

package object recon {
  implicit def RecordBuilder: Builder[Item] with From[Record] with State[Record] = Record.Builder
  implicit def TextBuilder: StringBuilder with From[Text] with State[Text] = Text.Builder

  implicit def StringToText: String => Text = Recon.StringToText
  implicit def IntToNumber: Int => Number = Recon.IntToNumber
  implicit def LongToNumber: Long => Number = Recon.LongToNumber
  implicit def FloatToNumber: Float => Number = Recon.FloatToNumber
  implicit def DoubleToNumber: Double => Number = Recon.DoubleToNumber
  implicit def BooleanToBool: Boolean => Bool = Recon.BooleanToBool

  implicit def ReconStringContext(stringContext: StringContext): ReconStringContext[Recon.type] =
    macro ReconMacros.globalReconStringContext

  def ReconParser: Recon.ReconParser.type = Recon.ReconParser
}

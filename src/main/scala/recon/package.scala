import basis.text._
import scala.runtime._

package object recon {
  lazy val True: Value = new Text(new UString("true"))
  lazy val False: Value = new Text(new UString("false"))

  implicit lazy val StringToRecon: String => Text = new StringToRecon()
  implicit lazy val IntToRecon: Int => Number = new IntToRecon()
  implicit lazy val LongToRecon: Long => Number = new LongToRecon()
  implicit lazy val FloatToRecon: Float => Number = new FloatToRecon()
  implicit lazy val DoubleToRecon: Double => Number = new DoubleToRecon()
  implicit lazy val BooleanToRecon: Boolean => Value = new BooleanToRecon()

  implicit def ReconStringContext(stringContext: StringContext): ReconStringContext =
    macro ReconMacros.ReconStringContext
}

package recon {
  private[recon] final class StringToRecon extends AbstractFunction1[String, Text] {
    override def apply(value: String): Text = Text(value)
    override def toString: String = "StringToRecon"
  }

  private[recon] final class IntToRecon extends AbstractFunction1[Int, Number] {
    override def apply(value: Int): Number = Number(value)
    override def toString: String = "IntToRecon"
  }

  private[recon] final class LongToRecon extends AbstractFunction1[Long, Number] {
    override def apply(value: Long): Number = Number(value)
    override def toString: String = "LongToRecon"
  }

  private[recon] final class FloatToRecon extends AbstractFunction1[Float, Number] {
    override def apply(value: Float): Number = Number(value)
    override def toString: String = "FloatToRecon"
  }

  private[recon] final class DoubleToRecon extends AbstractFunction1[Double, Number] {
    override def apply(value: Double): Number = Number(value)
    override def toString: String = "DoubleToRecon"
  }

  private[recon] final class BooleanToRecon extends AbstractFunction1[Boolean, Value] {
    override def apply(value: Boolean): Value = if (value) True else False
    override def toString: String = "BooleanToRecon"
  }
}

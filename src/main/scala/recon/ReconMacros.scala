package recon

import scala.reflect.macros._

private[recon] class ReconMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, prefix, WeakTypeTag }
  import c.universe._

  def ReconStringContext(stringContext: Expr[StringContext]): Expr[ReconStringContext] = {
    implicit val ReconStringContextTag =
      WeakTypeTag[ReconStringContext](mirror.staticClass("recon.ReconStringContext").toType)
    Expr[ReconStringContext](q"new $ReconStringContextTag($stringContext)")
  }
}

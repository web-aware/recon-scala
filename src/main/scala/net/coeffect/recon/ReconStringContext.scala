package net.coeffect.recon

import basis.collections._
import scala.reflect.macros._

class ReconStringContext[-R <: Recon](recon: R, stringContext: StringContext) {
  def recon(args: R#Value*): R#Value = macro ReconStringContextMacros.recon[R]
}

private[recon] class ReconMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, prefix, WeakTypeTag }
  import c.universe._

  def globalReconStringContext(stringContext: Expr[StringContext]): Expr[ReconStringContext[Recon.type]] = {
    implicit val ReconStringContext =
      WeakTypeTag[ReconStringContext[Recon.type]](
        appliedType(
          mirror.staticClass("net.coeffect.recon.ReconStringContext").toTypeConstructor,
          mirror.staticModule("net.coeffect.recon.Recon").moduleClass.asClass.toType :: Nil))
    Expr[ReconStringContext[Recon.type]](q"new $ReconStringContext(_root_.net.coeffect.recon.Recon, $stringContext)")
  }

  def prefixReconStringContext[R <: Recon](stringContext: Expr[StringContext]): Expr[ReconStringContext[R]] = {
    implicit val ReconStringContextR =
      WeakTypeTag[ReconStringContext[R]](
        appliedType(
          mirror.staticClass("net.coeffect.recon.ReconStringContext").toTypeConstructor,
          (if (prefix.actualType != null) prefix.actualType else prefix.staticType) :: Nil))
    Expr[ReconStringContext[R]](q"new $ReconStringContextR($prefix, $stringContext)")
  }
}

private[recon] class ReconStringContextMacros(val c: blackbox.Context { type PrefixType <: ReconStringContext[_] }) {
  import c.{ abort, Expr, prefix }
  import c.universe._

  def recon[R <: Recon : WeakTypeTag](args: Expr[R#Value]*): Expr[R#Value] = {
    val Typed(Apply(_, recon :: stringContext :: Nil), _) = prefix.tree
    val Apply(_, stringLiterals) = stringContext
    val literals = stringLiterals.iterator
    val values = args.iterator

    val factory = new ReconExprFactory[c.type, R](c)(Expr[R](recon))
    var parser = factory.DocumentParser: Iteratee[Int, Expr[R#Value]]

    var input = null: LiteralIterator[c.type]
    while (literals.hasNext && parser.isCont) {
      input = new LiteralIterator(c, literals.next())
      while (!input.isEmpty && parser.isCont) parser = parser.feed(input)
      if (values.hasNext && parser.isCont)
        parser = parser.asInstanceOf[factory.Parser[Expr[R#Value]]].interpolate(values.next())
    }
    if (!literals.hasNext && parser.isCont) parser = parser.feed(Iterator.done)
    if (parser.isDone) parser.bind
    else parser.trap match {
      case ex: ReconException => abort(input.pos, ex.getMessage)
      case error => abort(input.pos, error.toString)
    }
  }
}

private final class LiteralIterator[C <: blackbox.Context](
    val c: C,
    _literal: C#Tree,
    private[this] var index: Int)
  extends Iterator[Int] {

  def this(c: C, _literal: C#Tree) = this(c, _literal, 0)

  import c.universe._

  private[this] val literal: Tree = _literal.asInstanceOf[Tree]

  private[this] val Literal(Constant(string: String)) = literal

  def pos: Position = literal.pos.withPoint(literal.pos.point + index)

  override def isEmpty: Boolean = index >= string.length

  override def head: Int = {
    if (index >= string.length) Iterator.empty.head
    string.codePointAt(index)
  }

  override def step(): Unit = {
    if (index >= string.length) Iterator.empty.step()
    index = string.offsetByCodePoints(index, 1)
  }

  override def dup: Iterator[Int] = new LiteralIterator[C](c, literal, index)
}

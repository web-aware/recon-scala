package net.coeffect.recon

import basis.collections._
import scala.reflect.macros._

class ReconStringContext[-R <: Recon](recon: R, stringContext: StringContext) {
  def recon(args: R#Value*): R#Value = macro ReconStringContextMacros.recon[R]
}

private[recon] class ReconMacros(val c: blackbox.Context { type PrefixType <: Recon }) {
  import c.{ Expr, mirror, prefix, WeakTypeTag }
  import c.universe._

  def ReconStringContext[R <: Recon](stringContext: Expr[StringContext]): Expr[ReconStringContext[R]] = {
    implicit val ReconStringContextR = ReconStringContextTag[R]
    Expr[ReconStringContext[R]](q"new $ReconStringContextR($prefix, $stringContext)")
  }

  implicit private def ReconStringContextTag[R <: Recon]: WeakTypeTag[ReconStringContext[R]] =
    WeakTypeTag[ReconStringContext[R]](
      appliedType(
        mirror.staticClass("net.coeffect.recon.ReconStringContext").toTypeConstructor,
        (if (prefix.actualType != null) prefix.actualType else prefix.staticType) :: Nil))
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

    var input = null: LiteralIterator
    while (literals.hasNext && parser.isCont) {
      input = new LiteralIterator(literals.next())
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

  private final class LiteralIterator(literal: Tree, private[this] var index: Int) extends Iterator[Int] {
    def this(literal: Tree) = this(literal, 0)

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

    override def dup: Iterator[Int] = new LiteralIterator(literal, index)
  }
}

package recon

import basis.collections._
import scala.reflect.macros._

class ReconStringContext(stringContext: StringContext) {
  def recon(args: Value*): Value = macro ReconStringContextMacros.recon
}

private[recon] class ReconStringContextMacros(val c: blackbox.Context { type PrefixType <: ReconStringContext }) {
  import c.{ abort, Expr, prefix }
  import c.universe._

  def recon(args: Expr[Value]*): Expr[Value] = {
    val Typed(Apply(_, stringContext :: Nil), _) = prefix.tree
    val Apply(_, stringLiterals) = stringContext
    val literals = stringLiterals.iterator
    val values = args.iterator

    val factory = new ReconExprFactory[c.type](c)
    var parser = factory.DocumentParser: Iteratee[Int, Expr[Value]]

    var input = null: LiteralIterator[c.type]
    while (literals.hasNext && parser.isCont) {
      input = new LiteralIterator(c, literals.next())
      while (!input.isEmpty && parser.isCont)
        parser = parser.feed(input)
      if (values.hasNext && parser.isCont)
        parser = parser.asInstanceOf[factory.Parser[Expr[Value]]].interpolate(values.next())
    }
    if (!literals.hasNext && parser.isCont)
      parser = parser.feed(Iterator.done)
    if (parser.isDone) parser.bind
    else parser.trap match {
      case ex: ReconException => abort(input.pos, ex.getMessage)
      case error => abort(input.pos, error.toString)
    }
  }
}

private[recon] final class LiteralIterator[C <: blackbox.Context](
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

package net.coeffect.recon

import basis._
import basis.collections._
import basis.text._

private[recon] trait ReconParser extends ReconFactory { ReconParser =>
  lazy val DocumentParser: Iteratee[Int, Value] = new DocumentParser()

  lazy val BlockParser: Iteratee[Int, Value] = new BlockParser()

  lazy val ItemParser: Iteratee[Int, Item] = new ItemParser()

  lazy val NameParser: Iteratee[Int, String] = new NameParser()

  lazy val AttrParser: Iteratee[Int, Attr] = new AttrParser()

  lazy val SlotParser: Iteratee[Int, Slot] = new SlotParser()

  lazy val BlockValueParser: Iteratee[Int, Value] = new BlockValueParser()

  lazy val InlineValueParser: Iteratee[Int, Value] = new InlineValueParser()

  lazy val RecordParser: Iteratee[Int, Value] = new RecordParser()

  lazy val MarkupParser: Iteratee[Int, Value] = new MarkupParser()

  lazy val StringParser: Iteratee[Int, Text] = new StringParser()

  lazy val DataParser: Iteratee[Int, Data] = new DataParser()

  lazy val NumberParser: Iteratee[Int, Number] = new NumberParser()

  lazy val TokenParser: Iteratee[Int, Value] = new TokenParser()


  private def isSpace(c: Int): Boolean = c == 0x20 || c == 0x9

  private def isNewline(c: Int): Boolean = c == 0xA || c == 0xD

  private def isWhitespace(c: Int): Boolean = isSpace(c) || isNewline(c)

  private def isNameStartChar(c: Int): Boolean =
    c >= 'A' && c <= 'Z' ||
    c == '_' ||
    c >= 'a' && c <= 'z' ||
    c >= 0xC0 && c <= 0xD6 ||
    c >= 0xD8 && c <= 0xF6 ||
    c >= 0xF8 && c <= 0x2FF ||
    c >= 0x370 && c <= 0x37D ||
    c >= 0x37F && c <= 0x1FFF ||
    c >= 0x200C && c <= 0x200D ||
    c >= 0x2070 && c <= 0x218F ||
    c >= 0x2C00 && c <= 0x2FEF ||
    c >= 0x3001 && c <= 0xD7FF ||
    c >= 0xF900 && c <= 0xFDCF ||
    c >= 0xFDF0 && c <= 0xFFFD ||
    c >= 0x10000 && c <= 0xEFFFF

  private def isNameChar(c: Int): Boolean =
    c == '-' ||
    c >= '0' && c <= '9' ||
    c >= 'A' && c <= 'Z' ||
    c == '_' ||
    c >= 'a' && c <= 'z' ||
    c == 0xB7 ||
    c >= 0xC0 && c <= 0xD6 ||
    c >= 0xD8 && c <= 0xF6 ||
    c >= 0xF8 && c <= 0x37D ||
    c >= 0x37F && c <= 0x1FFF ||
    c >= 0x200C && c <= 0x200D ||
    c >= 0x203F && c <= 0x2040 ||
    c >= 0x2070 && c <= 0x218F ||
    c >= 0x2C00 && c <= 0x2FEF ||
    c >= 0x3001 && c <= 0xD7FF ||
    c >= 0xF900 && c <= 0xFDCF ||
    c >= 0xFDF0 && c <= 0xFFFD ||
    c >= 0x10000 && c <= 0xEFFFF

  private def isBase64Char(c: Int): Boolean =
    c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '0' && c <= '9' ||
    c == '+' || c == '-' ||
    c == '/' || c == '_'


  private[recon] abstract class Parser[+O] extends Iteratee[Int, O] {
    def interpolate(value: Value): Iteratee[Int, O] =
      Iteratee.error(new ReconException("invalid substitution", Iterator.done))
  }


  private final class DocumentParser(value: Iteratee[Int, Value]) extends Parser[Value] {
    def this() = this(BlockParser)

    override def feed(input: Iterator[Int]): Iteratee[Int, Value] = {
      var value = this.value
      while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
      if (value.isError) return value.asError
      if (value.isDone) {
        if (!input.isEmpty) return error(input, expected = 0, found = input.head)
        else if (input.isDone) return value
      }
      new DocumentParser(value)
    }

    override def interpolate(item: Value): Iteratee[Int, Value] = {
      val value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
      if (value.isError) value.asError
      else new DocumentParser(value)
    }

    override def state: Maybe[Value] = value.state

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"DocumentParser").state
  }


  private final class BlockParser(
      builder: ItemBuilder with State[Value],
      field: Iteratee[Int, Field],
      value: Iteratee[Int, Value],
      s: Int)
    extends Parser[Value] {

    def this() = this(null, null, null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Value] = {
      var c = 0
      var s = this.s
      var value = this.value
      var field = this.field
      val builder = if (this.builder ne null) this.builder else ValueBuilder
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          while (!input.isEmpty && { c = input.head; isWhitespace(c) }) input.step()
          if (!input.isEmpty) {
            if (isNameStartChar(c)) {
              field = SlotParser
              s = 2
            }
            else {
              value = BlockValueParser
              s = 3
            }
          }
          else if (input.isDone) return Iteratee.done(builder.state)
        }
        if (s == 2) {
          while ((!input.isEmpty || input.isDone) && field.isCont) field = field.feed(input)
          if (field.isDone) {
            builder.appendField(field.bind)
            field = null
            s = 4
          }
          else if (field.isError) return field.asError
        }
        if (s == 3) {
          while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
          if (value.isDone) {
            builder.appendValue(value.bind)
            value = null
            s = 4
          }
          else if (value.isError) return value.asError
        }
        if (s == 4) {
          while (!input.isEmpty && { c = input.head; isSpace(c) }) input.step()
          if (!input.isEmpty) {
            if (c == ',' || c == ';' || isNewline(c)) {
              input.step()
              s = 1
            }
            else return Iteratee.done(builder.state)
          }
          else if (input.isDone) return Iteratee.done(builder.state)
        }
      }
      new BlockParser(builder, field, value, s)
    }

    override def interpolate(item: Value): Iteratee[Int, Value] = {
      var s = this.s
      if (s == 1) {
        val builder = if (this.builder ne null) this.builder else ValueBuilder
        builder.appendValue(item)
        s = 4
        new BlockParser(builder, field, value, s)
      }
      else if (s == 2) {
        var field = this.field.asInstanceOf[Parser[Field]].interpolate(item)
        if (field.isError) return field.asError
        if (field.isDone) {
          builder.appendField(field.bind)
          field = null
          s = 4
        }
        new BlockParser(builder, field, value, s)
      }
      else if (s == 3) {
        var value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        if (value.isDone) {
          builder.appendValue(value.bind)
          value = null
          s = 4
        }
        new BlockParser(builder, field, value, s)
      }
      else super.interpolate(item)
    }

    override def state: Maybe[Value] = if (builder ne null) Bind(builder.state) else Trap

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"BlockParser").state
  }


  private final class ItemParser extends Parser[Item] {
    override def feed(input: Iterator[Int]): Iteratee[Int, Item] = {
      if (!input.isEmpty) {
        val c = input.head
        if (isNameStartChar(c)) SlotParser.feed(input)
        else BlockValueParser.feed(input)
      }
      else this
    }

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"ItemParser").state
  }


  private final class NameParser(
      builder: StringBuilder with State[String],
      s: Int)
    extends Parser[String] {

    def this() = this(null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, String] = {
      var c = 0
      var s = this.s
      var builder = this.builder
      if (s == 1) {
        if (!input.isEmpty && { c = input.head; isNameStartChar(c) }) {
          if (builder eq null) builder = String.Builder
          input.step()
          builder.append(c)
          s = 2
        }
        else if (!input.isEmpty) return error(input, expected = "name", found = c)
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 2) {
        while (!input.isEmpty && { c = input.head; isNameChar(c) }) {
          input.step()
          builder.append(c)
        }
        if (!input.isEmpty || input.isDone) return Iteratee.done(builder.state)
      }
      new NameParser(builder, s)
    }

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"NameParser").state
  }


  private final class AttrParser(
      name: Iteratee[Int, String],
      value: Iteratee[Int, Value],
      s: Int)
    extends Parser[Attr] {

    def this() = this(NameParser, BlockParser, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Attr] = {
      var c = 0
      var s = this.s
      var value = this.value
      var name = this.name
      if (s == 1) {
        if (!input.isEmpty && { c = input.head; c == '@' }) {
          input.step()
          s = 2
        }
        else if (!input.isEmpty) return error(input, expected = '@', found = c)
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 2) {
        name = name.feed(input)
        if (name.isDone) s = 3
        else if (name.isError) return name.asError
      }
      if (s == 3) {
        if (!input.isEmpty && input.head == '(') {
          input.step()
          s = 4
        }
        else if (!input.isEmpty || input.isDone) return Iteratee.done(Attr(name.bind))
      }
      if (s == 4) {
        while (!input.isEmpty && { c = input.head; isWhitespace(c) }) input.step()
        if (!input.isEmpty) {
          if (c == ')') {
            input.step()
            return Iteratee.done(Attr(name.bind))
          }
          else s = 5
        }
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 5) {
        while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
        if (value.isDone) s = 6
        else if (value.isError) return value.asError
      }
      if (s == 6) {
        while (!input.isEmpty && { c = input.head; isWhitespace(c) }) input.step()
        if (!input.isEmpty) {
          if (c == ')') {
            input.step()
            return Iteratee.done(Attr(name.bind, value.bind))
          }
          else return error(input, expected = ')', found = c)
        }
        else if (input.isDone) return unexpectedEOF
      }
      new AttrParser(name, value, s)
    }

    override def interpolate(item: Value): Iteratee[Int, Attr] = {
      var s = this.s
      if (s == 4 || s == 5) {
        val value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        s = if (value.isDone) 6 else 5
        new AttrParser(name, value, s)
      }
      else super.interpolate(item)
    }

    override def state: Maybe[Attr] =
      if (name.isDone) Bind(Attr(name.bind, value.state.bindOrElse(Extant))) else Trap

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"AttrParser").state
  }


  private final class SlotParser(
      name: Iteratee[Int, String],
      value: Iteratee[Int, Value],
      s: Int)
    extends Parser[Slot] {

    def this() = this(NameParser, BlockValueParser, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Slot] = {
      var c = 0
      var s = this.s
      var value = this.value
      var name = this.name
      if (s == 1) {
        name = name.feed(input)
        if (name.isDone) s = 2
        else if (name.isError) return name.asError
      }
      if (s == 2) {
        while (!input.isEmpty && { c = input.head; isSpace(c) }) input.step()
        if (!input.isEmpty) {
          if (c == ':') {
            input.step()
            s = 3
          }
          else return error(input, expected = ':', found = c)
        }
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 3) {
        while (!input.isEmpty && isSpace(input.head)) input.step()
        if (!input.isEmpty) s = 4
        else if (input.isDone) return Iteratee.done(Slot(name.bind))
      }
      if (s == 4) {
        while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
        if (value.isDone) return Iteratee.done(Slot(name.bind, value.bind))
        else if (value.isError) return value.asError
      }
      new SlotParser(name, value, s)
    }

    override def interpolate(item: Value): Iteratee[Int, Slot] = {
      if (s == 3) Iteratee.done(Slot(name.bind, item))
      else if (s == 4) {
        val value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isDone) Iteratee.done(Slot(name.bind, value.bind))
        else if (value.isError) value.asError
        else new SlotParser(name, value, s)
      }
      else super.interpolate(item)
    }

    override def state: Maybe[Slot] =
      if (name.isDone) Bind(Slot(name.bind, value.state.bindOrElse(Extant))) else Trap

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"SlotParser").state
  }


  private final class BlockValueParser(
      builder: ItemBuilder with State[Value],
      field: Iteratee[Int, Field],
      value: Iteratee[Int, Value],
      s: Int)
    extends Parser[Value] {

    def this() = this(null, null, null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Value] = {
      var c = 0
      var s = this.s
      var value = this.value
      var field = this.field
      var builder = this.builder
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          if (!input.isEmpty) {
            c = input.head
            if (c == '@') {
              field = AttrParser
              s = 2
            }
            else if (c == '{') {
              if (builder ne null) {
                value = new RecordParser(builder)
                s = 5
              }
              else {
                value = RecordParser
                s = 4
              }
            }
            else if (c == '[') {
              if (builder ne null) {
                value = new MarkupParser(builder)
                s = 5
              }
              else {
                value = MarkupParser
                s = 4
              }
            }
            else if (c == '"') {
              value = StringParser
              s = 4
            }
            else if (c == '%') {
              value = DataParser
              s = 4
            }
            else if (c == '-' || c >= '0' && c <= '9') {
              value = NumberParser
              s = 4
            }
            else if (c == '#') {
              value = TokenParser
              s = 4
            }
            else if (builder eq null) return Iteratee.done(Absent)
            else return Iteratee.done(builder.state)
          }
          else if (input.isDone) {
            if (builder eq null) return Iteratee.done(Absent)
            else return Iteratee.done(builder.state)
          }
        }
        if (s == 2) {
          while ((!input.isEmpty || input.isDone) && field.isCont) field = field.feed(input)
          if (field.isDone) {
            if (builder eq null) builder = ValueBuilder
            builder.appendField(field.bind)
            field = null
            s = 3
          }
          else if (field.isError) return field.asError
        }
        if (s == 3) {
          while (!input.isEmpty && isSpace(input.head)) input.step()
          if (!input.isEmpty) s = 1
          else if (input.isDone) return Iteratee.done(builder.state)
        }
        if (s == 4) {
          while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
          if (value.isDone) {
            if (builder eq null) builder = ValueBuilder
            builder.appendValue(value.bind)
            return Iteratee.done(builder.state)
          }
          else if (value.isError) return value.asError
        }
        if (s == 5) {
          while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
          if (value.isDone) return Iteratee.done(builder.state)
          else if (value.isError) return value.asError
        }
      }
      new BlockValueParser(builder, field, value, s)
    }

    override def interpolate(item: Value): Iteratee[Int, Value] = {
      var s = this.s
      if (s == 1 || s == 3) {
        val builder = if (this.builder ne null) this.builder else ValueBuilder
        builder.appendValue(item)
        Iteratee.done(builder.state)
      }
      else if (s == 2) {
        var field = this.field.asInstanceOf[Parser[Field]].interpolate(item)
        if (field.isError) return field.asError
        if (field.isDone) {
          val builder = if (this.builder ne null) this.builder else ValueBuilder
          builder.appendField(field.bind)
          field = null
          s = 3
        }
        new BlockValueParser(builder, field, value, s)
      }
      else if (s == 4) {
        val value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        if (value.isDone) {
          val builder = if (this.builder ne null) this.builder else ValueBuilder
          builder.appendValue(value.bind)
          Iteratee.done(builder.state)
        }
        else if (value.isError) value.asError
        else new BlockValueParser(builder, field, value, s)
      }
      else if (s == 5) {
        val value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        if (value.isDone) Iteratee.done(builder.state)
        else new BlockValueParser(builder, field, value, s)
      }
      else super.interpolate(item)
    }

    override def state: Maybe[Value] = if (builder ne null) Bind(builder.state) else Trap

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"BlockValueParser").state
  }


  private final class InlineValueParser(
      builder: ItemBuilder with State[Value],
      field: Iteratee[Int, Field],
      value: Iteratee[Int, Value],
      s: Int)
    extends Parser[Value] {

    def this() = this(null, null, null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Value] = {
      var c = 0
      var s = this.s
      var value = this.value
      var field = this.field
      var builder = this.builder
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          if (!input.isEmpty) {
            c = input.head
            if (c == '@') {
              field = AttrParser
              s = 2
            }
            else if (c == '{') {
              if (builder ne null) {
                value = new RecordParser(builder)
                s = 5
              }
              else {
                value = RecordParser
                s = 4
              }
            }
            else if (c == '[') {
              if (builder ne null) {
                value = new MarkupParser(builder)
                s = 5
              }
              else {
                value = MarkupParser
                s = 4
              }
            }
            else if (builder eq null) return Iteratee.done(Absent)
            else return Iteratee.done(builder.state)
          }
          else if (input.isDone) {
            if (builder eq null) return Iteratee.done(Absent)
            else return Iteratee.done(builder.state)
          }
        }
        if (s == 2) {
          while ((!input.isEmpty || input.isDone) && field.isCont) field = field.feed(input)
          if (field.isDone) {
            if (builder eq null) builder = ValueBuilder
            builder.appendField(field.bind)
            field = null
            s = 3
          }
          else if (field.isError) return field.asError
        }
        if (s == 3) {
          while (!input.isEmpty && isSpace(input.head)) input.step()
          if (!input.isEmpty) s = 1
          else if (input.isDone) return Iteratee.done(builder.state)
        }
        if (s == 4) {
          while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
          if (value.isDone) {
            if (builder eq null) builder = ValueBuilder
            builder.appendValue(value.bind)
            return Iteratee.done(builder.state)
          }
          else if (value.isError) return value.asError
        }
        if (s == 5) {
          while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
          if (value.isDone) return Iteratee.done(builder.state)
          else if (value.isError) return value.asError
        }
      }
      new InlineValueParser(builder, field, value, s)
    }

    override def interpolate(item: Value): Iteratee[Int, Value] = {
      var s = this.s
      if (s == 1 || s == 3) {
        val builder = if (this.builder ne null) this.builder else ValueBuilder
        builder.appendValue(item)
        Iteratee.done(builder.state)
      }
      else if (s == 2) {
        var field = this.field.asInstanceOf[Parser[Field]].interpolate(item)
        if (field.isError) return field.asError
        if (field.isDone) {
          val builder = if (this.builder ne null) this.builder else ValueBuilder
          builder.appendField(field.bind)
          field = null
          s = 3
        }
        new InlineValueParser(builder, field, value, s)
      }
      else if (s == 4) {
        val value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        if (value.isDone) {
          val builder = if (this.builder ne null) this.builder else ValueBuilder
          builder.appendValue(value.bind)
          Iteratee.done(builder.state)
        }
        else if (value.isError) value.asError
        else new InlineValueParser(builder, field, value, s)
      }
      else if (s == 5) {
        val value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        if (value.isDone) Iteratee.done(builder.state)
        else new InlineValueParser(builder, field, value, s)
      }
      else super.interpolate(item)
    }

    override def state: Maybe[Value] = if (builder ne null) Bind(builder.state) else Trap

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"InlineValueParser").state
  }


  private final class RecordParser(
      builder: ItemBuilder with State[Value],
      field: Iteratee[Int, Field],
      value: Iteratee[Int, Value],
      s: Int)
    extends Parser[Value] {

    def this(builder: ItemBuilder with State[Value]) = this(builder, null, null, 1)

    def this() = this(null, null, null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Value] = {
      var c = 0
      var s = this.s
      var value = this.value
      var field = this.field
      val builder = if (this.builder ne null) this.builder else RecordBuilder
      while (!input.isEmpty || input.isDone) {
        if (s == 1) {
          if (!input.isEmpty) {
            c = input.head
            if (c == '{') {
              input.step()
              s = 2
            }
            else return error(input, expected = '{', found = c)
          }
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 2) {
          while (!input.isEmpty && { c = input.head; isWhitespace(c) }) input.step()
          if (!input.isEmpty) {
            if (c == '}') {
              input.step()
              return Iteratee.done(builder.state)
            }
            else if (isNameStartChar(c)) {
              field = SlotParser
              s = 3
            }
            else {
              value = BlockValueParser
              s = 4
            }
          }
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 3) {
          while ((!input.isEmpty || input.isDone) && field.isCont) field = field.feed(input)
          if (field.isDone) {
            builder.appendField(field.bind)
            field = null
            s = 5
          }
          else if (field.isError) return field.asError
        }
        if (s == 4) {
          while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
          if (value.isDone) {
            builder.appendValue(value.bind)
            value = null
            s = 5
          }
          else if (value.isError) return value.asError
        }
        if (s == 5) {
          while (!input.isEmpty && { c = input.head; isSpace(c) }) input.step()
          if (!input.isEmpty) {
            if (c == ',' || c == ';' || isNewline(c)) {
              input.step()
              s = 2
            }
            else if (c == '}') {
              input.step()
              return Iteratee.done(builder.state)
            }
            else return error(input, expected = "'}', ';', ',', or newline", found = c)
          }
          else if (input.isDone) return unexpectedEOF
        }
      }
      new RecordParser(builder, field, value, s)
    }

    override def interpolate(item: Value): Iteratee[Int, Value] = {
      var s = this.s
      if (s == 2) {
        builder.appendValue(item)
        s = 5
        new RecordParser(builder, field, value, s)
      }
      else if (s == 3) {
        var field = this.field.asInstanceOf[Parser[Field]].interpolate(item)
        if (field.isError) return field.asError
        if (field.isDone) {
          builder.appendField(field.bind)
          field = null
          s = 5
        }
        new RecordParser(builder, field, value, s)
      }
      else if (s == 4) {
        var value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        if (value.isDone) {
          builder.appendValue(value.bind)
          value = null
          s = 5
        }
        new RecordParser(builder, field, value, s)
      }
      else super.interpolate(item)
    }

    override def state: Maybe[Value] = if (builder ne null) Bind(builder.state) else Trap

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"RecordParser").state
  }


  private final class MarkupParser(
      builder: ItemBuilder with State[Value],
      text: StringBuilder with State[Text],
      value: Iteratee[Int, Value],
      s: Int)
    extends Parser[Value] {

    def this(builder: ItemBuilder with State[Value]) = this(builder, null, null, 1)

    def this() = this(null, null, null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Value] = {
      var c = 0
      var s = this.s
      var value = this.value
      var text = this.text
      var builder = this.builder
      if (s == 1) {
        if (!input.isEmpty) {
          c = input.head
          if (c == '[') {
            input.step()
            s = 2
          }
          else return error(input, expected = '[', found = c)
        }
        else if (input.isDone) return unexpectedEOF
      }
      while (!input.isEmpty || input.isDone) {
        if (s == 2) {
          while (!input.isEmpty && {
              c = input.head;
              c != '@' && c != '{' && c != '}' && c != '[' && c != ']' && c != '\\'
            }) {
            input.step()
            if (text eq null) text = TextBuilder
            text.append(c)
          }
          if (!input.isEmpty) {
            if (c == ']') {
              input.step()
              if (builder eq null) {
                if (text eq null) text = TextBuilder
                return Iteratee.done(text.state)
              }
              else {
                if (text ne null) builder.appendValue(text.state)
                return Iteratee.done(builder.state)
              }
            }
            else if (c == '@') {
              if (builder eq null) builder = RecordBuilder
              if (text ne null) {
                builder.appendValue(text.state)
                text = null
              }
              value = InlineValueParser
              s = 3
            }
            else if (c == '{') {
              if (builder eq null) builder = RecordBuilder
              if (text ne null) {
                builder.appendValue(text.state)
                text = null
              }
              value = new RecordParser(builder)
              s = 4
            }
            else if (c == '[') {
              if (builder eq null) builder = RecordBuilder
              if (text ne null) {
                builder.appendValue(text.state)
                text = null
              }
              value = new MarkupParser(builder)
              s = 4
            }
            else if (c == '\\') {
              input.step()
              s = 5
            }
            else return error(input, expected = "", found = c)
          }
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 3) {
          while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
          if (value.isDone) {
            builder.appendValue(value.bind)
            value = null
            s = 2
          }
          else if (value.isError) return value.asError
        }
        if (s == 4) {
          while ((!input.isEmpty || input.isDone) && value.isCont) value = value.feed(input)
          if (value.isDone) {
            value = null
            s = 2
          }
          else if (value.isError) return value.asError
        }
        if (s == 5) {
          if (!input.isEmpty) {
            c = input.head
            if (text eq null) text = TextBuilder
            if (c == '"' || c == '\\' || c == '/' || c == '@' || c == '{' || c == '}' || c == '[' || c == ']') {
              input.step()
              text.append(c)
              s = 2
            }
            else if (c == 'b') {
              input.step()
              text.append('\b')
              s = 2
            }
            else if (c == 'f') {
              input.step()
              text.append('\f')
              s = 2
            }
            else if (c == 'n') {
              input.step()
              text.append('\n')
              s = 2
            }
            else if (c == 'r') {
              input.step()
              text.append('\r')
              s = 2
            }
            else if (c == 't') {
              input.step()
              text.append('\t')
              s = 2
            }
            else return error(input, expected = "escape character", found = c)
          }
          else if (input.isDone) return unexpectedEOF
        }
      }
      new MarkupParser(builder, text, value, s)
    }

    override def interpolate(item: Value): Iteratee[Int, Value] = {
      var s = this.s
      if (s == 2) {
        var text = this.text
        val builder = if (this.builder ne null) this.builder else RecordBuilder
        if (text ne null) {
          builder.appendValue(text.state)
          text = null
        }
        builder.appendValue(item)
        new MarkupParser(builder, text, value, s)
      }
      else if (s == 3) {
        var value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        if (value.isDone) {
          builder.appendValue(value.bind)
          value = null
          s = 2
        }
        new MarkupParser(builder, text, value, s)
      }
      else if (s == 4) {
        var value = this.value.asInstanceOf[Parser[Value]].interpolate(item)
        if (value.isError) return value.asError
        if (value.isDone) {
          value = null
          s = 2
        }
        new MarkupParser(builder, text, value, s)
      }
      else super.interpolate(item)
    }

    override def state: Maybe[Value] =
      if (builder ne null) Bind(builder.state)
      else if (text ne null) Bind(text.state)
      else Trap

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"MarkupParser").state
  }


  private final class StringParser(
      text: StringBuilder with State[Text],
      s: Int)
    extends Parser[Text] {

    def this() = this(null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Text] = {
      var c = 0
      var s = this.s
      var text = this.text
      if (s == 1) {
        if (!input.isEmpty && { c = input.head; c == '"' }) {
          input.step()
          s = 2
        }
        else if (!input.isEmpty) return error(input, expected = '"', found = c)
        else if (input.isDone) return unexpectedEOF
      }
      while (!input.isEmpty || input.isDone) {
        if (s == 2) {
          if (text eq null) text = TextBuilder
          while (!input.isEmpty && { c = input.head; c != '"' && c != '\\' }) {
            input.step()
            text.append(c)
          }
          if (!input.isEmpty) {
            if (c == '"') {
              input.step()
              return Iteratee.done(text.state)
            }
            else if (c == '\\') {
              input.step()
              s = 3
            }
          }
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 3) {
          if (!input.isEmpty) {
            c = input.head
            if (c == '"' || c == '\\' || c == '/' || c == '@' || c == '{' || c == '}' || c == '[' || c == ']') {
              input.step()
              text.append(c)
              s = 2
            }
            else if (c == 'b') {
              input.step()
              text.append('\b')
              s = 2
            }
            else if (c == 'f') {
              input.step()
              text.append('\f')
              s = 2
            }
            else if (c == 'n') {
              input.step()
              text.append('\n')
              s = 2
            }
            else if (c == 'r') {
              input.step()
              text.append('\r')
              s = 2
            }
            else if (c == 't') {
              input.step()
              text.append('\t')
              s = 2
            }
            else return error(input, expected = "escape character", found = c)
          }
          else if (input.isDone) return unexpectedEOF
        }
      }
      new StringParser(text, s)
    }

    override def state: Maybe[Text] = if (text ne null) Bind(text.state) else Trap

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"StringParser").state
  }


  private final class DataParser(
      data: StringBuilder with State[Data],
      s: Int)
    extends Parser[Data] {

    def this() = this(null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Data] = {
      var c = 0
      var s = this.s
      val data = if (this.data ne null) this.data else DataBuilder
      if (s == 1) {
        if (!input.isEmpty && { c = input.head; c == '%' }) {
          input.step()
          s = 2
        }
        else if (!input.isEmpty) return error(input, expected = '%', found = c)
        else if (input.isDone) return unexpectedEOF
      }
      while (!input.isEmpty || input.isDone) {
        if (s == 2) {
          if (!input.isEmpty && { c = input.head; isBase64Char(c) }) {
            input.step()
            data.append(c)
            s = 3
          }
          else if (!input.isEmpty || input.isDone) return Iteratee.done(data.state)
        }
        if (s == 3) {
          if (!input.isEmpty && { c = input.head; isBase64Char(c) }) {
            input.step()
            data.append(c)
            s = 4
          }
          else if (!input.isEmpty) return error(input, expected = "base64 digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 4) {
          if (!input.isEmpty && { c = input.head; isBase64Char(c) || c == '=' }) {
            input.step()
            data.append(c)
            if (c != '=') s = 5
            else s = 6
          }
          else if (!input.isEmpty) return error(input, expected = "base64 digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        if (s == 5) {
          if (!input.isEmpty && { c = input.head; isBase64Char(c) || c == '=' }) {
            input.step()
            data.append(c)
            if (c != '=') s = 2
            else return Iteratee.done(data.state)
          }
          else if (!input.isEmpty) return error(input, expected = "base64 digit", found = c)
          else if (input.isDone) return unexpectedEOF
        }
        else if (s == 6) {
          if (!input.isEmpty && { c = input.head; c == '=' }) {
            input.step()
            data.append(c)
            return Iteratee.done(data.state)
          }
          else if (!input.isEmpty) return error(input, expected = '=', found = c)
          else if (input.isDone) return unexpectedEOF
        }
      }
      new DataParser(data, s)
    }

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"DataParser").state
  }


  private final class NumberParser(
      builder: Builder[Int] with State[String],
      s: Int)
    extends Parser[Number] {

    def this() = this(null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Number] = {
      var c = 0
      var s = this.s
      val builder = if (this.builder ne null) this.builder else String.Builder
      if (s == 1) {
        if (!input.isEmpty) {
          c = input.head
          if (c == '-') {
            input.step()
            builder.append(c)
          }
          s = 2
        }
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 2) {
        if (!input.isEmpty) {
          c = input.head
          if (c == '0') {
            input.step()
            builder.append(c)
            s = 4
          }
          else if (c >= '1' && c <= '9') {
            input.step()
            builder.append(c)
            s = 3
          }
          else return error(input, expected = "digit", found = c)
        }
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 3) {
        while (!input.isEmpty && { c = input.head; c >= '0' && c <= '9' }) {
          input.step()
          builder.append(c)
        }
        if (!input.isEmpty) s = 4
        else if (input.isDone) return Iteratee.done(Number(builder.state))
      }
      if (s == 4) {
        if (!input.isEmpty) {
          c = input.head
          if (c == '.') {
            input.step()
            builder.append(c)
            s = 5
          }
          else if (c == 'E' || c == 'e') {
            input.step()
            builder.append(c)
            s = 8
          }
          else return Iteratee.done(Number(builder.state))
        }
        else if (input.isDone) return Iteratee.done(Number(builder.state))
      }
      if (s == 5) {
        if (!input.isEmpty) {
          c = input.head
          if (c >= '0' && c <= '9') {
            input.step()
            builder.append(c)
            s = 6
          }
          else return error(input, expected = "digit", found = c)
        }
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 6) {
        while (!input.isEmpty && { c = input.head; c >= '0' && c <= '9' }) {
          input.step()
          builder.append(c)
        }
        if (!input.isEmpty) s = 7
        else if (input.isDone) return Iteratee.done(Number(builder.state))
      }
      if (s == 7) {
        c = input.head
        if (c == 'E' || c == 'e') {
          input.step()
          builder.append(c)
          s = 8
        }
        else return Iteratee.done(Number(builder.state))
      }
      if (s == 8) {
        if (!input.isEmpty) {
          c = input.head
          if (c == '+' || c == '-') {
            input.step()
            builder.append(c)
          }
          s = 9
        }
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 9) {
        if (!input.isEmpty) {
          c = input.head
          if (c >= '0' && c <= '9') {
            input.step()
            builder.append(c)
            s = 10
          }
          else return error(input, expected = "digit", found = c)
        }
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 10) {
        while (!input.isEmpty && { c = input.head; c >= '0' && c <= '9' }) {
          input.step()
          builder.append(c)
        }
        if (!input.isEmpty || input.isDone) return Iteratee.done(Number(builder.state))
      }
      new NumberParser(builder, s)
    }

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"NumberParser").state
  }


  private final class TokenParser(
      builder: StringBuilder with State[String],
      s: Int)
    extends Parser[Value] {

    def this() = this(null, 1)

    override def feed(input: Iterator[Int]): Iteratee[Int, Value] = {
      var c = 0
      var s = this.s
      var builder = this.builder
      if (s == 1) {
        if (!input.isEmpty && { c = input.head; c == '#' }) {
          input.step()
          s = 2
        }
        else if (!input.isEmpty) return error(input, expected = '#', found = c)
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 2) {
        if (!input.isEmpty && { c = input.head; isNameStartChar(c) }) {
          if (builder eq null) builder = String.Builder
          input.step()
          builder.append(c)
          s = 3
        }
        else if (!input.isEmpty) {
          val message = (String.Builder~"expected #true or #false, but found #"~c).state
          return Iteratee.error(new ReconException(message, input))
        }
        else if (input.isDone) return unexpectedEOF
      }
      if (s == 3) {
        while (!input.isEmpty && { c = input.head; isNameChar(c) }) {
          input.step()
          builder.append(c)
        }
        if (!input.isEmpty || input.isDone) {
          val token = builder.state
          if (token == "true") return Iteratee.done(True)
          else if (token == "false") return Iteratee.done(False)
          else {
            val message = (String.Builder~"expected #true or #false, but found #"~token).state
            return Iteratee.error(new ReconException(message, input))
          }
        }
      }
      new TokenParser(builder, s)
    }

    override def toString: String = (String.Builder~ReconParser.toString~'.'~"TokenParser").state
  }


  private def unexpectedEOF: Iteratee[Any, Nothing] =
    Iteratee.error(new ReconException("unexpected end of input", Iterator.done))

  private def error(input: Iterator[Int], expected: Int, found: Int): Iteratee[Any, Nothing] = {
    val message =
      if (expected == 0 && found == 0) "unexpected input"
      else if (expected == 0) (String.Builder~"unexpected "~'\''~found~'\'').state
      else if (found == 0) (String.Builder~"expected "~'\''~expected~'\'').state
      else (String.Builder~"expected "~'\''~expected~'\''~", but found "~'\''~found~'\'').state
    Iteratee.error(new ReconException(message, input))
  }

  private def error(input: Iterator[Int], expected: String, found: Int): Iteratee[Any, Nothing] = {
    val message =
      if (expected.length == 0 && found == 0) "unexpected input"
      else if (expected.length == 0) (String.Builder~"unexpected "~'\''~found~'\'').state
      else if (found == 0) (String.Builder~"expected "~expected).state
      else (String.Builder~"expected "~expected~", but found "~'\''~found~'\'').state
    Iteratee.error(new ReconException(message, input))
  }
}

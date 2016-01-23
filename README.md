# Record Notation (RECON)

[![Build Status](https://travis-ci.org/web-aware/recon-scala.svg?branch=master)](https://travis-ci.org/web-aware/recon-scala)

RECON brings attributes into the era of object notation, and provides a simple
grammar and uniform tree model for attributed text markup.  RECON aims to
combine the minimalism of JSON with the expressiveness of XML in a
human-friendly syntax.

## Language Quick Start

### Primtives

RECON has three primitive datatypes: _text_, _number_, and _data_.

#### Text

Text values take one of two forms: a quoted _string_, or an unquoted
_identifier_.

```recon
"string"
identifier
```

#### Numbers

Numbers serialize as decimal literals.

```recon
-1
3.14
6.02e23
```

#### Data

Binary data serializes as a leading '%' symbol, followed by a base64 literal.

```recon
%AA==
```

### Records

RECON's sole aggregate datatype, the _record_, plays the combined role of array
and associative array.  Think of a record as a partially keyed list.  The
example record below contains two ordered items, first a "subject" field with
value "Greetings", then the unkeyed string "Hello, Earthlings!".

```recon
{ subject: "Greetings", "Hello, Earthlings!" }
```

A single comma, a single semicolon, or one or more newlines separate items.
Newline separated records provide a clean syntax for pretty-printed documents.

```recon
{
  subject: "Re: Greetings"
  "Hi Martians!"
}
```

Records support arbitrary values as slot keys.

```recon
{
  @planet Jupiter: {}
  @god Jupiter: {}
}
```

### Blocks

Top-level documents can omit the curly braces around their root record.  We
call the content of a record, sans curly braces, a _block_.  When a block
contains only a single item, the value of the block reduces to just the value
of the item it contains.  The example block below is equivalent to the sample
record above.

```recon
subject: "Re: Greetings"
"Hi Martians!"
```

### Attributes

The @ sign introduces an attribute.  Attributes call out key fields of a
record.  The previous markup example further reduces to the form below.

```recon
{
  "Hello, "
  {
    "@em":
    "world"
  }
  "!"
}
```

Note that the `@em` field above has no explicit value.  The RECON data model
refers to unspecified–but existent–values as _extant_.  We say that the record
`@em[world]` has an extant attribute named `em`.

Of course, attributes can have associated values too.  Place attribute
parameters in parentheses, following the attribute's name.

```recon
@answer(42)
@event("onClick")
```

The above attributes are structurally equivalent to:

```recon
{"@answer":42}
{"@event":"onClick"}
```

Attribute parentheses enclose a block, meaning attribute values construct an
implicit record when needed. An example, with its desugared equivalent, follows.

```recon
@img(src: "tesseract.png", width: 10, height: 10, depth: 10, time: -1)

{
  "@img": {
    src: "tesseract.png"
    width: 10
    height: 10
    depth: 10
    time: -1
  }
}
```

Attributes _modify_ adjacent values.  Modified values interpolate into the
record formed by their adjacent attributes.  Here are some examples of values
with prefix, postfix, and circumfix attributes:

```recon
@duration 30
30 @seconds
@duration 30 @seconds
@relative @duration 30 @seconds
```

The above attribute expressions desugar to the following records:

```recon
{ "@duration":, 30 }
{ 30, "@seconds": }
{ "@duration":, 30, "@seconds": }
{ "@relative":, "@duration":, 30, "@seconds": }
```

Modified records flatten into the record formed by their adjacent attributes.
So `@point{x:0,y:0}`, reduces to `{"@point":,x:0,y:0}`, not
`{"@point":,{x:0,y:0}}`.

### Markup

Square brackets denote _markup_.  Markup offers an inverted syntax for records,
with values embedded in text, as opposed to text embedded in records.

```recon
[Hello, @em[world]!]
```

Markup is really just syntactic sugar for records.  The above example expresses
the exact same structure as the one below.

```recon
{ "Hello, "; @em "world"; "!" }
```

Curly braces within markup lift the enclosed block into the markup's record.
The following records are equivalent.

```recon
[Answer: {42}.]
{ "Answer", 42, "." }
```

Square brackets lift nested markup into the enclosing record.  Make sure to
backslash escape square brackets if you want to include them verbatim.

```recon
[Say [what]?]
{ "Say ", "what", "?"}

[Say \[what\]?]
{ "Say [what]?" }
```

Sequential attributes within markup don't chain; each markup-embedded
attribute inserts a nested record.

```recon
[http@colon@slash@slash]
{ "http", @colon, @slash, @slash }
```

Attributes in markup can prefix curly brace enclosed blocks, and nested markup.

```recon
[Goals: @select(max:2){fast,good,cheap}.]
{ "Goals: ", @select(max:2){fast,good,cheap}, "." }
```

Beware that whitespace inside markup is significant.  Notice how the single
space added to the example below completely changes its meaning, when compared
to the previous example.

```recon
[Goals: @select(max:2) {fast,good,cheap}.]
{ "Goals: ", @select(max:2), " ", {fast,good,cheap}, "." }
```

## Scala Library

To get started with the RECON Scala library, add the `recon-scala` dependency
to your SBT build.

```
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.webaware" %% "recon-scala" % "0.2.0-SNAPSHOT"
```

### Tutorial

The Scala library support compile-time RECON string interpolation.

```scala
scala> import recon._
import recon._

scala> val label = "Example"
label: String = Example

scala> recon"""@a(href:"example.com")[$label]"""
res0: recon.Value = Record(Attr("a", Record(Slot("href", Text("example.com")))), Text("Example"))

scala> recon"""{1, 2 3, 4}"""
<console>:11: error: expected '}', ';', ',', or newline, but found '3'
       recon"""{1, 2 3, 4}"""
                     ^
```

Of course, you can parse and serialize RECON at runtime too.

```scala
scala> import recon._
import recon._

scala> val event = Value.parseRecon("@event(onClick)")
event: recon.Value = Record(Attr("event", Text("onClick")))

scala> event.toRecon
res0: String = @event(onClick)
```

Use the `/` operator to select keyed fields:

```scala
scala> import recon._
import recon._

scala> val msg = recon"{from: me, to: you}"
msg: recon.Value = Record(Slot("from", Text("me")), Slot("to", Text("you")))

scala> msg / "from"
res0: recon.Value = Text("me")

scala> msg / "to"
res1: recon.Value = Text("you")

scala> msg / "body"
res2: recon.Value = Absent
```

Higher order collection operations and transformations on records just work.
The lightweight basis collections library provides collection utilities as
unintrusive implicit extension methods, and implements them using fast,
inlined macros.

```scala
scala> import recon._
import recon._

scala> val list = recon"@ol{@li[a],@li[b],@li[c]}"
list: recon.Value = Record(Attr("ol"), Record(Attr("li"), Text("a")), Record(Attr("li"), Text("b")), Record(Attr("li"), Text("c")))

scala> list.asRecord.map {
     |   case Attr("ol", _) => Attr("ul")
     |   case item => item
     | }
res0: recon.Record = Record(Attr("ul"), Record(Attr("li"), Text("a")), Record(Attr("li"), Text("b")), Record(Attr("li"), Text("c")))
```

The `Form` typeclass projects RECON values to Scala values, and vice versa.
The pattern of projecting weakly typed structural values to strongly typed
objects allows the RECON language to serve as an expressive and malleable
syntax for domain specific data formats.

`Item.cast[T]` converts a RECON item to a Scala value of type `Maybe[T]` using
an implicitly available `Form[T]`.  `Item.coerce[T]` converts an item to a
plain `T` value, returning `Form[T].unit` if the conversion fails.

Use the `Value` factory method to marshal Scala values to RECON values.

```scala
scala> import recon._; import basis.collections._
import recon._
import basis.collections._

scala> recon"""{ 1, @prime 2, "3" }""".coerce[Array[Int]]
res0: Array[Int] = Array(1, 2, 3)

scala> recon"""@alpha { a: 1, b: 2 }""".coerce[Map[String, Int]]
res1: basis.collections.Map[String,Int] = HashTrieMap("a" -> 1, "b" -> 2)

scala> recon"beta".cast[Int]
res2: basis.Maybe[Int] = Trap
```

Use the `Value` factory to marshal Scala values to RECON values by way of
implicit `Form` typeclass instances.

```scala
scala> import recon._; import basis.collections._
import recon._
import basis.collections._

scala> Value(Seq(0.5, 0.25))
res0: recon.Value = Record(Number(0.5), Number(0.25))
```

### Data Model

To a first approximation, the RECON data model has the following structure:

```scala
sealed abstract class Item                              // |-- Item
sealed abstract class Field extends Item                // |   |-- Field
case class Attr(key: Text, value: Value) extends Field  // |   |   |-- Attr
case class Slot(key: Value, value: Value) extends Field // |   |   |-- Slot
sealed abstract class Value extends Item                // |   |-- Value
case class Record(items: Item*) extends Value           // |   |   |-- Record
case class Text(toString: String) extends Value         // |   |   |-- Text
case class Data(toArray: Array[Byte]) extends Value     // |   |   |-- Data
case class Number(toDouble: Double) extends Value       // |   |   |-- Number
case object Extant extends Value                        // |   |   |-- Extant
case object Absent extends Value                        // |   |   |-- Absent
```

### Ordering

RECON defines a total ordering over all items.  Items of different types sort
in the following relative order: attributes, slots, records, data, text,
numbers, extant, then absent.

## Language Grammar

```
SP ::= #x20 | #x9

NL ::= #xA | #xD

WS ::= SP | NL

Char ::= [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]

NameStartChar ::=
  [A-Z] | "_" | [a-z] |
  [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] |
  [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] |
  [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] |
  [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]

NameChar ::=  NameStartChar | '-' | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]

MarkupChar ::= Char - ('\\' | '@' | '{' | '}' | '[' | ']')

StringChar ::= Char - ('"' | '\\' | '@' | '{' | '}' | '[' | ']' | '\b' | '\f' | '\n' | '\r' | '\t')

CharEscape ::= '\\' ('"' | '\\' | '/' | '@' | '{' | '}' | '[' | ']' | 'b' | 'f' | 'n' | 'r' | 't')

Base64Char ::= [A-Za-z0-9+/]

Block ::= WS* Slots WS*

Slots ::= Slot SP* ((',' | ';' | NL) WS* Slots)?

Slot ::= BlockValue (SP* ':' SP* BlockValue?)?

Attr ::= '@' Ident ('(' Block ')')?

BlockValue ::=
  Attr SP* BlockValue? |
  (Record | Markup | Ident | String | Number | Data) SP* (Attr SP* BlockValue?)?

InlineValue ::= Attr (Record | Markup)? | Record | Markup

Record ::= '{' Block '}'

Markup ::= '[' (MarkupChar* | CharEscape | InlineValue)* ']'

Ident ::= NameStartChar NameChar*

String ::= '"' (StringChar* | CharEscape)* '"'

Number ::= '-'? (([1-9] [0-9]*) | [0-9]) ('.' [0-9]+)? (('E' | 'e') ('+' | '-')? [0-9]+)?

Data ::= '%' (Base64Char{4})* (Base64Char Base64Char ((Base64Char '=') | ('=' '=')))?
```

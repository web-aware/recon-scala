# Record Notation (RECON)

[![Build Status](https://travis-ci.org/coeffect/recon-scala.svg?branch=master)](https://travis-ci.org/coeffect/recon-scala)

RECON brings attributes into the era of object notation, and provides a simple
grammar and uniform tree model for attributed text markup.  RECON aims to
combine the minimalism of JSON with the expressiveness of XML in a
human-friendly syntax.

## Language Quick Start

### Primtives

RECON has three primitive datatypes: _text_, _number_, and _data_.

#### Text

Text values can take one of three forms: _string_, _markup_, or _identifier_.

```recon
"string"
[markup]
identifier
```

#### Numbers

Numbers enocde as decimal literals.

```recon
1
1.41
```

#### Data

RECON encodes binary data as base64 literals following an initial `%`.

```recon
%AA==
```

### Records

RECON's sole aggregate datatype, the _record_, plays the combined role of array
and associative array.  Think of a record as a partially keyed list.  The
example record below has two items: a "subject" field with value "Greetings",
followed by an unkeyed string.

```recon
{ subject: "Greetings", "Hello, Earthlings!" }
```

Newlines can separate items too, giving pretty-printed documents a cleaner look.

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

Top-level documents can omit the curly braces around a root record.  We call
the content of a record sans curly braces a _block_.  When a block contains
only a single item, the value of the block reduces to just the value of the
element it contains.  The example block below is equivalent to the example
record above.

```recon
subject: "Re: Greetings"
"Hi Martians!"
```

### Markup

Square brackets denote _markup_.  Markup is an inverted syntax for records,
with values embedded in text, instead of text embedded in records.

```recon
[Hello, @em[world]!]
```

Markup really is just an alternative encoding for records.  The above example
is exactly equivalent to the below example.

```recon
{ "Hello, "; @em "world"; "!" }
```

### Attributes

The @ sign introduces an attribute.  Attributes are syntactic sugar for
pulling key fields out in front of a record.  The previous markup example
further reduces to the form below.

```recon
{
  "Hello, ",
  {
    em:
    "world"
  },
  "!"
}
```

Note that the `em:` field has no value.  RECON is an algebraic datatype, an
unspecified value is said to be _extant_.  `Extant` is one of the unit type
constructors of the RECON algebraic datatype, the other being `Absent`.
Neither `Extant` nor `Absent` have explicit syntax, but they play important
roles in the RECON data model.  Think of `Extant` and `Absent` as more
meaningful versions of `null`.

Of course, attributes can have values too.  Place parameters in parentheses
after an attribute name.

```recon
@answer(42)
@event("onClick")
```

The above attributes desugar to:

```recon
{answer:42}
{event:"onClick"}
```

Attribute parentheses enclose a block, allowing them to contain keyed slots.
An example, with its desugared equivalent, follows.

```recon
@img(src: "tesseract.png", width: 10, height: 10, depth: 10, time: -1)

{
  img: {
    src: "tesseract.png"
    width: 10
    height: 10
    depth: 10
    time: -1
  }
}
```

When an attribute precedes a value, the value gets appended to the record
holding the attribute fields.  The first unkeyed item in a record is known as
the record's _target_.

```recon
@a(href:"example.com")[Some examples...]

{
  a: { href: "example.com" }
  "Some examples..."
}
```

Attributed records are concatenated to their preceding attributes.

```recon
@agent("007") @license("to-kill") {
  public-name: "Bond"
  private-name: @secret "James Bond"
}

{
  agent: "007"
  license: "to-kill"
  public-name: "Bond"
  private-name: {
    secret:
    "James Bond"
  }
}
```

## Data Model

To a first approximation, the RECON data model has the following structure:

```scala
sealed abstract class Item                              // |-- Item
sealed abstract class Field extends Item                // |   |-- Field
case class Attr(key: Text, value: Value) extends Field  // |   |   |-- Attr
case class Slot(key: Value, value: Value) extends Field // |   |   |-- Slot
sealed abstract class Value extends Item                // |   |-- Value
case class Record(items: Item*) extends Value           // |   |   |-- Record
case class Text(toString: String) extends Value         // |   |   |-- Text
case class Number(toDouble: Double) extends Value       // |   |   |-- Number
case class Data(toArray: Array[Byte]) extends Value     // |   |   |-- Data
case object Extant extends Value                        // |   |   |-- Extant
case object Absent extends Value                        // |   |   |-- Absent
```

## Scala Library

To get started with the RECON Scala library, add the `recon-scala` dependency
to your SBT build.

```
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "net.coeffect" %% "recon-scala" % "0.0.0-SNAPSHOT"
```

The library support compile-time RECON string interpolation.

```scala
scala> import recon._
import recon._

scala> val label = "Example"
label: String = Example

scala> recon"""@a(href:"example.com")[$label]"""
res0: recon.Recon.Value = Record(Attr("a", Record(Slot("href", Text("example.com")))), Text("Example"))

scala> recon"""{1, 2 3, 4}"""
<console>:11: error: expected '}', ';', ',', or newline, but found '3'
       recon"""{1, 2 3, 4}"""
                     ^
```

Of course, you can parse and serialize RECON at runtime too.

```scala
scala> import recon._
import recon._

scala> val event = Value.parseRecon("""@event("onClick")""")
event: recon.Recon.Value = Record(Attr("event", Text("onClick")))

scala> event.toRecon
res0: String = @event("onClick")
```

Use the `/` operator to select named fields:

```scala
scala> import recon._
import recon._

scala> val msg = recon"""{from:"me",to:"you"}"""
msg: recon.Recon.Value = Record(Slot("from", Text("me")), Slot("to", Text("you")))

scala> msg / "from"
res0: recon.Recon.Value = Text("me")

scala> msg / "to"
res1: recon.Recon.Value = Text("you")

scala> msg / "body"
res2: recon.Recon.Value = Absent
```

Higher order collection operations and transformations just work.  The
lightweight basis collections library provides collection utilities as
unintrusive implicit extension methods, and implements them using fast,
inlined macros.

```scala
scala> import recon._
import recon._

scala> val list = recon"@ol{@li[a],@li[b],@li[c]}"
list: recon.Recon.Value = Record(Attr("ol"), Record(Attr("li"), Text("a")), Record(Attr("li"), Text("b")), Record(Attr("li"), Text("c")))

scala> list.asRecord.map {
     |   case Attr("ol", _) => Attr("ul")
     |   case item => item
     | }
res0: recon.Record = Record(Attr("ul"), Record(Attr("li"), Text("a")), Record(Attr("li"), Text("b")), Record(Attr("li"), Text("c")))
```

The `Mold` typeclass projects RECON values to Scala values, and vice versa.
The pattern of projecting weakly typed structural values to strongly typed
data models allows the RECON language to serve as an expressive and malleable
syntax for domain specific data formats.  `cast` and `coerce` methods project
RECON items to Scala values using molds.

```scala
scala> import recon._; import basis.collections._
import recon._
import basis.collections._

scala> recon"""{ 1, @prime 2, "3" }""".coerce[Array[Int]]
res0: Array[Int] = Array(1, 2, 3)

scala> recon"""@alpha { a: 1, b: 2 }""".coerce[Map[String, Int]]
res1: basis.collections.Map[String,Int] = HashTrieMap("a" -> 1, "b" -> 2)

scala> Recon(Seq(0.5, 0.25))
res2: recon.Recon.Value = Record(Number(0.5), Number(0.25))
```

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

NameChar ::=  NameStartChar | "-" | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]

MarkupChar ::= Char - ('\\' | '@' | '{' | '}' | '[' | ']')

StringChar ::= Char - ('"' | '\\' | '@' | '{' | '}' | '[' | ']' | '\b' | '\f' | '\n' | '\r' | '\t')

CharEscape ::= '\\' ('"' | '\\' | '/' | '@' | '{' | '}' | '[' | ']' | 'b' | 'f' | 'n' | 'r' | 't')

Base64Char ::= [A-Za-z0-9+/]

Block ::= WS* Slot SP* ((',' | ';' | NL) Block)? WS*

Attr ::= '@' Ident ('(' WS* Block WS* ')')?

Slot ::= BlockValue (SP* ':' SP* BlockValue)?

BlockValue ::= ((Attr SP* BlockValue) | Record | Markup | Ident | String | Number | Data)?

InlineValue ::= ((Attr SP* InlineValue) | Record | Markup)?

Record ::= '{' WS* Block WS* '}'

Markup ::= '[' (MarkupChar* | CharEscape | InlineValue)* ']'

String ::= '"' (StringChar* | CharEscape)* '"'

Data ::= '%' (Base64Char{4})* (Base64Char Base64Char ((Base64Char '=') | ('=' '=')))?

Number ::= '-'? (([1-9] [0-9]*) | [0-9]) ('.' [0-9]+)? (('E' | 'e') ('+' | '-')? [0-9]+)?

Ident ::= NameStartChar NameChar*
```

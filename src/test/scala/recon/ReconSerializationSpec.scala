package recon

import org.scalatest._

class ReconSerializationSpec extends FreeSpec with Matchers {
  override def suiteName = "RECON serialization"

  "should serialize absent values" in {
    Absent.toRecon should equal ("")
  }

  "should serialize empty records" in {
    Record.empty.toRecon should equal ("{}")
  }

  "should serialize unary records" in {
    Record(Number(1)).toRecon should equal ("{1}")
  }

  "should serialize non-empty records" in {
    Record(Number(1), Number(2), Text("3"), True).toRecon should equal ("{1,2,\"3\",true}")
  }

  "should serialize empty text" in {
    Text.empty.toRecon should equal ("\"\"")
  }

  "should serialize non-empty text" in {
    Text("Hello, world!").toRecon should equal ("\"Hello, world!\"")
  }

  "should serialize identifiers" in {
    Text("test").toRecon should equal ("test")
  }

  "should serialize empty data" in {
    Data.empty.toRecon should equal ("%")
  }

  "should serialize non-empty data" in {
    Data("AA==").toRecon should equal ("%AA==")
  }

  "should serialize numbers" in {
    Number(0).toRecon should equal ("0")
    Number(1).toRecon should equal ("1")
    Number(-1).toRecon should equal ("-1")
    Number(15).toRecon should equal ("15")
    Number(-20).toRecon should equal ("-20")
    Number(3.14).toRecon should equal ("3.14")
    Number(-0.5).toRecon should equal ("-0.5")
    Number(6.02E23).toRecon should equal ("6.02E23")
  }

  "should serialize booleans" in {
    True.toRecon should equal ("true")
    False.toRecon should equal ("false")
  }

  "should serialize extant attributes with no parameters" in {
    Record(Attr("answer")).toRecon should equal ("@answer")
  }

  "should serialize extant attributes with single parameters" in {
    Record(Attr("answer", Record.empty)).toRecon should equal ("@answer({})")
    Record(Attr("answer", Text("42"))).toRecon should equal ("@answer(\"42\")")
    Record(Attr("answer", Number(42))).toRecon should equal ("@answer(42)")
    Record(Attr("answer", True)).toRecon should equal ("@answer(true)")
  }

  "should serialize extant attributes with multiple parameters" in {
    val record = Record(Attr("answer", Record(Number(42), True)))
    record.toRecon should equal ("@answer(42,true)")
  }

  "should serialize extant attributes with named parameters" in {
    val record = Record(Attr("answer", Record(Slot("number", Number(42)))))
    record.toRecon should equal ("@answer(number:42)")
  }

  "should serialize records with ident keyed slots" in {
    val record = Record(Slot("a", Number(1)), False, Slot("c", Number(3)))
    record.toRecon should equal ("{a:1,false,c:3}")
  }

  "should serialize records with value keyed slots" in {
    val record = Record(Slot(Number(1), Text("one")), Slot(Record(Attr("id"), Text("foo")), Text("bar")))
    record.toRecon should equal ("{1:one,@id foo:bar}")
  }

  "should serialize records with extant slots" in {
    val record = Record(Slot(Text("blank")))
    record.toRecon should equal ("{blank:}")
  }

  "should serialize prefix attributed empty records" in {
    Record(Attr("hello"), Record.empty).toRecon should equal ("@hello{{}}")
  }

  "should serialize prefix attributed empty text" in {
    Record(Attr("hello"), Text.empty).toRecon should equal ("@hello\"\"")
  }

  "should serialize prefix attributed non-empty text" in {
    Record(Attr("hello"), Text("world!")).toRecon should equal ("@hello\"world!\"")
  }

  "should serialize prefix attributed numbers" in {
    Record(Attr("answer"), Number(42)).toRecon should equal ("@answer 42")
  }

  "should serialize prefix attributed booleans" in {
    Record(Attr("answer"), True).toRecon should equal ("@answer true")
  }

  "should serialize prefix attributed slots" in {
    Record(Attr("hello"), Slot("subject", Text("world!"))).toRecon should equal ("@hello{subject:\"world!\"}")
  }

  "should serialize postfix attributed empty records" in {
    Record(Record.empty, Attr("signed")).toRecon should equal ("{{}}@signed")
  }

  "should serialize postfix attributed empty text" in {
    Record(Text.empty, Attr("signed")).toRecon should equal ("\"\"@signed")
  }

  "should serialize postfix attributed non-empty text" in {
    Record(Text("world!"), Attr("signed")).toRecon should equal ("\"world!\"@signed")
  }

  "should serialize postfix attributed numbers" in {
    Record(Number(42), Attr("signed")).toRecon should equal ("42@signed")
  }

  "should serialize postfix attributed booleans" in {
    Record(True, Attr("signed")).toRecon should equal ("true@signed")
  }

  "should serialize postfix attributed slots" in {
    Record(Slot("subject", Text("world!")), Attr("signed")).toRecon should equal ("{subject:\"world!\"}@signed")
  }

  "should serialize single values with multiple postfix attributes" in {
    Record(Number(6), Attr("months"), Attr("remaining")).toRecon should equal ("6@months@remaining")
  }

  "should serialize single values with circumfix attributes" in {
    Record(Attr("a"), Attr("b"), False, Attr("x"), Attr("y")).toRecon should equal ("@a@b false@x@y")
  }

  "should serialize single values with interspersed attributes" in {
    Record(Attr("a"), Number(1), Attr("b"), Number(2)).toRecon should equal ("@a 1@b 2")
  }

  "should serialize single values with interspersed attribute groups" in {
    Record(Attr("a"), Attr("b"), Number(1), Attr("c"), Attr("d"), Number(2)).toRecon should equal ("@a@b 1@c@d 2")
  }

  "should serialize multiple items with multiple postfix attributes" in {
    Record(Number(1), Number(2), Attr("x"), Attr("y")).toRecon should equal ("{1,2}@x@y")
  }

  "should serialize multiple items with circumfix attributes" in {
    Record(Attr("a"), Attr("b"), Number(1), Number(2), Attr("x"), Attr("y")).toRecon should equal ("@a@b{1,2}@x@y")
  }

  "should serialize multiple items with interspersed attributes" in {
    Record(Attr("a"), Number(1), Number(2), Attr("b"), Number(3), Number(4)).toRecon should equal ("@a{1,2}@b{3,4}")
  }

  "should serialize multiple items with interspersed attribute groups" in {
    Record(Attr("a"), Attr("b"), Number(1), Number(2), Attr("c"), Attr("d"), Number(3), Number(4)).toRecon should equal ("@a@b{1,2}@c@d{3,4}")
  }

  "should serialize markup" in {
    val record1 = Record(Text("Hello, "), Record(Attr("em"), Text("world")), Text("!"))
    record1.toRecon should equal ("[Hello, @em[world]!]")
    val record2 =
      Record(
        Text("Hello, "),
        Record(Attr("em", Record(Slot("class", Text("subject")))), Text("world")),
        Text("!"))
    record2.toRecon should equal ("[Hello, @em(class:subject)[world]!]")
  }

  "should serialize nested markup" in {
    val record = Record(Text("X "), Record(Attr("p"), Text("Y "), Record(Attr("q"), Text("Z")), Text(".")), Text("."))
    record.toRecon should equal ("[X @p[Y @q[Z].].]")
  }

  "should serialize nested markup with non-prefix attributes" in {
    val record = Record(Text("X "), Record(Attr("p"), Text("Y."), Attr("q")), Text("."))
    record.toRecon should equal ("[X {@p\"Y.\"@q}.]")
  }

  "should serialize markup in attribute parameters" in {
    val record = Record(Attr("msg", Record(Text("Hello, "), Record(Attr("em"), Text("world")), Text("!"))))
    record.toRecon should equal ("@msg([Hello, @em[world]!])")
  }

  "should serialize markup-embedded values" in {
    val record1 = Record(Text("Hello, "), Number(6), Text("!"))
    record1.toRecon should equal ("[Hello, {6}!]")
    val record2 = Record(Text("Hello, "), Number(6), Number(7), Text("!"))
    record2.toRecon should equal ("[Hello, {6,7}!]")
  }

  "should serialize markup-embedded values with subsequent attributes" in {
    val record1 = Record(Text("Wait "), Number(1), Attr("second"), Text(" longer"), Record(Attr("please")))
    record1.toRecon should equal ("[Wait {1}]@second[ longer@please]")
    val record2 = Record(Text("Wait "), Number(1), Number(2), Attr("second"), Text(" longer"), Record(Attr("please")))
    record2.toRecon should equal ("[Wait {1,2}]@second[ longer@please]")
  }

  "should serialize markup-embedded records" in {
    val record1 = Record(Text("Hello, "), Record.empty, Text("!"))
    record1.toRecon should equal ("[Hello, {{}}!]")
    val record2 = Record(Text("Hello, "), Record(Number(1)), Text("!"))
    record2.toRecon should equal ("[Hello, {{1}}!]")
    val record3 = Record(Text("Hello, "), Record(Number(1), Number(2)), Text("!"))
    record3.toRecon should equal ("[Hello, {{1,2}}!]")
  }

  "should serialize markup-embedded attributed values" in {
    val record = Record(Text("Hello, "), Record(Attr("number"), Number(6)), Text("!"))
    record.toRecon should equal ("[Hello, @number{6}!]")
  }

  "should serialize markup-embedded attributed records" in {
    val record = Record(Text("Hello, "), Record(Attr("choice"), Text("Earth"), Text("Mars")), Text("!"))
    record.toRecon should equal ("[Hello, @choice{Earth,Mars}!]")
  }

  "should serialize markup-embedded records with non-prefix attributes" in {
    val record = Record(Text("Hello, "), Record(Number(1), Attr("second")), Text("!"))
    record.toRecon should equal ("[Hello, {1@second}!]")
  }
}

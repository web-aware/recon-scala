package recon

import org.scalatest._

trait ReconBehaviors extends Matchers { this: FlatSpec =>
  def SerializesRecon(recon: Recon): Unit = {
    import recon._

    it should "serialize absent values" in {
      Absent.toRecon should equal ("")
    }

    it should "serialize empty records" in {
      Record.empty.toRecon should equal ("{}")
    }

    it should "serialize non-empty records" in {
      Record(Number(1), Number(2), Text("3"), True).toRecon should equal ("{1,2,\"3\",true}")
    }

    it should "serialize empty text" in {
      Text.empty.toRecon should equal ("\"\"")
    }

    it should "serialize non-empty text" in {
      Text("Hello, world!").toRecon should equal ("\"Hello, world!\"")
    }

    it should "serialize identifiers" in {
      Text("test").toRecon should equal ("test")
    }

    it should "serialize empty data" in {
      Data.empty.toRecon should equal ("%")
    }

    it should "serialize non-empty data" in {
      Data("AA==").toRecon should equal ("%AA==")
    }

    it should "serialize numbers" in {
      Number(0).toRecon should equal ("0")
      Number(1).toRecon should equal ("1")
      Number(-1).toRecon should equal ("-1")
      Number(15).toRecon should equal ("15")
      Number(-20).toRecon should equal ("-20")
      Number(3.14).toRecon should equal ("3.14")
      Number(-0.5).toRecon should equal ("-0.5")
      Number(6.02E23).toRecon should equal ("6.02E23")
    }

    it should "serialize booleans" in {
      True.toRecon should equal ("true")
      False.toRecon should equal ("false")
    }

    it should "serialize extant attributes with no parameters" in {
      Record(Attr("answer")).toRecon should equal ("@answer")
    }

    it should "serialize extant attributes with single parameters" in {
      Record(Attr("answer", Record.empty)).toRecon should equal ("@answer()")
      Record(Attr("answer", Text("42"))).toRecon should equal ("@answer(\"42\")")
      Record(Attr("answer", Number(42))).toRecon should equal ("@answer(42)")
      Record(Attr("answer", True)).toRecon should equal ("@answer(true)")
    }

    it should "serialize extant attributes with multiple parameters" in {
      val record = Record(Attr("answer", Record(Number(42), True)))
      record.toRecon should equal ("@answer(42,true)")
    }

    it should "serialize extant attributes with named parameters" in {
      val record = Record(Attr("answer", Record(Slot("number", Number(42)))))
      record.toRecon should equal ("@answer(number:42)")
    }

    it should "serialize records with ident keyed slots" in {
      val record = Record(Slot("a", Number(1)), False, Slot("c", Number(3)))
      record.toRecon should equal ("{a:1,false,c:3}")
    }

    it should "serialize records with value keyed slots" in {
      val record = Record(Slot(Number(1), Text("one")), Slot(Record(Attr("id"), Text("foo")), Text("bar")))
      record.toRecon should equal ("{1:one,@id foo:bar}")
    }

    it should "serialize records with extant slots" in {
      val record = Record(Slot(Text("blank")))
      record.toRecon should equal ("{blank:}")
    }

    it should "serialize attributed empty records" in {
      Record(Attr("hello"), Record.empty).toRecon should equal ("@hello{{}}")
    }

    it should "serialize attributed empty text" in {
      Record(Attr("hello"), Text.empty).toRecon should equal ("@hello \"\"")
    }

    it should "serialize attributed non-empty text" in {
      Record(Attr("hello"), Text("world!")).toRecon should equal ("@hello \"world!\"")
    }

    it should "serialize attributed numbers" in {
      Record(Attr("answer"), Number(42)).toRecon should equal ("@answer 42")
    }

    it should "serialize attributed booleans" in {
      Record(Attr("answer"), True).toRecon should equal ("@answer true")
    }

    it should "serialize attributed slots" in {
      Record(Attr("hello"), Slot("subject", Text("world!"))).toRecon should equal ("@hello{subject:\"world!\"}")
    }

    it should "serialize postfix attributed empty records" in {
      Record(Record.empty, Attr("signed")).toRecon should equal ("{{}}@signed")
    }

    it should "serialize postfix attributed empty text" in {
      Record(Text.empty, Attr("signed")).toRecon should equal ("\"\"@signed")
    }

    it should "serialize postfix attributed non-empty text" in {
      Record(Text("world!"), Attr("signed")).toRecon should equal ("\"world!\"@signed")
    }

    it should "serialize postfix attributed numbers" in {
      Record(Number(42), Attr("signed")).toRecon should equal ("42@signed")
    }

    it should "serialize postfix attributed booleans" in {
      Record(True, Attr("signed")).toRecon should equal ("true@signed")
    }

    it should "serialize postfix attributed slots" in {
      Record(Slot("subject", Text("world!")), Attr("signed")).toRecon should equal ("{subject:\"world!\"}@signed")
    }

    it should "serialize single values with multiple postfix attributes" in {
      Record(Number(6), Attr("months"), Attr("remaining")).toRecon should equal ("6@months@remaining")
    }

    it should "serialize single values with circumfix attributes" in {
      Record(Attr("a"), Attr("b"), False, Attr("x"), Attr("y")).toRecon should equal ("@a@b false@x@y")
    }

    it should "serialize multiple items with multiple postfix attributes" in {
      Record(Number(1), Number(2), Attr("x"), Attr("y")).toRecon should equal ("{1,2}@x@y")
    }

    it should "serialize multiple items with circumfix attributes" in {
      Record(Attr("a"), Attr("b"), Number(1), Number(2), Attr("x"), Attr("y")).toRecon should equal ("@a@b{1,2}@x@y")
    }

    it should "serialize markup" in {
      val record1 = Record(Text("Hello, "), Record(Attr("em"), Text("world")), Text("!"))
      record1.toRecon should equal ("[Hello, @em[world]!]")
      val record2 =
        Record(
          Text("Hello, "),
          Record(Attr("em", Record(Slot("class", Text("subject")))), Text("world")),
          Text("!"))
      record2.toRecon should equal ("[Hello, @em(class:subject)[world]!]")
    }

    it should "serialize markup in attribute parameters" in {
      val record = Record(Attr("msg", Record(Text("Hello, "), Record(Attr("em"), Text("world")), Text("!"))))
      record.toRecon should equal ("@msg([Hello, @em[world]!])")
    }
  }
}

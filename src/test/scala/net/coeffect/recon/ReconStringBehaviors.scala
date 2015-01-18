package net.coeffect.recon

import org.scalatest._

trait ReconStringBehaviors extends Matchers { this: FlatSpec =>
  def InterpolatesReconLiterals(recon: Recon): Unit = {
    import recon._

    it should "interpolate empty input" in {
      recon"" should equal (Absent)
    }

    it should "interpolate empty records" in {
      recon"{}" should equal (Record.empty)
    }

    it should "interpolate empty markup" in {
      recon"[]" should equal (Text.empty)
    }

    it should "interpolate empty strings" in {
      recon""" "" """ should equal (Text.empty)
    }

    it should "interpolate non-empty strings" in {
      recon""" "test" """ should equal (Text("test"))
    }

    it should "interpolate strings with escapes" in {
      recon""" "\"\\\/\@\{\}\[\]\b\f\n\r\t" """ should equal (Text("\"\\/@{}[]\b\f\n\r\t"))
    }

    it should "interpolate positive integers" in {
      recon"0"  should equal (Number(0))
      recon"1"  should equal (Number(1))
      recon"5"  should equal (Number(5))
      recon"10" should equal (Number(10))
      recon"11" should equal (Number(11))
      recon"15" should equal (Number(15))
    }

    it should "interpolate negative integers" in {
      recon"-0"  should equal (Number(-0))
      recon"-1"  should equal (Number(-1))
      recon"-5"  should equal (Number(-5))
      recon"-10" should equal (Number(-10))
      recon"-11" should equal (Number(-11))
      recon"-15" should equal (Number(-15))
    }

    it should "interpolate positive decimals" in {
      recon"0.0"  should equal (Number(0.0))
      recon"0.5"  should equal (Number(0.5))
      recon"1.0"  should equal (Number(1.0))
      recon"1.5"  should equal (Number(1.5))
      recon"10.0" should equal (Number(10.0))
      recon"10.5" should equal (Number(10.5))
    }

    it should "interpolate negative decimals" in {
      recon"-0.0"  should equal (Number(-0.0))
      recon"-0.5"  should equal (Number(-0.5))
      recon"-1.0"  should equal (Number(-1.0))
      recon"-1.5"  should equal (Number(-1.5))
      recon"-10.0" should equal (Number(-10.0))
      recon"-10.5" should equal (Number(-10.5))
    }

    it should "interpolate positive decimals with exponents" in {
      recon"4e2"    should equal (Number(400.0))
      recon"4E2"    should equal (Number(400.0))
      recon"4e+2"   should equal (Number(400.0))
      recon"4E+2"   should equal (Number(400.0))
      recon"4e-2"   should equal (Number(0.04))
      recon"4E-2"   should equal (Number(0.04))
      recon"4.0e2"  should equal (Number(400.0))
      recon"4.0E2"  should equal (Number(400.0))
      recon"4.0e+2" should equal (Number(400.0))
      recon"4.0E+2" should equal (Number(400.0))
      recon"4.0e-2" should equal (Number(0.04))
      recon"4.0E-2" should equal (Number(0.04))
    }

    it should "interpolate negative decimals with exponents" in {
      recon"-4e2"    should equal (Number(-400.0))
      recon"-4E2"    should equal (Number(-400.0))
      recon"-4e+2"   should equal (Number(-400.0))
      recon"-4E+2"   should equal (Number(-400.0))
      recon"-4e-2"   should equal (Number(-0.04))
      recon"-4E-2"   should equal (Number(-0.04))
      recon"-4.0e2"  should equal (Number(-400.0))
      recon"-4.0E2"  should equal (Number(-400.0))
      recon"-4.0e+2" should equal (Number(-400.0))
      recon"-4.0E+2" should equal (Number(-400.0))
      recon"-4.0e-2" should equal (Number(-0.04))
      recon"-4.0E-2" should equal (Number(-0.04))
    }

    it should "interpolate booleans" in {
      recon"#true"  should equal (True)
      recon"#false" should equal (False)
    }

    it should "interpolate single values with trailing commas" in {
      recon"1," should equal (Number(1))
    }

    it should "interpolate single values with trailing semicolons" in {
      recon"1;" should equal (Number(1))
    }

    it should "interpolate multiple comma-separated items" in {
      val xs = Record(Number(1), Number(2), Number(3), Number(4))
      recon"  1, 2,3 ,4  " should equal (xs)
      recon"{ 1, 2,3 ,4 }" should equal (xs)
    }

    it should "interpolate multiple semicolon-separated items" in {
      val xs = Record(Number(1), Number(2), Number(3), Number(4))
      recon"  1; 2;3 ;4  " should equal (xs)
      recon"{ 1; 2;3 ;4 }" should equal (xs)
    }

    it should "interpolate multiple items with trailing commas" in {
      val xs = Record(Number(1), Number(2), Number(3), Number(4))
      recon"  1, 2,3 ,4,  " should equal (xs)
      recon"{ 1, 2,3 ,4, }" should equal (xs)
    }

    it should "interpolate multiple items with trailing semicolones" in {
      val xs = Record(Number(1), Number(2), Number(3), Number(4))
      recon"  1; 2;3 ;4;  " should equal (xs)
      recon"{ 1; 2;3 ;4; }" should equal (xs)
    }

    it should "interpolate multiple newline-separated items" in {
      val xs = Record(Number(1), Number(2), Number(3))
      recon"""
        1
        2
        3
      """ should equal (xs)
      recon""" {
        1
        2
        3
      } """ should equal (xs)
    }

    it should "interpolate multiple items with mixed separators" in {
      val xs = Record(Number(1), Number(2), Number(3), Number(4), Number(5))
      recon"""
        1, 2
        3
        4; 5
      """ should equal (xs)
      recon""" {
        1, 2
        3
        4; 5
      } """ should equal (xs)
    }

    it should "interpolate multiple command-newline-separated items" in {
      val xs = Record(Number(1), Number(2), Number(3))
      recon"""
        1,
        2,
        3
      """ should equal (xs)
      recon""" {
        1,
        2,
        3
      } """ should equal (xs)
    }

    it should "interpolate multiple semicolon-newline-separated items" in {
      val xs = Record(Number(1), Number(2), Number(3))
      recon"""
        1;
        2;
        3
      """ should equal (xs)
      recon""" {
        1;
        2;
        3
      } """ should equal (xs)
    }

    it should "interpolate heterogeneous top-level items as a record" in {
      recon"""
        record:  {}
        markup:  []
        ""
        integer: 0
        decimal: 0.0
        #true
        #false
      """ should equal (Record(
        Slot("record", Record.empty),
        Slot("markup", Text.empty),
        Text.empty,
        Slot("integer", Number(0)),
        Slot("decimal", Number(0.0)),
        True,
        False
      ))
    }

    it should "interpolate heterogeneous items in a record" in {
      recon""" {
        record:  {}
        markup:  []
        ""
        integer: 0
        decimal: 0.0
        #true
        #false
      } """ should equal (Record(
        Slot("record", Record.empty),
        Slot("markup", Text.empty),
        Text.empty,
        Slot("integer", Number(0)),
        Slot("decimal", Number(0.0)),
        True,
        False
      ))
    }

    it should "interpolate single extant attributes with no parameters" in {
      recon"@test" should equal (Record(Attr("test")))
    }

    it should "interpolate single extant attributes with empty parameters" in {
      recon"@test()" should equal (Record(Attr("test")))
    }

    it should "interpolate single extant attributes with single parameters" in {
      recon"""@hello({})"""      should equal (Record(Attr("hello", Record.empty)))
      recon"""@hello([world])""" should equal (Record(Attr("hello", Text("world"))))
      recon"""@hello("world")""" should equal (Record(Attr("hello", Text("world"))))
      recon"""@hello(42)"""      should equal (Record(Attr("hello", Number(42))))
      recon"""@hello(#true)"""   should equal (Record(Attr("hello", True)))
      recon"""@hello(#false)"""  should equal (Record(Attr("hello", False)))
    }

    it should "interpolate single extant attributes with multiple parameters" in {
      recon"""@hello("world", 42, #true)""" should equal (
        Record(Attr("hello", Record(Text("world"), Number(42), True))))
      recon"""@hello("world"; 42; #true)""" should equal (
        Record(Attr("hello", Record(Text("world"), Number(42), True))))
      recon"""@hello("world"
                     42
                     #true)""" should equal (
        Record(Attr("hello", Record(Text("world"), Number(42), True))))
    }

    it should "interpolate single extant attributes with named parameters" in {
      recon"""@hello(name: "world")""" should equal (
        Record(Attr("hello", Record(Slot("name", Text("world"))))))
      recon"""@hello(name: "world", number: 42, #false)""" should equal (
        Record(Attr("hello", Record(Slot("name", Text("world")), Slot("number", Number(42)), False))))
    }

    it should "interpolate multiple extant attributes with no parameters" in {
      recon"@a @b" should equal (Record(Attr("a"), Attr("b")))
    }

    it should "interpolate multiple extant attributes with empty parameters" in {
      recon"@a() @b()" should equal (Record(Attr("a"), Attr("b")))
    }

    it should "interpolate multiple extant attributes with single parameters" in {
      recon"""@a({}) @b([])""" should equal (Record(Attr("a", Record.empty), Attr("b", Text.empty)))
      recon"""@a("test") @b(42)""" should equal (Record(Attr("a", Text("test")), Attr("b", Number(42))))
      recon"""@a(#true) @b(#false)""" should equal (Record(Attr("a", True), Attr("b", False)))
    }

    it should "interpolate multiple extant attributes with complex parameters" in {
      recon"""@hello("world", 42) @test(name: "interpolate", pending: #false)""" should equal(
        Record(
          Attr("hello", Record(Text("world"), Number(42))),
          Attr("test", Record(Slot("name", Text("interpolate")), Slot("pending", False)))))
    }

    it should "interpolate attributed empty records" in {
      recon"""@hello {}"""   should equal (Record(Attr("hello")))
      recon"""@hello() {}""" should equal (Record(Attr("hello")))
      recon"""@hello("world") {}""" should equal (Record(Attr("hello", Text("world"))))
      recon"""@hello(name: "world") {}""" should equal(
        Record(Attr("hello", Record(Slot("name", Text("world"))))))
    }

    it should "interpolate attributed nonempty records" in {
      recon"""@hello { {}, [] }"""   should equal (Record(Attr("hello"), Record.empty, Text.empty))
      recon"""@hello() { "world", 42 }""" should equal (Record(Attr("hello"), Text("world"), Number(42)))
      recon"""@hello(name: "world") { number: 42, #true }""" should equal(
        Record(Attr("hello", Record(Slot("name", Text("world")))), Slot("number", Number(42)), True))
    }

    it should "interpolate attributed empty markup" in {
      recon"""@hello []"""   should equal (Record(Attr("hello")))
      recon"""@hello() []""" should equal (Record(Attr("hello")))
      recon"""@hello("world") []""" should equal (Record(Attr("hello", Text("world"))))
      recon"""@hello(name: "world") []""" should equal(
        Record(Attr("hello", Record(Slot("name", Text("world"))))))
    }

    it should "interpolate attributed empty strings" in {
      recon"""@hello "" """   should equal (Record(Attr("hello"), Text.empty))
      recon"""@hello() "" """ should equal (Record(Attr("hello"), Text.empty))
      recon"""@hello("world") "" """ should equal (Record(Attr("hello", Text("world")), Text.empty))
      recon"""@hello(name: "world") "" """ should equal(
        Record(Attr("hello", Record(Slot("name", Text("world")))), Text.empty))
    }

    it should "interpolate attributed non-empty strings" in {
      recon"""@hello "test" """   should equal (Record(Attr("hello"), Text("test")))
      recon"""@hello() "test" """ should equal (Record(Attr("hello"), Text("test")))
      recon"""@hello("world") "test" """ should equal (Record(Attr("hello", Text("world")), Text("test")))
      recon"""@hello(name: "world") "test" """ should equal(
        Record(Attr("hello", Record(Slot("name", Text("world")))), Text("test")))
    }

    it should "interpolate attributed numbers" in {
      recon"""@hello 42"""   should equal (Record(Attr("hello"), Number(42)))
      recon"""@hello() -42""" should equal (Record(Attr("hello"), Number(-42)))
      recon"""@hello("world") 42.0""" should equal (Record(Attr("hello", Text("world")), Number(42.0)))
      recon"""@hello(name: "world") -42.0""" should equal(
        Record(Attr("hello", Record(Slot("name", Text("world")))), Number(-42.0)))
    }

    it should "interpolate attributed booleans" in {
      recon"""@hello #true"""   should equal (Record(Attr("hello"), True))
      recon"""@hello() #false""" should equal (Record(Attr("hello"), False))
      recon"""@hello("world") #true""" should equal (Record(Attr("hello", Text("world")), True))
      recon"""@hello(name: "world") #false""" should equal(
        Record(Attr("hello", Record(Slot("name", Text("world")))), False))
    }

    it should "interpolate plain markup as text" in {
      recon"[test]" should equal (Text("test"))
    }

    it should "interpolate plain markup with escapes" in {
      recon"""[\"\\\/\@\{\}\[\]\b\f\n\r\t]""" should equal (Text("\"\\/@{}[]\b\f\n\r\t"))
    }

    it should "interpolate markup with embedded markup" in {
      recon"""[Hello, [good] world!]""" should equal (Record(Text("Hello, "), Text("good"), Text(" world!")))
    }

    it should "interpolate markup with embedded structure" in {
      recon"""[Hello{}world]""" should equal (Record(Text("Hello"), Text("world")))
      recon"""[A: {"answer"}.]""" should equal (Record(Text("A: "), Text("answer"), Text(".")))
      recon"""[A: {42}.]""" should equal (Record(Text("A: "), Number(42), Text(".")))
      recon"""[A: {#true}.]""" should equal (Record(Text("A: "), True, Text(".")))
      recon"""[A: {#false}.]""" should equal (Record(Text("A: "), False, Text(".")))
      recon"""[A: {answer:0.0}.]""" should equal (Record(Text("A: "), Slot("answer", Number(0.0)), Text(".")))
    }

    it should "interpolate markup with embedded attributes" in {
      recon"""[A: @answer.]""" should equal (Record(Text("A: "), Record(Attr("answer")), Text(".")))
      recon"""[A: @answer().]""" should equal (Record(Text("A: "), Record(Attr("answer")), Text(".")))
      recon"""[A: @answer("secret").]""" should equal (
        Record(Text("A: "), Record(Attr("answer", Text("secret"))), Text(".")))
      recon"""[A: @answer(number: 42, #true).]""" should equal (
        Record(Text("A: "), Record(Attr("answer", Record(Slot("number", Number(42)), True))), Text(".")))
    }

    it should "interpolate markup with embedded attributed markup" in {
      recon"""[Hello, @em[world]!]""" should equal (
        Record(Text("Hello, "), Record(Attr("em"), Text("world")), Text("!")))
      recon"""[Hello, @em() [world]!]""" should equal (
        Record(Text("Hello, "), Record(Attr("em"), Text("world")), Text("!")))
      recon"""[Hello, @em("italic")[world]!]""" should equal (
        Record(Text("Hello, "), Record(Attr("em", Text("italic")), Text("world")), Text("!")))
      recon"""[Hello, @em(class:"subject",style:"italic")[world]!]""" should equal (
        Record(
          Text("Hello, "),
          Record(
            Attr("em", Record(Slot("class", Text("subject")), Slot("style", Text("italic")))),
            Text("world")),
          Text("!")))
    }

    it should "interpolate markup with embedded attributed values" in {
      recon"""[A: @answer{42}.]""" should equal (
        Record(Text("A: "), Record(Attr("answer"), Number(42)), Text(".")))
      recon"""[A: @answer() {42}.]""" should equal (
        Record(Text("A: "), Record(Attr("answer"), Number(42)), Text(".")))
      recon"""[A: @answer("secret") {42}.]""" should equal (
        Record(Text("A: "), Record(Attr("answer", Text("secret")), Number(42)), Text(".")))
      recon"""[A: @answer(number: 42, "secret") {#true}.]""" should equal (
        Record(
          Text("A: "),
          Record(
            Attr("answer", Record(Slot("number", Number(42)), Text("secret"))),
            True),
          Text(".")))
    }
  }

  def SubstitutesReconVariables(recon: Recon): Unit = {
    import recon._

    it should "substitute top-level record variables" in {
      val x = recon"{}"
      recon"$x" should equal (x)
      val y = recon"{a:1,b:#true}"
      recon"$y" should equal (y)
    }

    it should "substitute top-level markup variables" in {
      val x = recon"[]"
      recon"$x" should equal (x)
      val y = recon"[test]"
      recon"$y" should equal (y)
    }

    it should "substitute top-level string variables" in {
      val x = recon""" "" """
      recon"$x" should equal (x)
      val y = recon""" "test" """
      recon"$y" should equal (y)
    }

    it should "substitute top-level number variables" in {
      val x = recon"42"
      recon"$x" should equal (x)
      val y = recon"3.14"
      recon"$y" should equal (y)
    }

    it should "substitute top-level boolean variables" in {
      val x = recon"#true"
      recon"$x" should equal (x)
      val y = recon"#false"
      recon"$y" should equal (y)
    }

    it should "substitute multiple heterogeneous variables" in {
      val a = Record.empty
      val b = Record(True)
      val c = Text.empty
      val d = Text("test")
      val e = Number(42)
      val f = True
      val g = False
      recon" $a;$b;$c;$d;$e;$f;$g " should equal (Record(a, b, c, d, e, f, g))
      recon" $a,$b,$c,$d,$e,$f,$g " should equal (Record(a, b, c, d, e, f, g))
      recon"{$a;$b;$c;$d;$e;$f;$g}" should equal (Record(a, b, c, d, e, f, g))
      recon"{$a,$b,$c,$d,$e,$f,$g}" should equal (Record(a, b, c, d, e, f, g))
    }

    it should "substitute attributed variables" in {
      val world = Text("world")
      recon"@hello $world" should equal (Record(Attr("hello"), world))
      recon"@hello() $world" should equal (Record(Attr("hello"), world))
      recon"@hello @good $world" should equal (Record(Attr("hello"), Attr("good"), world))
      recon"""@span(class:"subject") $world""" should equal (
        Record(Attr("span", Record(Slot("class", Text("subject")))), world))
    }

    it should "substitute variables in slot values" in {
      val xs = recon"""{ 1, 2, 3 }"""
      recon"  xs: $xs  " should equal (Record(Slot("xs", xs)))
      recon"{ xs: $xs }" should equal (Record(Slot("xs", xs)))
      val ys = recon"""{ "a", "b", "c" }"""
      recon"  xs: $xs, ys: $ys  " should equal (Record(Slot("xs", xs), Slot("ys", ys)))
      recon"{ xs: $xs, ys: $ys }" should equal (Record(Slot("xs", xs), Slot("ys", ys)))
    }

    it should "substitute variables in attribute parameters" in {
      val x = Text("world")
      recon"@span($x)" should equal (Record(Attr("span", x)))
      recon"@span(class:$x)" should equal (Record(Attr("span", Record(Slot("class", x)))))
      recon"@span(#true,$x)" should equal (Record(Attr("span", Record(True, x))))
      recon"@span($x,#false)" should equal (Record(Attr("span", Record(x, False))))
      recon"@span(#true,$x,#false)" should equal (Record(Attr("span", Record(True, x, False))))
      recon"@span(#true,class:$x)" should equal (Record(Attr("span", Record(True, Slot("class", x)))))
      recon"@span(class:$x,#false)" should equal (Record(Attr("span", Record(Slot("class", x), False))))
      recon"@span(#true,class:$x,#false)" should equal (
        Record(Attr("span", Record(True, Slot("class", x), False))))
    }

    it should "substitute variables in markup" in {
      val x = Text("test")
      recon"[$x]" should equal (Record(x))
      val y = Number(42)
      recon"[$y]" should equal (Record(y))
      recon"[A: $y.]" should equal (Record(Text("A: "), y, Text(".")))
    }

    it should "substitute variables in nested markup" in {
      val x = Text("test")
      recon"[[$x]]" should equal (Record(x))
      val y = Number(42)
      recon"[[$y]]" should equal (Record(y))
      recon"[A: [$y].]" should equal (Record(Text("A: "), y, Text(".")))
      recon"[A: [$y.]]" should equal (Record(Text("A: "), y, Text(".")))
    }

    it should "substitute variables in embedded markup structure" in {
      val x = Text("test")
      recon"[{$x}]" should equal (Record(x))
      val y = Number(42)
      recon"[{$y}]" should equal (Record(y))
      recon"[A: {$y}.]" should equal (Record(Text("A: "), y, Text(".")))
    }

    it should "substitute variables in embedded markup slot values" in {
      val x = Text("foo")
      recon"[{answer:$x}]" should equal (Record(Slot("answer", x)))
      val y = Number(42)
      recon"[{answer:$y}]" should equal (Record(Slot("answer", y)))
      recon"[A: {answer:$y}.]" should equal (Record(Text("A: "), Slot("answer", y), Text(".")))
    }

    it should "substitute variables in markup-embedded attribute parameters" in {
      val x = Text("subject")
      recon"[Hello, @span($x)[world]!]" should equal (
        Record(Text("Hello, "), Record(Attr("span", x), Text("world")), Text("!")))
      recon"[Hello, @span(class:$x)[world]!]" should equal (
        Record(Text("Hello, "), Record(Attr("span", Record(Slot("class", x))), Text("world")), Text("!")))
    }
  }
}

package recon

import org.scalatest._

class ReconStringContextSpec extends FreeSpec with Matchers {
  override def suiteName = "recon\"\" string interpolation"

  "should compile empty input" in {
    recon"" should equal (Absent)
  }

  "should compile empty records" in {
    recon"{}" should equal (Record.empty)
  }

  "should compile empty markup" in {
    recon"[]" should equal (Record.empty)
  }

  "should compile empty strings" in {
    recon""" "" """ should equal (Text.empty)
  }

  "should compile non-empty strings" in {
    recon""" "test" """ should equal (Text("test"))
  }

  "should compile strings with escapes" in {
    recon""" "\"\\\/\@\{\}\[\]\b\f\n\r\t" """ should equal (Text("\"\\/@{}[]\b\f\n\r\t"))
  }

  "should compile empty data" in {
    recon"%" should equal (Data.empty)
  }

  "should compile non-empty data" in {
    recon"%AAAA" should equal (Data("AAAA"))
    recon"%AAA=" should equal (Data("AAA="))
    recon"%AA==" should equal (Data("AA=="))
    recon"%ABCDabcd12/+" should equal (Data("ABCDabcd12/+"))
  }

  "should compile positive integers" in {
    recon"0"  should equal (Number(0))
    recon"1"  should equal (Number(1))
    recon"5"  should equal (Number(5))
    recon"10" should equal (Number(10))
    recon"11" should equal (Number(11))
    recon"15" should equal (Number(15))
  }

  "should compile negative integers" in {
    recon"-0"  should equal (Number(-0))
    recon"-1"  should equal (Number(-1))
    recon"-5"  should equal (Number(-5))
    recon"-10" should equal (Number(-10))
    recon"-11" should equal (Number(-11))
    recon"-15" should equal (Number(-15))
  }

  "should compile positive decimals" in {
    recon"0.0"  should equal (Number(0.0))
    recon"0.5"  should equal (Number(0.5))
    recon"1.0"  should equal (Number(1.0))
    recon"1.5"  should equal (Number(1.5))
    recon"10.0" should equal (Number(10.0))
    recon"10.5" should equal (Number(10.5))
  }

  "should compile negative decimals" in {
    recon"-0.0"  should equal (Number(-0.0))
    recon"-0.5"  should equal (Number(-0.5))
    recon"-1.0"  should equal (Number(-1.0))
    recon"-1.5"  should equal (Number(-1.5))
    recon"-10.0" should equal (Number(-10.0))
    recon"-10.5" should equal (Number(-10.5))
  }

  "should compile positive decimals with exponents" in {
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

  "should compile negative decimals with exponents" in {
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

  "should compile booleans" in {
    recon"true"  should equal (True)
    recon"false" should equal (False)
  }

  "should compile single values with trailing commas" in {
    recon"1," should equal (Number(1))
  }

  "should compile single values with trailing semicolons" in {
    recon"1;" should equal (Number(1))
  }

  "should compile multiple comma separated items" in {
    val xs = Record(Number(1), Number(2), Number(3), Number(4))
    recon"  1, 2,3 ,4  " should equal (xs)
    recon"{ 1, 2,3 ,4 }" should equal (xs)
  }

  "should compile multiple semicolon separated items" in {
    val xs = Record(Number(1), Number(2), Number(3), Number(4))
    recon"  1; 2;3 ;4  " should equal (xs)
    recon"{ 1; 2;3 ;4 }" should equal (xs)
  }

  "should compile multiple items with trailing commas" in {
    val xs = Record(Number(1), Number(2), Number(3), Number(4))
    recon"  1, 2,3 ,4,  " should equal (xs)
    recon"{ 1, 2,3 ,4, }" should equal (xs)
  }

  "should compile multiple items with trailing semicolones" in {
    val xs = Record(Number(1), Number(2), Number(3), Number(4))
    recon"  1; 2;3 ;4;  " should equal (xs)
    recon"{ 1; 2;3 ;4; }" should equal (xs)
  }

  "should compile multiple newline separated items" in {
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

  "should compile multiple items with mixed separators" in {
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

  "should compile multiple comma-newline separated items" in {
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

  "should compile multiple semicolon-newline separated items" in {
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

  "should compile heterogeneous top-level items as a record" in {
    recon"""
      extant:
      record:  {}
      markup:  []
      ""
      %AA==
      integer: 0
      decimal: 0.0
      true
      false
    """ should equal (Record(
      Slot("extant"),
      Slot("record", Record.empty),
      Slot("markup", Record.empty),
      Text.empty,
      Data("AA=="),
      Slot("integer", Number(0)),
      Slot("decimal", Number(0.0)),
      True,
      False
    ))
  }

  "should compile heterogeneous items in a record" in {
    recon""" {
      extant:
      record:  {}
      markup:  []
      ""
      %AA==
      integer: 0
      decimal: 0.0
      true
      false
    } """ should equal (Record(
      Slot("extant"),
      Slot("record", Record.empty),
      Slot("markup", Record.empty),
      Text.empty,
      Data("AA=="),
      Slot("integer", Number(0)),
      Slot("decimal", Number(0.0)),
      True,
      False
    ))
  }

  "should compile single extant attributes with no parameters" in {
    recon"@test" should equal (Record(Attr("test")))
  }

  "should compile single extant attributes with empty parameters" in {
    recon"@test()" should equal (Record(Attr("test")))
  }

  "should compile single extant attributes with single parameters" in {
    recon"""@hello({})"""      should equal (Record(Attr("hello", Record.empty)))
    recon"""@hello([world])""" should equal (Record(Attr("hello", Record(Text("world")))))
    recon"""@hello("world")""" should equal (Record(Attr("hello", Text("world"))))
    recon"""@hello(42)"""      should equal (Record(Attr("hello", Number(42))))
    recon"""@hello(true)"""    should equal (Record(Attr("hello", True)))
    recon"""@hello(false)"""   should equal (Record(Attr("hello", False)))
  }

  "should compile single extant attributes with multiple parameters" in {
    recon"""@hello("world", %AA==, 42, true)""" should equal (
      Record(Attr("hello", Record(Text("world"), Data("AA=="), Number(42), True))))
    recon"""@hello("world"; %AA==; 42; true)""" should equal (
      Record(Attr("hello", Record(Text("world"), Data("AA=="), Number(42), True))))
    recon"""@hello("world"
                   %AA==
                   42
                   true)""" should equal (
      Record(Attr("hello", Record(Text("world"), Data("AA=="), Number(42), True))))
  }

  "should compile single extant attributes with named parameters" in {
    recon"""@hello(name: "world")""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world"))))))
    recon"""@hello(name: "world", data: %AA==, number: 42, false)""" should equal (
      Record(
        Attr(
          "hello",
          Record(
            Slot("name", Text("world")),
            Slot("data", Data("AA==")),
            Slot("number", Number(42)),
            False))))
  }

  "should compile multiple extant attributes with no parameters" in {
    recon"@a@b" should equal (Record(Attr("a"), Attr("b")))
    recon"@a @b" should equal (Record(Attr("a"), Attr("b")))
  }

  "should compile multiple extant attributes with empty parameters" in {
    recon"@a()@b()" should equal (Record(Attr("a"), Attr("b")))
    recon"@a() @b()" should equal (Record(Attr("a"), Attr("b")))
  }

  "should compile multiple extant attributes with single parameters" in {
    recon"""@a({}) @b([])""" should equal (Record(Attr("a", Record.empty), Attr("b", Text.empty)))
    recon"""@a("test") @b(42)""" should equal (Record(Attr("a", Text("test")), Attr("b", Number(42))))
    recon"""@a(true) @b(false)""" should equal (Record(Attr("a", True), Attr("b", False)))
  }

  "should compile multiple extant attributes with complex parameters" in {
    recon"""@hello("world", 42) @test(name: "compile", pending: false)""" should equal (
      Record(
        Attr("hello", Record(Text("world"), Number(42))),
        Attr("test", Record(Slot("name", Text("compile")), Slot("pending", False)))))
  }

  "should compile prefix attributed empty records" in {
    recon"""@hello {}""" should equal (Record(Attr("hello")))
    recon"""@hello() {}""" should equal (Record(Attr("hello")))
    recon"""@hello("world") {}""" should equal (Record(Attr("hello", Text("world"))))
    recon"""@hello(name: "world") {}""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world"))))))
  }

  "should compile prefix attributed non-empty records" in {
    recon"""@hello { {}, [] }""" should equal (Record(Attr("hello"), Record.empty, Text.empty))
    recon"""@hello() { "world", 42 }""" should equal (Record(Attr("hello"), Text("world"), Number(42)))
    recon"""@hello(name: "world") { number: 42, true }""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world")))), Slot("number", Number(42)), True))
  }

  "should compile prefix attributed empty markup" in {
    recon"""@hello []""" should equal (Record(Attr("hello")))
    recon"""@hello() []""" should equal (Record(Attr("hello")))
    recon"""@hello("world") []""" should equal (Record(Attr("hello", Text("world"))))
    recon"""@hello(name: "world") []""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world"))))))
  }

  "should compile prefix attributed non-empty markup" in {
    recon"""@hello [test]""" should equal (Record(Attr("hello"), Text("test")))
    recon"""@hello() [test]""" should equal (Record(Attr("hello"), Text("test")))
    recon"""@hello("world") [test]""" should equal (Record(Attr("hello", Text("world")), Text("test")))
    recon"""@hello(name: "world") [test]""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world")))), Text("test")))
  }

  "should compile prefix attributed empty strings" in {
    recon"""@hello "" """ should equal (Record(Attr("hello"), Text.empty))
    recon"""@hello() "" """ should equal (Record(Attr("hello"), Text.empty))
    recon"""@hello("world") "" """ should equal (Record(Attr("hello", Text("world")), Text.empty))
    recon"""@hello(name: "world") "" """ should equal (
      Record(Attr("hello", Record(Slot("name", Text("world")))), Text.empty))
  }

  "should compile prefix attributed non-empty strings" in {
    recon"""@hello "test" """ should equal (Record(Attr("hello"), Text("test")))
    recon"""@hello() "test" """ should equal (Record(Attr("hello"), Text("test")))
    recon"""@hello("world") "test" """ should equal (Record(Attr("hello", Text("world")), Text("test")))
    recon"""@hello(name: "world") "test" """ should equal (
      Record(Attr("hello", Record(Slot("name", Text("world")))), Text("test")))
  }

  "should compile prefix attributed empty data" in {
    recon"""@hello % """ should equal (Record(Attr("hello"), Data.empty))
    recon"""@hello() % """ should equal (Record(Attr("hello"), Data.empty))
    recon"""@hello("world") % """ should equal (Record(Attr("hello", Text("world")), Data.empty))
    recon"""@hello(name: "world") % """ should equal (
      Record(Attr("hello", Record(Slot("name", Text("world")))), Data.empty))
  }

  "should compile prefix attributed non-empty data" in {
    recon"""@hello %AA== """ should equal (Record(Attr("hello"), Data("AA==")))
    recon"""@hello() %AAA= """ should equal (Record(Attr("hello"), Data("AAA=")))
    recon"""@hello("world") %AAAA """ should equal (Record(Attr("hello", Text("world")), Data("AAAA")))
    recon"""@hello(name: "world") %ABCDabcd12+/ """ should equal (
      Record(Attr("hello", Record(Slot("name", Text("world")))), Data("ABCDabcd12+/")))
  }

  "should compile prefix attributed numbers" in {
    recon"""@hello 42""" should equal (Record(Attr("hello"), Number(42)))
    recon"""@hello() -42""" should equal (Record(Attr("hello"), Number(-42)))
    recon"""@hello("world") 42.0""" should equal (Record(Attr("hello", Text("world")), Number(42.0)))
    recon"""@hello(name: "world") -42.0""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world")))), Number(-42.0)))
  }

  "should compile prefix attributed booleans" in {
    recon"""@hello true""" should equal (Record(Attr("hello"), True))
    recon"""@hello() false""" should equal (Record(Attr("hello"), False))
    recon"""@hello("world") true""" should equal (Record(Attr("hello", Text("world")), True))
    recon"""@hello(name: "world") false""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world")))), False))
  }

  "should compile postfix attributed empty records" in {
    recon"""{} @signed""" should equal (Record(Attr("signed")))
    recon"""{} @signed()""" should equal (Record(Attr("signed")))
    recon"""{} @signed("me")""" should equal (Record(Attr("signed", Text("me"))))
    recon"""{} @signed(by: "me")""" should equal (
      Record(Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed non-empty records" in {
    recon"""{ {}, [] } @signed""" should equal (Record(Record.empty, Text.empty, Attr("signed")))
    recon"""{ "world", 42 } @signed()""" should equal (Record(Text("world"), Number(42), Attr("signed")))
    recon"""{ number: 42, true } @signed(by: "me")""" should equal (
      Record(Slot("number", Number(42)), True, Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed empty markup" in {
    recon"""[] @signed""" should equal (Record(Attr("signed")))
    recon"""[] @signed()""" should equal (Record(Attr("signed")))
    recon"""[] @signed("me")""" should equal (Record(Attr("signed", Text("me"))))
    recon"""[] @signed(by: "me")""" should equal (
      Record(Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed non-empty markup" in {
    recon"""[test] @signed""" should equal (Record(Text("test"), Attr("signed")))
    recon"""[test] @signed()""" should equal (Record(Text("test"), Attr("signed")))
    recon"""[test] @signed("me")""" should equal (Record(Text("test"), Attr("signed", Text("me"))))
    recon"""[test] @signed(by: "me")""" should equal (
      Record(Text("test"), Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed empty strings" in {
    recon""" "" @signed""" should equal (Record(Text.empty, Attr("signed")))
    recon""" "" @signed()""" should equal (Record(Text.empty, Attr("signed")))
    recon""" "" @signed("me")""" should equal (Record(Text.empty, Attr("signed", Text("me"))))
    recon""" "" @signed(by: "me")""" should equal (
      Record(Text.empty, Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed non-empty strings" in {
    recon""" "test" @signed""" should equal (Record(Text("test"), Attr("signed")))
    recon""" "test" @signed()""" should equal (Record(Text("test"), Attr("signed")))
    recon""" "test" @signed("me")""" should equal (Record(Text("test"), Attr("signed", Text("me"))))
    recon""" "test" @signed(by: "me")""" should equal (
      Record(Text("test"), Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed empty data" in {
    recon"""% @signed""" should equal (Record(Data.empty, Attr("signed")))
    recon"""% @signed()""" should equal (Record(Data.empty, Attr("signed")))
    recon"""% @signed("me")""" should equal (Record(Data.empty, Attr("signed", Text("me"))))
    recon"""% @signed(by: "me")""" should equal (
      Record(Data.empty, Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed non-empty data" in {
    recon"""%AA== @signed""" should equal (Record(Data("AA=="), Attr("signed")))
    recon"""%AAA= @signed()""" should equal (Record(Data("AAA="), Attr("signed")))
    recon"""%AAAA @signed("me")""" should equal (Record(Data("AAAA"), Attr("signed", Text("me"))))
    recon"""%ABCDabcd12+/ @signed(by: "me")""" should equal (
      Record(Data("ABCDabcd12+/"), Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed numbers" in {
    recon"""42 @signed""" should equal (Record(Number(42), Attr("signed")))
    recon"""-42 @signed()""" should equal (Record(Number(-42), Attr("signed")))
    recon"""42.0 @signed("me")""" should equal (Record(Number(42.0), Attr("signed", Text("me"))))
    recon"""-42.0 @signed(by: "me")""" should equal (
      Record(Number(-42.0), Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile postfix attributed booleans" in {
    recon"""true @signed""" should equal (Record(True, Attr("signed")))
    recon"""false @signed()""" should equal (Record(False, Attr("signed")))
    recon"""true @signed("me")""" should equal (Record(True, Attr("signed", Text("me"))))
    recon"""false @signed(by: "me")""" should equal (
      Record(False, Attr("signed", Record(Slot("by", Text("me"))))))
  }

  "should compile infix attributed empty records" in {
    recon"""{}@hello{}""" should equal (Record(Attr("hello")))
    recon"""{}@hello(){}""" should equal (Record(Attr("hello")))
    recon"""{}@hello("world"){}""" should equal (Record(Attr("hello", Text("world"))))
    recon"""{}@hello(name:"world"){}""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world"))))))
  }

  "should compile infix attributed non-empty records" in {
    recon"""{{}}@hello{[]}""" should equal (Record(Record.empty, Attr("hello"), Record.empty))
    recon"""{42}@hello(){"world"}""" should equal (Record(Number(42), Attr("hello"), Text("world")))
    recon"""{number:42}@hello(name:"world"){true}""" should equal (
      Record(Slot("number", Number(42)), Attr("hello", Record(Slot("name", Text("world")))), True))
  }

  "should compile infix attributed empty markup" in {
    recon"""[]@hello[]""" should equal (Record(Attr("hello")))
    recon"""[]@hello()[]""" should equal (Record(Attr("hello")))
    recon"""[]@hello("world")[]""" should equal (Record(Attr("hello", Text("world"))))
    recon"""[]@hello(name: "world")[]""" should equal (
      Record(Attr("hello", Record(Slot("name", Text("world"))))))
  }

  "should compile infix attributed non-empty markup" in {
    recon""" [a]@hello[test] """ should equal (Record(Text("a"), Attr("hello"), Text("test")))
    recon""" [a]@hello()[test] """ should equal (Record(Text("a"), Attr("hello"), Text("test")))
    recon""" [a]@hello("world")[test] """ should equal (
      Record(Text("a"), Attr("hello", Text("world")), Text("test")))
    recon""" [a]@hello(name:"world")[test] """ should equal (
      Record(Text("a"), Attr("hello", Record(Slot("name", Text("world")))), Text("test")))
  }

  "should compile infix attributed empty strings" in {
    recon""" ""@hello"" """ should equal (Record(Text.empty, Attr("hello"), Text.empty))
    recon""" ""@hello()"" """ should equal (Record(Text.empty, Attr("hello"), Text.empty))
    recon""" ""@hello("world")"" """ should equal (
      Record(Text.empty, Attr("hello", Text("world")), Text.empty))
    recon""" ""@hello(name:"world")"" """ should equal (
      Record(Text.empty, Attr("hello", Record(Slot("name", Text("world")))), Text.empty))
  }

  "should compile infix attributed non-empty strings" in {
    recon""" "a"@hello"test" """ should equal (Record(Text("a"), Attr("hello"), Text("test")))
    recon""" "a"@hello()"test" """ should equal (Record(Text("a"), Attr("hello"), Text("test")))
    recon""" "a"@hello("world")"test" """ should equal (
      Record(Text("a"), Attr("hello", Text("world")), Text("test")))
    recon""" "a"@hello(name:"world")"test" """ should equal (
      Record(Text("a"), Attr("hello", Record(Slot("name", Text("world")))), Text("test")))
  }

  "should compile infix attributed empty data" in {
    recon"""%@hello%""" should equal (Record(Data.empty, Attr("hello"), Data.empty))
    recon"""%@hello()%""" should equal (Record(Data.empty, Attr("hello"), Data.empty))
    recon"""%@hello("world")%""" should equal (Record(Data.empty, Attr("hello", Text("world")), Data.empty))
    recon"""%@hello(name:"world")%""" should equal (
      Record(Data.empty, Attr("hello", Record(Slot("name", Text("world")))), Data.empty))
  }

  "should compile infix attributed non-empty data" in {
    recon"""%AA==@hello%BB==""" should equal (Record(Data("AA=="), Attr("hello"), Data("BB==")))
    recon"""%AAA=@hello()%BBB=""" should equal (Record(Data("AAA="), Attr("hello"), Data("BBB=")))
    recon"""%AAAA@hello("world")%BBBB""" should equal (
      Record(Data("AAAA"), Attr("hello", Text("world")), Data("BBBB")))
    recon"""%ABCDabcd12+/@hello(name:"world")%/+21dcbaDCBA""" should equal (
      Record(Data("ABCDabcd12+/"), Attr("hello", Record(Slot("name", Text("world")))), Data("/+21dcbaDCBA")))
  }

  "should compile infix attributed numbers" in {
    recon"""2@hello 42""" should equal (Record(Number(2), Attr("hello"), Number(42)))
    recon"""-2@hello()-42""" should equal (Record(Number(-2), Attr("hello"), Number(-42)))
    recon"""2.0@hello("world")42.0""" should equal (
      Record(Number(2.0), Attr("hello", Text("world")), Number(42.0)))
    recon"""-2.0@hello(name:"world")-42.0""" should equal (
      Record(Number(-2.0), Attr("hello", Record(Slot("name", Text("world")))), Number(-42.0)))
  }

  "should compile infix attributed booleans" in {
    recon"""true@hello true""" should equal (Record(True, Attr("hello"), True))
    recon"""false@hello()false""" should equal (Record(False, Attr("hello"), False))
    recon"""true@hello("world")true""" should equal (Record(True, Attr("hello", Text("world")), True))
    recon"""false@hello(name:"world")false""" should equal (
      Record(False, Attr("hello", Record(Slot("name", Text("world")))), False))
  }

  "should compile plain markup" in {
    recon"[test]" should equal (Record(Text("test")))
  }

  "should compile plain markup with escapes" in {
    recon"""[\"\\\/\@\{\}\[\]\b\f\n\r\t]""" should equal (Record(Text("\"\\/@{}[]\b\f\n\r\t")))
  }

  "should compile markup with embedded markup" in {
    recon"""[Hello, [good] world!]""" should equal (Record(Text("Hello, "), Text("good"), Text(" world!")))
  }

  "should compile markup with embedded structure" in {
    recon"""[Hello{}world]""" should equal (Record(Text("Hello"), Text("world")))
    recon"""[A: {"answer"}.]""" should equal (Record(Text("A: "), Text("answer"), Text(".")))
    recon"""[A: {%AA==}.]""" should equal (Record(Text("A: "), Data("AA=="), Text(".")))
    recon"""[A: {42}.]""" should equal (Record(Text("A: "), Number(42), Text(".")))
    recon"""[A: {true}.]""" should equal (Record(Text("A: "), True, Text(".")))
    recon"""[A: {false}.]""" should equal (Record(Text("A: "), False, Text(".")))
    recon"""[A: {answer:0.0}.]""" should equal (Record(Text("A: "), Slot("answer", Number(0.0)), Text(".")))
  }

  "should compile markup with embedded single extant attributes" in {
    recon"""[A: @answer.]""" should equal (Record(Text("A: "), Record(Attr("answer")), Text(".")))
    recon"""[A: @answer().]""" should equal (Record(Text("A: "), Record(Attr("answer")), Text(".")))
    recon"""[A: @answer("secret").]""" should equal (
      Record(Text("A: "), Record(Attr("answer", Text("secret"))), Text(".")))
    recon"""[A: @answer(number: 42, true).]""" should equal (
      Record(Text("A: "), Record(Attr("answer", Record(Slot("number", Number(42)), True))), Text(".")))
  }

  "should compile markup with embedded sequential extant attributes" in {
    recon"""[A: @good @answer.]""" should equal (
      Record(Text("A: "), Record(Attr("good")), Text(" "), Record(Attr("answer")), Text(".")))
    recon"""[A: @good@answer.]""" should equal (
      Record(Text("A: "), Record(Attr("good")), Record(Attr("answer")), Text(".")))
    recon"""[A: @good() @answer().]""" should equal (
      Record(Text("A: "), Record(Attr("good")), Text(" "), Record(Attr("answer")), Text(".")))
    recon"""[A: @good()@answer().]""" should equal (
      Record(Text("A: "), Record(Attr("good")), Record(Attr("answer")), Text(".")))
  }

  "should compile markup with embedded attributed markup" in {
    recon"""[Hello, @em[world]!]""" should equal (
      Record(Text("Hello, "), Record(Attr("em"), Text("world")), Text("!")))
    recon"""[Hello, @em()[world]!]""" should equal (
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

  "should compile markup with embedded attributed values" in {
    recon"""[A: @answer{42}.]""" should equal (
      Record(Text("A: "), Record(Attr("answer"), Number(42)), Text(".")))
    recon"""[A: @answer(){42}.]""" should equal (
      Record(Text("A: "), Record(Attr("answer"), Number(42)), Text(".")))
    recon"""[A: @answer("secret"){42}.]""" should equal (
      Record(Text("A: "), Record(Attr("answer", Text("secret")), Number(42)), Text(".")))
    recon"""[A: @answer(number: 42, "secret"){true}.]""" should equal (
      Record(
        Text("A: "),
        Record(
          Attr("answer", Record(Slot("number", Number(42)), Text("secret"))),
          True),
        Text(".")))
  }


  "should interpolate top-level record variables" in {
    val x = recon"{}"
    recon"$x" should equal (x)
    val y = recon"{a:1,b:true}"
    recon"$y" should equal (y)
  }

  "should interpolate top-level markup variables" in {
    val x = recon"[]"
    recon"$x" should equal (x)
    val y = recon"[test]"
    recon"$y" should equal (y)
  }

  "should interpolate top-level string variables" in {
    val x = recon""" "" """
    recon"$x" should equal (x)
    val y = recon""" "test" """
    recon"$y" should equal (y)
  }

  "should interpolate top-level data variables" in {
    val x = recon"%"
    recon"$x" should equal (x)
    val y = recon"%AA=="
    recon"$y" should equal (y)
  }

  "should interpolate top-level number variables" in {
    val x = recon"42"
    recon"$x" should equal (x)
    val y = recon"3.14"
    recon"$y" should equal (y)
  }

  "should interpolate top-level boolean variables" in {
    val x = recon"true"
    recon"$x" should equal (x)
    val y = recon"false"
    recon"$y" should equal (y)
  }

  "should interpolate multiple heterogeneous variables" in {
    val a = Record.empty
    val b = Record(True)
    val c = Text.empty
    val d = Data.empty
    val e = Number(42)
    val f = True
    val g = False
    recon" $a;$b;$c;$d;$e;$f;$g " should equal (Record(a, b, c, d, e, f, g))
    recon" $a,$b,$c,$d,$e,$f,$g " should equal (Record(a, b, c, d, e, f, g))
    recon"{$a;$b;$c;$d;$e;$f;$g}" should equal (Record(a, b, c, d, e, f, g))
    recon"{$a,$b,$c,$d,$e,$f,$g}" should equal (Record(a, b, c, d, e, f, g))
  }

  "should interpolate prefix attributed variables" in {
    val world = Text("world")
    recon"@hello $world" should equal (Record(Attr("hello"), world))
    recon"@hello() $world" should equal (Record(Attr("hello"), world))
    recon"@hello @good $world" should equal (Record(Attr("hello"), Attr("good"), world))
    recon"""@span(class:"subject") $world""" should equal (
      Record(Attr("span", Record(Slot("class", Text("subject")))), world))
  }

  "should interpolate postfix attributed variables" in {
    val x = Number(42)
    recon"$x @meters" should equal (Record(x, Attr("meters")))
    recon"$x@meters" should equal (Record(x, Attr("meters")))
    recon"$x @meters()" should equal (Record(x, Attr("meters")))
    recon"$x@meters()" should equal (Record(x, Attr("meters")))
    recon"$x @meters @absolute" should equal (Record(x, Attr("meters"), Attr("absolute")))
    recon"$x@meters@absolute" should equal (Record(x, Attr("meters"), Attr("absolute")))
    recon"$x @meters(uncertainty: 0.5)" should equal (
      Record(x, Attr("meters", Record(Slot("uncertainty", Number(0.5))))))
  }

  "should interpolate infix attributed variables" in {
    val x = Number(42)
    recon"@length $x @meters" should equal (Record(Attr("length"), x, Attr("meters")))
    recon"@length() $x @meters()" should equal (Record(Attr("length"), x, Attr("meters")))
    recon"@length $x @meters @absolute" should equal (
      Record(Attr("length"), x, Attr("meters"), Attr("absolute")))
    recon"@absolute @length $x @meters" should equal (
      Record(Attr("absolute"), Attr("length"), x, Attr("meters")))
    recon"@length $x @meters(uncertainty: 0.5)" should equal (
      Record(Attr("length"), x, Attr("meters", Record(Slot("uncertainty", Number(0.5))))))
  }

  "should interpolate variables in slot values" in {
    val xs = recon"""{ 1, 2, 3 }"""
    recon"  xs: $xs  " should equal (Record(Slot("xs", xs)))
    recon"{ xs: $xs }" should equal (Record(Slot("xs", xs)))
    val ys = recon"""{ "a", "b", "c" }"""
    recon"  xs: $xs, ys: $ys  " should equal (Record(Slot("xs", xs), Slot("ys", ys)))
    recon"{ xs: $xs, ys: $ys }" should equal (Record(Slot("xs", xs), Slot("ys", ys)))
  }

  "should interpolate variables in attribute parameters" in {
    val x = Text("world")
    recon"@span($x)" should equal (Record(Attr("span", x)))
    recon"@span(class:$x)" should equal (Record(Attr("span", Record(Slot("class", x)))))
    recon"@span(true,$x)" should equal (Record(Attr("span", Record(True, x))))
    recon"@span($x,false)" should equal (Record(Attr("span", Record(x, False))))
    recon"@span(true,$x,false)" should equal (Record(Attr("span", Record(True, x, False))))
    recon"@span(true,class:$x)" should equal (Record(Attr("span", Record(True, Slot("class", x)))))
    recon"@span(class:$x,false)" should equal (Record(Attr("span", Record(Slot("class", x), False))))
    recon"@span(true,class:$x,false)" should equal (
      Record(Attr("span", Record(True, Slot("class", x), False))))
  }

  "should interpolate variables in postfix attribute parameters" in {
    val x = Text("world")
    recon"{} @cite($x)" should equal (Record(Attr("cite", x)))
    recon"{} @cite(class:$x)" should equal (Record(Attr("cite", Record(Slot("class", x)))))
    recon"{} @cite(true,$x)" should equal (Record(Attr("cite", Record(True, x))))
    recon"{} @cite($x,false)" should equal (Record(Attr("cite", Record(x, False))))
    recon"{} @cite(true,$x,false)" should equal (Record(Attr("cite", Record(True, x, False))))
    recon"{} @cite(true,class:$x)" should equal (Record(Attr("cite", Record(True, Slot("class", x)))))
    recon"{} @cite(class:$x,false)" should equal (Record(Attr("cite", Record(Slot("class", x), False))))
    recon"{} @cite(true,class:$x,false)" should equal (
      Record(Attr("cite", Record(True, Slot("class", x), False))))
  }

  "should interpolate variables in markup" in {
    val x = Text("test")
    recon"[$x]" should equal (Record(x))
    val y = Number(42)
    recon"[$y]" should equal (Record(y))
    recon"[A: $y.]" should equal (Record(Text("A: "), y, Text(".")))
  }

  "should interpolate variables in nested markup" in {
    val x = Text("test")
    recon"[[$x]]" should equal (Record(x))
    val y = Number(42)
    recon"[[$y]]" should equal (Record(y))
    recon"[A: [$y].]" should equal (Record(Text("A: "), y, Text(".")))
    recon"[A: [$y.]]" should equal (Record(Text("A: "), y, Text(".")))
  }

  "should interpolate variables in embedded markup structure" in {
    val x = Text("test")
    recon"[{$x}]" should equal (Record(x))
    val y = Number(42)
    recon"[{$y}]" should equal (Record(y))
    recon"[A: {$y}.]" should equal (Record(Text("A: "), y, Text(".")))
  }

  "should interpolate variables in embedded markup slot values" in {
    val x = Text("foo")
    recon"[{answer:$x}]" should equal (Record(Slot("answer", x)))
    val y = Number(42)
    recon"[{answer:$y}]" should equal (Record(Slot("answer", y)))
    recon"[A: {answer:$y}.]" should equal (Record(Text("A: "), Slot("answer", y), Text(".")))
  }

  "should interpolate variables in markup-embedded attribute parameters" in {
    val x = Text("subject")
    recon"[Hello, @span($x)[world]!]" should equal (
      Record(Text("Hello, "), Record(Attr("span", x), Text("world")), Text("!")))
    recon"[Hello, @span(class:$x)[world]!]" should equal (
      Record(Text("Hello, "), Record(Attr("span", Record(Slot("class", x))), Text("world")), Text("!")))
  }
}

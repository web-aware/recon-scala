package recon

import org.scalatest._
import org.scalatest.matchers._

class ReconSpec extends FreeSpec with Matchers {
  "Item ordering" - {
    "Attrs should order by key then by value" in {
      Attr("a")    should order <  Attr("b")
      Attr("b")    should order >  Attr("a")
      Attr("a", 0) should order <  Attr("a", 1)
      Attr("a", 1) should order >  Attr("a", 0)
      Attr("a")    should order == Attr("a")
      Attr("a", 0) should order == Attr("a", 0)
    }

    "Slots should order by key then by value" in {
      Slot("a")    should order <  Slot("b")
      Slot("b")    should order >  Slot("a")
      Slot("a", 0) should order <  Slot("a", 1)
      Slot("a", 1) should order >  Slot("a", 0)
      Slot("a")    should order == Slot("a")
      Slot("a", 0) should order == Slot("a", 0)
    }

    "Records should order by sequential item order" in {
      Record.empty   should order <  Record(1)
      Record(1)      should order >  Record()
      Record(1)      should order <  Record(1, "a")
      Record(1, "a") should order >  Record(1)
      Record(1, "a") should order <  Record(1, "b")
      Record(1, "b") should order >  Record(1, "a")
      Record(0, "a") should order <  Record(1)
      Record(1)      should order >  Record(0, "a")
      Record.empty   should order == Record.empty
      Record(1)      should order == Record(1)
      Record(1, "a") should order == Record(1, "a")
    }

    "Text should order by sequential character order" in {
      Text.empty should order <  Text("a")
      Text("a")  should order >  Text.empty
      Text("a")  should order <  Text("aa")
      Text("aa") should order >  Text("a")
      Text("aa") should order <  Text("ab")
      Text("ab") should order >  Text("aa")
      Text("ab") should order <  Text("b")
      Text("b")  should order >  Text("ab")
      Text.empty should order == Text.empty
      Text("a")  should order == Text("a")
      Text("ab") should order == Text("ab")
    }

    "Data should order by sequential byte order" in {
      Data.empty   should order <  Data("AA==")
      Data("AA==") should order >  Data.empty
      Data("AA==") should order <  Data("AAA=")
      Data("AAA=") should order >  Data("AA==")
      Data("AAA=") should order <  Data("AAE=")
      Data("AAE=") should order >  Data("AAA=")
      Data("AAE=") should order <  Data("AQ==")
      Data("AQ==") should order >  Data("AAE=")
      Data.empty   should order == Data.empty
      Data("AA==") should order == Data("AA==")
      Data("AAE=") should order == Data("AAE=")
    }

    "Numbers should order numerically" in {
      Number(0)   should order <  Number(1)
      Number(1)   should order >  Number(0)
      Number(0.5) should order <  Number(1.0)
      Number(1.0) should order >  Number(0.5)
      Number(-1)  should order <  Number(1)
      Number(1)   should order >  Number(-1)
      Number(0)   should order == Number(0)
      Number(1)   should order == Number(1)
      Number(-1)  should order == Number(-1)
      Number(0.5) should order == Number(0.5)
    }

    "Extant should order the same as itself" in {
      Extant should order == Extant
    }

    "Absent should order the same as itself" in {
      Absent should order == Absent
    }

    "Attrs should order before Slots, Records, Data, Text, Numbers, Extant, and Absent" in {
      Attr("a", 1) should order < Slot("a", 1)
      Attr("a", 1) should order < Record.empty
      Attr("a", 1) should order < Data.empty
      Attr("a", 1) should order < Text.empty
      Attr("a", 1) should order < Number(0)
      Attr("a", 1) should order < Extant
      Attr("a", 1) should order < Absent
    }

    "Slots should order after Attrs and before Records, Data, Text, Numbers, Extant, and Absent" in {
      Slot("a", 1) should order > Attr("a", 1)
      Slot("a", 1) should order < Record.empty
      Slot("a", 1) should order < Data.empty
      Slot("a", 1) should order < Text.empty
      Slot("a", 1) should order < Number(0)
      Slot("a", 1) should order < Extant
      Slot("a", 1) should order < Absent
    }

    "Records should order after Attrs and Slots, and before Data, Text, Numbers, Extant, and Absent" in {
      Record.empty should order > Attr("a", 1)
      Record.empty should order > Slot("a", 1)
      Record.empty should order < Data.empty
      Record.empty should order < Text.empty
      Record.empty should order < Number(0)
      Record.empty should order < Extant
      Record.empty should order < Absent
    }

    "Data should order after Attrs, Slots, and Records, and before Text, Numbers, Extant, and Absent" in {
      Data.empty should order > Attr("a", 1)
      Data.empty should order > Slot("a", 1)
      Data.empty should order > Record.empty
      Data.empty should order < Text.empty
      Data.empty should order < Number(0)
      Data.empty should order < Extant
      Data.empty should order < Absent
    }

    "Text should order after Attrs, Slots, Records, and Data, and before Numbers, Extant, and Absent" in {
      Text.empty should order > Attr("a", 1)
      Text.empty should order > Slot("a", 1)
      Text.empty should order > Record.empty
      Text.empty should order > Data.empty
      Text.empty should order < Number(0)
      Text.empty should order < Extant
      Text.empty should order < Absent
    }

    "Numbers should order after Attrs, Slots, Records, Data, and Text, and before Extant and Absent" in {
      Number(0) should order > Attr("a", 1)
      Number(0) should order > Slot("a", 1)
      Number(0) should order > Record.empty
      Number(0) should order > Data.empty
      Number(0) should order > Text.empty
      Number(0) should order < Extant
      Number(0) should order < Absent
    }

    "Extant should order after Attrs, Slots, Records, Data, Text, and Numbers, and before Absent" in {
      Extant should order > Attr("a", 1)
      Extant should order > Slot("a", 1)
      Extant should order > Record.empty
      Extant should order > Data.empty
      Extant should order > Text.empty
      Extant should order > Number(0)
      Extant should order < Absent
    }

    "Absent should order after Attrs, Slots, Records, Data, Text, Numbers, and Extant" in {
      Absent should order > Attr("a", 1)
      Absent should order > Slot("a", 1)
      Absent should order > Record.empty
      Absent should order > Data.empty
      Absent should order > Text.empty
      Absent should order > Number(0)
      Absent should order > Extant
    }
  }


  val order = new OrderWord()

  final class OrderWord {
    def < (right: Item) = new Matcher[Item] {
      def apply(left: Item) = MatchResult(
        left.compareTo(right) < 0,
        s"$left did not order before $right",
        s"$left ordered before $right"
      )
    }

    def > (right: Item) = new Matcher[Item] {
      def apply(left: Item) = MatchResult(
        left.compareTo(right) > 0,
        s"$left did not order after $right",
        s"$left ordered after $right"
      )
    }

    def == (right: Item) = new Matcher[Item] {
      def apply(left: Item) = MatchResult(
        left.compareTo(right) == 0,
        s"$left did not order the same as $right",
        s"$left ordered the same as $right"
      )
    }
  }
}

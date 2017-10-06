import org.scalatest.{FlatSpec, _}

class FieldBuilderSpec extends FlatSpec with Matchers {
  "The field builder " should "Give a 'Invalid input' error when fed an empty array" in {
      FieldBuilder.buildField(1,1, Seq()) shouldBe Left("Invalid input")
  }

  "The field builder " should "Give a 'Invalid input' error when fed a line count of 0" in {
    FieldBuilder.buildField(0,1, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder " should "Give a 'Invalid input' error when fed a line count of -1" in {
    FieldBuilder.buildField(-1,1, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder " should "Give a 'Invalid input' error when fed a column count of 0" in {
    FieldBuilder.buildField(1,0, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder " should "Give a 'Invalid input' error when fed a column count of -1" in {
    FieldBuilder.buildField(1,-1, Seq("*")) shouldBe Left("Invalid input")
  }
}

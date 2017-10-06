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

  "The field builder" should "Return a field with one mine when passed a field with one mine" in {
    FieldBuilder.buildField(1, 1, Seq("*")) shouldBe Right(Seq("*"))
  }

  "The field builder" should "Give an 'Invalid input' error when passed a field with fewer lines than the line count" in {
    FieldBuilder.buildField(2, 1, Seq("*")) shouldBe Left("Invalid input")
  }
  "The field builder" should "Give an 'Invalid input' error when passed a field with more lines than the line count" in {
    FieldBuilder.buildField(1, 1, Seq("*","*")) shouldBe Left("Invalid input")
  }

}

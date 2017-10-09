import org.scalatest.{FlatSpec, _}

class FieldBuilderSpec extends FlatSpec with Matchers {
  val myFieldBuilder: FieldBuilder = ForLoopBasedFieldBuilder
  "The field builder " should "Give a 'Invalid input' error when fed an empty array" in {
      myFieldBuilder.buildField(1,1, Seq()) shouldBe Left("Invalid input")
  }

  "The field builder " should "Give a 'Invalid input' error when fed a line count of 0" in {
    myFieldBuilder.buildField(0,1, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder " should "Give a 'Invalid input' error when fed a line count of -1" in {
    myFieldBuilder.buildField(-1,1, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder " should "Give a 'Invalid input' error when fed a column count of 0" in {
    myFieldBuilder.buildField(1,0, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder " should "Give a 'Invalid input' error when fed a column count of -1" in {
    myFieldBuilder.buildField(1,-1, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder" should "Return a field with one mine when passed a field with one mine" in {
    myFieldBuilder.buildField(1, 1, Seq("*")) shouldBe Right(Seq("*"))
  }

  "The field builder" should "Give an 'Invalid input' error when passed a field with fewer lines than the line count" in {
    myFieldBuilder.buildField(2, 1, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder" should "Give an 'Invalid input' error when passed a field with more lines than the line count" in {
    myFieldBuilder.buildField(1, 1, Seq("*","*")) shouldBe Left("Invalid input")
  }

  "The field builder" should "Give an 'Invalid input' error when passed a field with fewer columns than the column count" in {
    myFieldBuilder.buildField(1, 2, Seq("*")) shouldBe Left("Invalid input")
  }

  "The field builder" should "Return a field with a 0 when passed a filed with no mines" in  {
    myFieldBuilder.buildField(1, 1, Seq(".")) shouldBe Right(Seq("0"))
  }

  "The field builder" should "Return a field with a '*1' when passed '*.'" in  {
    myFieldBuilder.buildField(1, 2, Seq("*.")) shouldBe Right(Seq("*1"))
  }

  "The field builder" should "Return a field with a '1*' when passed '.*'" in  {
    myFieldBuilder.buildField(1, 2, Seq(".*")) shouldBe Right(Seq("1*"))
  }

}

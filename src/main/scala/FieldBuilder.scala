object FieldBuilder{
  def buildField(lineCount:Integer, columnCount: Integer, lines:Seq[String]):Either[String,Seq[String]] = {
    Left("Invalid input")
  }
}
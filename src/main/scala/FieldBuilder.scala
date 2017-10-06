object FieldBuilder{
  def buildField(lineCount:Integer, columnCount: Integer, lines:Seq[String]):Either[String,Seq[String]] = {
    if(lineCount < 1 || columnCount < 1 || lines.isEmpty || lines.size != lineCount){
      Left("Invalid input")
    } else {
      Right(lines)
    }

  }
}
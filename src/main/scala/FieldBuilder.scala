trait FieldBuilder{
  def buildField(lineCount:Integer, columnCount: Integer, lines:Seq[String]):Either[String,Seq[String]]
}

object FieldBuilder extends  FieldBuilder{
  def buildField(lineCount:Integer, columnCount: Integer, lines:Seq[String]):Either[String,Seq[String]] = {
    if(!isInputValid(lineCount, columnCount, lines)){
      Left("Invalid input")
    } else {
      val leftNeighbours: Seq[String] = lines.map( line => line.foldLeft("")(
        (acc,point) => if(point == '*'){acc + point} else if(acc.endsWith("*")){acc + '1'} else {acc +'0'}))
      Right(leftNeighbours)
      //Right(lines.map(line => line.map(point=> if (point == '*'){'*'} else {'0'})))
    }
  }

  private def isInputValid(lineCount:Integer, columnCount: Integer, lines:Seq[String]) = {
    lineCount > 0 && columnCount > 0 && lines.nonEmpty && lines.size == lineCount && lines.forall(line => line.length == columnCount)
  }
}

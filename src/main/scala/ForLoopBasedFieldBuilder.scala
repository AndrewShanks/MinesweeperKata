
object ForLoopBasedFieldBuilder extends  FieldBuilder{
  def buildField(lineCount:Integer, columnCount: Integer, lines:Seq[String]):Either[String,Seq[String]] = {
    if(!isInputValid(lineCount, columnCount, lines)){
      Left("Invalid input")
    } else {
      val columnIndexes = 0 to columnCount-1
      Right(Seq(columnIndexes.foldLeft("")((acc, ind)=> acc + getPointValue(ind, 0, lines))))
    }
  }

  def getPointValue(x:Int, y:Int, lines:Seq[String]):String = {
    if(lines(y)(x) == '*'){
      "*"
    } else {
      (minesOnPoint(x-1,y, lines) + minesOnPoint(x+1,y, lines)).toString
    }
  }

  def minesOnPoint(x:Int, y:Int, lines:Seq[String]): Int = {
    if ( x < 0 || x >= lines(y).length ){
      0
    } else {
      if ( lines(y)(x) == '*'){
        1
      }else {
        0
      }
    }
  }

  private def isInputValid(lineCount:Integer, columnCount: Integer, lines:Seq[String]) = {
    lineCount > 0 && columnCount > 0 && lines.nonEmpty && lines.size == lineCount && lines.forall(line => line.length == columnCount)
  }
}
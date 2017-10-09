
object ForLoopBasedFieldBuilder extends  FieldBuilder{
  def buildField(lineCount:Integer, columnCount: Integer, lines:Seq[String]):Either[String,Seq[String]] = {

    def getPointValue(x:Int, y:Int):String = {
      if(lines(y)(x) == '*'){
        "*"
      } else {
        (minesOnPoint(x-1,y-1) + minesOnPoint(x,y-1) + minesOnPoint(x+1, y-1) +
          minesOnPoint(x-1,y) + minesOnPoint(x+1,y) +
          minesOnPoint(x-1, y+1) + minesOnPoint(x, y+1) + minesOnPoint(x+1, y+1)).toString
      }
    }

    def minesOnPoint(x:Int, y:Int): Int = {
      if ( y< 0 || y >= lines.length || x < 0 || x >= lines(y).length ){
        0
      } else if ( lines(y)(x) == '*'){
        1
      } else {
        0
      }
    }

    if(!isInputValid(lineCount, columnCount, lines)){
      Left("Invalid input")
    } else {
      val rowIndices = 0 until lineCount
      val columnIndices = 0 to columnCount-1
      Right(
        rowIndices.foldLeft(Seq():Seq[String])((row_acc, row_ind) => row_acc :+ columnIndices.foldLeft("")((acc, col_ind)=> acc + getPointValue(col_ind, row_ind)))
      )
    }
  }


  private def isInputValid(lineCount:Integer, columnCount: Integer, lines:Seq[String]) = {
    lineCount > 0 && columnCount > 0 && lines.nonEmpty && lines.size == lineCount && lines.forall(line => line.length == columnCount)
  }
}

object ForLoopBasedFieldBuilder extends  FieldBuilder{
  def buildField(lineCount:Integer, columnCount: Integer, lines:Seq[String]):Either[String,Seq[String]] = {

    val MINE = -1

    def calculatePointValue(x:Int, y:Int):Int = {
      if(lines(y)(x) == '*'){
        MINE
      } else {
        minesOnPoint(x-1,y-1) + minesOnPoint(x,y-1) + minesOnPoint(x+1, y-1) +
          minesOnPoint(x-1,y) + minesOnPoint(x+1,y) +
          minesOnPoint(x-1, y+1) + minesOnPoint(x, y+1) + minesOnPoint(x+1, y+1)
      }
    }

    def determineRepresentation(pointValue: Int):String = {
      if(pointValue == MINE) {
        "*"
      } else {
        pointValue.toString
      }
    }

    def minesOnPoint(x:Int, y:Int): Int = {
      if ( y < 0 || y >= lines.length || x < 0 || x >= lines(y).length ){
        0
      } else if ( lines(y)(x) == '*'){
        1
      } else {
        0
      }
    }

    def buildRowString(row:Seq[Int]) : String = {
      row.foldLeft("")((col_acc, col)=> col_acc + determineRepresentation(col))
    }

    if(!isInputValid(lineCount, columnCount, lines)){
      Left("Invalid input")
    } else {
      val rowIndices = 0 until lineCount
      val columnIndices = 0 until columnCount

      val numericField: Seq[Seq[Int]] = rowIndices.map( row_ind =>
        columnIndices.map( col_ind => calculatePointValue(col_ind, row_ind))
      )

      Right(
        numericField.foldLeft(Seq():Seq[String])((row_acc, row) => row_acc :+ buildRowString(row))
      )
    }
  }

  private def isInputValid(lineCount:Integer, columnCount: Integer, lines:Seq[String]) = {
    lineCount > 0 && columnCount > 0 && lines.nonEmpty && lines.size == lineCount && lines.forall(line => line.length == columnCount)
  }
}
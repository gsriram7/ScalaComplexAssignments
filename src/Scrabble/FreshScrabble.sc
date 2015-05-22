val grid: Seq[Seq[String]] = Array(  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("TW","","","DL","","","","DW","","","","DL","","","TW").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq)
val score: Seq[Int] = Array(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10)

case class Position(x:Int, y:Int){
  def moveDown()=Position(x, y+1)
  def moveRight()=Position(x-1,y)
}

def scrabble(result: Int, point: Position, input: List[Char], dw: Int, tw: Int):Int ={
  input match {
    case Nil => result * tw * dw
    case x :: y if grid(point.x)(point.y).equals("DW") => scrabble(result + score(x - 97), point.moveRight(), y, dw*2, tw)
    case x :: y if grid(point.x)(point.y).equals("TW") => scrabble(result + score(x - 97), point.moveRight(), y, dw, tw*3)
    case x :: y if grid(point.x)(point.y).equals("DL") => scrabble(result + score(x - 97) * 2, point.moveRight(), y, dw, tw)
    case x :: y if grid(point.x)(point.y).equals("TL") => scrabble(result + score(x - 97) * 3, point.moveRight(), y, dw, tw)
    case x :: y => scrabble(result + score(x - 97), point.moveRight(), y, dw, tw)
  }
}

scrabble(0, Position(14, 14), "cryptozoologies".toCharArray.toList, 1, 1)


val grid: Seq[Seq[String]] = Array(  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("TW","","","DL","","","","DW","","","","DL","","","TW").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq)
val score: Seq[Int] = Array(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10)

case class Position(x:Int, y:Int){
  def shiftPosition(dir:Char) = {
    val newPosition = Map('R' -> Position(x, y + 1), 'D' -> Position(x - 1, y))
    newPosition(dir)
  }
}

val compute = Map[String, (Int) => Int]("TL" -> ((x:Int) => x*3), "DL" -> ((x:Int) => x*2), "" -> ((x:Int) => x))

def scrabble(result: Int, point: Position, input: List[Char], dw: Int, tw: Int, dir:Char):Int ={
  input match {
    case Nil => result * tw * dw
    case x :: y if grid(point.x)(point.y).equals("DW") => scrabble(result + score(x - 97), point.shiftPosition(dir), y, dw*2, tw, dir)
    case x :: y if grid(point.x)(point.y).equals("TW") => scrabble(result + score(x - 97), point.shiftPosition(dir), y, dw, tw*3, dir)
    case x :: y => scrabble(result + compute(grid(point.x)(point.y))(score(x - 97)), point.shiftPosition(dir), y, dw, tw, dir)
  }
}

scrabble(0, Position(8, 7), "bootcamp".toCharArray.toList, 1, 1, 'D') == 34
scrabble(0, Position(14, 14), "cryptozoologies".toCharArray.toList, 1, 1, 'D') == 999
scrabble(0, Position(0,0), "indix".toCharArray.toList, 1, 1, 'R') == 42
scrabble(0, Position(0,0), "unconsciousness".toCharArray.toList, 1, 1, 'R') == 567


val grid: Seq[Seq[String]] = Array(  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("TW","","","DL","","","","DW","","","","DL","","","TW").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq)
case class Position(x:Int, y:Int){
  def shiftPosition(dir:Char) = {
    val newPosition = Map('R' -> Position(x, y + 1), 'D' -> Position(x - 1, y))
    newPosition(dir)
  }
}

case class Word(dw:Int, tw:Int, totalScore:Int) {
  val score: Seq[Int] = Array(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10)
  def tripleWord(x:Char) = Word(dw, tw*3, totalScore + score(x - 97))
  def doubleWord(x:Char) = Word(dw*2, tw, totalScore + score(x - 97))
  def computeScore(respectiveGrid: String, x:Char) ={
    val compute = Map[String, (Int) => Int]("TL" -> ((x:Int) => x*3), "DL" -> ((x:Int) => x*2), "" -> ((x:Int) => x))
    Word(dw, tw, totalScore + compute(respectiveGrid)(score(x - 97)))
  }
  def resultantScore() = totalScore * dw * tw
}

def scrabble(word: Word, point: Position, input: List[Char], dir:Char):Int ={
  input match {
    case Nil => word.resultantScore
    case x :: y if grid(point.x)(point.y).equals("DW") => scrabble(word.doubleWord(x), point.shiftPosition(dir), y, dir)
    case x :: y if grid(point.x)(point.y).equals("TW") => scrabble(word.tripleWord(x), point.shiftPosition(dir), y, dir)
    case x :: y => scrabble(word.computeScore(grid(point.x)(point.y), x), point.shiftPosition(dir), y, dir)
  }
}

val word: Word = Word(1, 1, 0)
scrabble(word, Position(8, 7), "bootcamp".toCharArray.toList, 'D') == 34
scrabble(word, Position(14, 14), "cryptozoologies".toCharArray.toList, 'D') == 999
scrabble(word, Position(0,0), "indix".toCharArray.toList, 'R') == 42
scrabble(word, Position(0,0), "unconsciousness".toCharArray.toList, 'R') == 567

case class Position(x:Int, y:Int, direction:Char)

def moveRight(point:Position):Position = Position(point.x, point.y+1, 'R')
def moveDown(point:Position):Position = Position(point.x+1, point.y, 'D')

val compute = Map("TW"->{(_: Int) * 3}, "DW"->{(_: Int) * 2})

val alphabetScores: Seq[Int] = Array(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10)

val grid: Seq[Seq[String]] = Array(
  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq,
  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,
  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,
  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,
  Array("","","","","DW","","","","","","DW","","","","").toSeq,
  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,
  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,
  Array("TW","","","DL","","","","DW","","","","DL","","","TW").toSeq,
  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,
  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,
  Array("","","","","DW","","","","","","DW","","","","").toSeq,
  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,
  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,
  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,
  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq
)

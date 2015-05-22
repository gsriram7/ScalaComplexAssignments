case class Position(x:Int, y:Int)

def moveRight(point:Position):Position = Position(point.x+1, point.y)
def moveDown(point:Position):Position = Position(point.x, point.y-1)

def mulBy3(x:Int):Int = x*3
def mulBy2(x:Int):Int = x*2
def mulBy1(x:Int):Int = x*1

val grid: Seq[Seq[String]] = Array(  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("TW","","","DL","","","","DW","","","","DL","","","TW").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq)
val move = Map('R' -> moveRight _, 'D' -> moveDown _)
val compute: Map[String, (Int)=>Int] = Map("TW" -> mulBy3 _,"DW" -> mulBy2 _,"TL" -> mulBy3 _, "DL" -> mulBy2 _, "" ->  mulBy1 _)

val alphabetScores: Seq[Int] = Array(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10)

def scrabble(accumulator:Int, point:Position, sequence:List[Char], special:Map[String, Int], dir:Char):Int ={
  sequence match {
    case Nil => accumulator * special("TW") * special("DW")
    case head :: tail if grid(point.y)(point.x).equals("TW") => scrabble(accumulator + alphabetScores(head - 97), move(dir)(point), tail, special.updated(grid(point.y)(point.x), special(grid(point.y)(point.x))*3), dir)
    case head :: tail if grid(point.y)(point.x).equals("DW") => scrabble(accumulator + alphabetScores(head - 97), move(dir)(point), tail, special.updated(grid(point.y)(point.x), special(grid(point.y)(point.x))*2), dir)
    case head :: tail => scrabble(accumulator + compute(grid(point.y)(point.x))(alphabetScores(head - 97)), move(dir)(point), tail, special, dir)

  }
}
val init: Map[String, Int] = Map("TW" -> 1, "DW" -> 1)
scrabble(0, Position(0,0), "aaa".toCharArray.toList, init, 'R')
scrabble(0, Position(6,7), "bootcamp".toCharArray.toList, init, 'D')


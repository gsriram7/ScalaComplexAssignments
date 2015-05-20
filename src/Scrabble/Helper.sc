val compute = Map("TW"->{(_: Int) * 3}, "DW"->{(_: Int) * 2})

val alphabetScores: Seq[Int] = Array(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10)

val grid: Seq[Seq[String]] = Array(  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("TW","","","DL","","","","DW","","","","DL","","","TW").toSeq,  Array("","","DL","","","","DL","","DL","","","","DL","","").toSeq,  Array("","TL","","","","TL","","","","TL","","","","TL","").toSeq,  Array("","","","","DW","","","","","","DW","","","","").toSeq,  Array("DL","","","DW","","","","DL","","","","DW","","","DL").toSeq,  Array("","","DW","","","","DL","","DL","","","","DW","","").toSeq,  Array("","DW","","","","TL","","","","TL","","","","DW","").toSeq,  Array("TW","","","DL","","","","TW","","","","DL","","","TW").toSeq)

println(grid)
def incr(accumulator:Int, spl2:Map[String, Int], seq:List[String]):Int ={
  seq match {
    case Nil => accumulator * spl2("TW") * spl2("DW")
    case head :: tail if 96<head.charAt(0) && head.charAt(0)<123 => incr(accumulator+alphabetScores(head.charAt(0)-97), spl2, tail)
    case head :: tail => incr(accumulator, spl2.updated(head, compute(head)(spl2(head))), tail)
  }
}

incr(0,Map("TW" -> 1, "DW" -> 1), List("b","o","o","TW","TW","DW","DW"))



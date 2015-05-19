def blockedChannelsWithinRange(start:Int, end:Int, blocked:List[Int]):Int = blocked.count(x =>start < x && x < end)

def size(accumulator:Int, number:Int):Int =  if (number < 10)accumulator else size(accumulator+1, number/10)

def minSize(start: Int, end: Int, prev: Int, curr: Int, blocked: List[Int]):Int ={
  val sizeOfNumber = size(1, curr)
  val backward = Math.abs(Math.abs(prev - curr) - blockedChannelsWithinRange(prev, curr, blocked))
  val forward = Math.abs(curr - end) + prev - blockedChannelsWithinRange(curr, end, blocked) -blockedChannelsWithinRange(start, prev, blocked)
  println("curr is "+curr+"\tsize "+sizeOfNumber+"\tback "+backward+"\tfor "+forward)
  Math.min(sizeOfNumber, Math.min(forward, backward))
}

def minimumClicks(start: Int, end: Int, blocked: List[Int], sequence: List[Int]):Int ={
  var back = start-1
  var prev = start
  var minCount = 0
  sequence.foreach(curr=>{
    if (back == curr) {
      println("back to "+curr)
      minCount += 1
    }
    else
      minCount+=minSize(start, end, prev, curr, blocked)
    back = prev
    prev = curr
  })
  minCount
}

minimumClicks(1, 20, List(18, 19), List(15, 14, 17, 1, 17))
//minimumClicks(103, 108, List(104), List(105, 106, 107, 103, 105))
//minimumClicks(1, 200, List(), List(1, 100, 1, 101))



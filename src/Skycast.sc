def blockedChannelsWithinRange(start:Int, end:Int, blocked:List[Int]):Int = blocked.count(x =>start < x && x < end)

def size(accumulator:Int, number:Int):Int =
  if (number < 10)
    accumulator
  else
    size(accumulator+1, number/10)


def minimumClicks(start: Int, end: Int, blocked: List[Int], sequence: List[Int]):Int ={
  var back = start-1
  var prev = start
  var minCount = 0
  sequence.foreach(curr=>{
    if (back == curr)
      minCount+=1
    else if (size(1, curr) < Math.abs(Math.abs(prev - curr) - blockedChannelsWithinRange(prev, curr, blocked))) {
      minCount+=2
    }
    else
      minCount+=Math.abs(prev - curr - blockedChannelsWithinRange(prev, curr, blocked))
    back = prev
    prev = curr
  })
  minCount
}

minimumClicks(1, 20, List(18, 19), List(15, 14, 17, 1, 17))


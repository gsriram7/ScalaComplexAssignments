def size(accumulator:Int, number:Int):Int =  if (number < 10)accumulator else size(accumulator+1, number/10)

def blockedChannelsWithinRange(from: Int, to: Int, blocked: List[Int]) = blocked.count(x=> from<x && x<to)

def backward(prev:Int, curr:Int, blocked:List[Int]):Int ={
  Math.abs(prev - curr) - blockedChannelsWithinRange(Math.min(prev, curr), Math.max(prev, curr), blocked)
}

def forward(start:Int, end:Int, prev:Int, curr:Int, blocked:List[Int]):Int ={
  1 + end - start - Math.abs(prev - curr) - blockedChannelsWithinRange(start, Math.min(prev, curr), blocked) - blockedChannelsWithinRange(Math.max(prev,curr), end, blocked)
}

def minSize(start: Int, end: Int, prev: Int, curr: Int, blocked: List[Int]):Int ={
  Math.min(size(1, curr), Math.min(forward(start, end, prev, curr, blocked), backward(prev, curr, blocked)))
}

def minimumClicks(accumulator:Int, start:Int, end:Int, prev:Int, back:Int,blocked:List[Int], sequence:List[Int]):Int ={
  sequence match {
    case Nil => accumulator
    case head :: tail if head == back => minimumClicks(accumulator+1, start, end, head, prev, blocked, tail)
    case head :: tail => minimumClicks(accumulator+minSize(start, end, prev, head, blocked), start, end, head, prev, blocked, tail)
  }
}

def remote(start:Int, end:Int, blocked:List[Int], channelSequence:List[Int]):Int ={
  minimumClicks(0, start, end, start, start-1, blocked, channelSequence)
}

remote(1,20,List(18,19),List(15,14,17,1,17))
remote(103,108,List(104),List(105,106,107,103,105))
remote(1,200,List(),List(1, 100, 1, 101))
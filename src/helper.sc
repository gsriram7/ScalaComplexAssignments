val number = (1 to 10).toList
number.filter(x=>{x>5 && x<9}).size

def size(accumulator:Int, number:Int):Int = {
  if (number < 10)
    accumulator
  else
    size(accumulator+1, number/10)
}
size(1, 1234512345)


def blockedChannelsWithinRange(from: Int, to: Int, blocked: List[Int], f:Int=>Boolean) = blocked.count(x=> f(x))

def traverseBackward(start:Int, end:Int, blocked:List[Int]):Int ={
  Math.abs(start - end) - blockedChannelsWithinRange(Math.min(start, end), Math.max(start, end), blocked, (x:Int)=> if(start<x && x<end)true else false)
}

traverseBackward(15, 1, List(18, 19, 12))

def blockedChannelsOutOfRange(prev: Int, curr: Int, start: Int, end: Int, blocked: List[Int]) =blocked.count(x=> (start<x && x<prev) || (curr<x && x<end))

def traverseForward(prev:Int, curr:Int, start:Int,end:Int , blocked:List[Int]):Int ={
  1 + end - start - Math.abs(prev-curr) - blockedChannelsOutOfRange(Math.min(prev, curr), Math.max(prev, curr), start, end, blocked)
}

traverseForward(107, 103, 103, 108, List(104))

val number = (1 to 10).toList
number.filter(x=>{x>5 && x<9}).size

def blockedChannelsWithinRange(start:Int, end:Int, blocked:List[Int]):Int = blocked.count(x =>start < x && x < end)

blockedChannelsWithinRange(2, 7, List(1,4,5))

def size(accumulator:Int, number:Int):Int = {
  if (number < 10)
    accumulator
  else
    size(accumulator+1, number/10)
}
size(1, 1234512345)

def minSize(start: Int, end: Int, prev: Int, curr: Int, blocked: List[Int]):Int ={
  val sizeOfNumber = size(1, curr)
  val backward = Math.abs(Math.abs(prev - curr) - blockedChannelsWithinRange(prev, curr, blocked)) % end
  val forward = Math.abs(curr - end) + prev - blockedChannelsWithinRange(curr, end, blocked) -blockedChannelsWithinRange(start, prev, blocked)
  println("size "+sizeOfNumber+"\nbackward "+backward+"\nforward "+forward)
  Math.min(sizeOfNumber, Math.min(forward, backward))
}

minSize(1, 20, 11, 15, List(12, 18, 19))


def size2(number:Int)= number.toString.size

size2(123)


def traverseBackward(start:Int, end:Int, blocked:List[Int]):Int ={
  Math.abs(start - end) - blockedChannelsWithinRange(Math.min(start, end), Math.max(start, end), blocked)
}

traverseBackward(15, 1, List(18, 19))

def blockedChannelsOutOfRange(prev: Int, curr: Int, start: Int, end: Int, blocked: List[Int]) =blocked.count(x=> (start<x && x<prev) || (curr<x && x<end))

def traverseForward(prev:Int, curr:Int, start:Int,end:Int , blocked:List[Int]):Int ={
  1 + end - start - Math.abs(prev-curr) - blockedChannelsOutOfRange(Math.min(prev, curr), Math.max(prev, curr), start, end, blocked)
}

traverseForward(107, 103, 103, 108, List(104))

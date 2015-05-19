
def disp(i:Int, j:Int, b:List[Int], f:Int=>Boolean)= b.count(x=> f(x))

val blo = List(18, 19)

disp(15, 20, blo, (x:Int)=> if(15<x && x<20)true else false)

def blockedChannelsWithinRange(from: Int, to: Int, blocked: List[Int]) = blocked.count(x=> from<x && x<to)

def traverseBackward(prev:Int, curr:Int, blocked:List[Int]):Int ={
  Math.abs(prev - curr) - blockedChannelsWithinRange(Math.min(prev, curr), Math.max(prev, curr), blocked)
}

def traverseForward(start:Int, end:Int, prev:Int, curr:Int, blocked:List[Int]):Int ={
  1 + end - start - Math.abs(prev - curr) - blockedChannelsWithinRange(start, Math.min(prev, curr), blocked) - blockedChannelsWithinRange(Math.max(prev,curr), end, blocked)
}

traverseBackward(15, 1, List(18, 19, 12, 11))
traverseForward(1, 20, 15, 14, List(18, 19, 12, 11))

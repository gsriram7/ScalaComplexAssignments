val number = (1 to 10).toList
number.count(x => {x > 5 && x < 9})

def size(accumulator:Int, number:Int):Int = {
  if (number < 10)
    accumulator
  else
    size(accumulator+1, number/10)
}
size(1, 1234512345)


def blockedChannelsWithinRange(blocked: List[Int], f: (Int) => Boolean) = blocked.count(x=> f(x))

def tryBlocking(start:Int, end:Int, prev:Int, curr:Int, blocked:List[Int]): Int ={
  blockedChannelsWithinRange(blocked, {x:Int => (start<x && x<prev) || (curr<x && x<end)})
}
def tryBlockingWithin(prev: Int, curr: Int, blocked: List[Int]): Int ={
  blockedChannelsWithinRange(blocked, {x:Int => prev<x && x<curr})
}

tryBlocking(1, 20, 1, 15, List(11, 10, 14, 18, 19))
tryBlockingWithin(1, 15, List(11, 10, 14, 18, 19))

tryBlockingWithin(105, 106, List(104))
tryBlocking(103, 108, 105, 106, List(104))


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
def minSize(start:Int, end:Int, curr:Int, prev:Int, blocked:List[Int]):Int ={
  val sizeOfNumber = size(1, curr)
  val backward = Math.abs(Math.abs(prev - curr) - blockedChannelsWithinRange(prev, curr, blocked)) % end
  val forward = Math.abs(curr - end) + prev - blockedChannelsWithinRange(curr, end, blocked) -blockedChannelsWithinRange(start, curr, blocked)
  println("size "+sizeOfNumber+"\nbackward "+backward+"\nforward "+forward)
  Math.min(sizeOfNumber, Math.min(forward, backward))
}

minSize(1,20,15,11,List(12,18,19))
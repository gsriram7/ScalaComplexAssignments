case class Position(x: Int, y: Int, direction: Char)

val point = Position(0, 0, 'W')

def moveWest(point: Position): Position = Position(point.x, point.y - 1, 'W')
def moveEast(point: Position): Position = Position(point.x, point.y + 1, 'E')
def moveNorth(point: Position): Position = Position(point.x + 1, point.y, 'N')
def moveSouth(point: Position): Position = Position(point.x - 1, point.y, 'S')

val move = Map('W' -> moveWest _, 'E' -> moveEast _, 'N' -> moveNorth _, 'S' -> moveSouth _)

val seq = List('E', 'N', 'W', 'S')

def marsRover(accumulator:Position, sequence: List[Char]):Position ={
  sequence match {
    case head :: Nil => move(head)(accumulator)
    case head :: tail if (head == 'M') => move(accumulator.direction)(accumulator)
    case head :: tail if (head != 'M') => move(accumulator.direction)(accumulator)
    case head :: tail => marsRover(move(head)(accumulator), tail)
  }
}

marsRover(point, seq)
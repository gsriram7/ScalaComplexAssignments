case class Position(x: Int, y: Int, direction: Char)

def moveWest(point: Position): Position = Position(point.x, point.y - 1, 'W')
def moveEast(point: Position): Position = Position(point.x, point.y + 1, 'E')
def moveNorth(point: Position): Position = Position(point.x + 1, point.y, 'N')
def moveSouth(point: Position): Position = Position(point.x - 1, point.y, 'S')

val move = Map('W' -> moveWest _, 'E' -> moveEast _, 'N' -> moveNorth _, 'S' -> moveSouth _)

val seq = List('W', 'M', 'S', 'M', 'E', 'M', 'N', 'M', 'M')

def marsRover(accumulator: Position, sequence: List[Char]): Position = {
  sequence match {
    case head :: Nil if head == 'M' => move(accumulator.direction)(accumulator)
    case head :: Nil => Position(accumulator.x, accumulator.y, head)
    case head :: tail if head == 'M' => marsRover(move(accumulator.direction)(accumulator), tail)
    case head :: tail => marsRover(Position(accumulator.x, accumulator.y, head), tail)
  }
}

marsRover(Position(2, 1, 'N'), seq)

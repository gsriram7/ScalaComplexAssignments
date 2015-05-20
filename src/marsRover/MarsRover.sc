case class Position(x: Int, y: Int, direction: Char)

def moveWest(point: Position): Position = Position(point.x - 1, point.y, 'W')
def moveEast(point: Position): Position = Position(point.x + 1, point.y, 'E')
def moveNorth(point: Position): Position = Position(point.x, point.y + 1, 'N')
def moveSouth(point: Position): Position = Position(point.x, point.y - 1, 'S')

val move = Map('W' -> moveWest _, 'E' -> moveEast _, 'N' -> moveNorth _, 'S' -> moveSouth _)
val directions: Seq[Char] = Array('N', 'E', 'S', 'W')

def marsRover(accumulator: Position, sequence: List[Char]): Position = {
  sequence match {
    case head :: Nil if head == 'M' => move(accumulator.direction)(accumulator)
    case head :: Nil if head == 'R' => Position(accumulator.x, accumulator.y, directions((directions.indexOf(accumulator.direction)+1)%4))
    case head :: Nil if head == 'L' => Position(accumulator.x, accumulator.y, directions((directions.indexOf(accumulator.direction)-1)%4))

    case head :: tail if head == 'M' => marsRover(move(accumulator.direction)(accumulator), tail)
    case head :: tail if head == 'R' => marsRover(Position(accumulator.x, accumulator.y, directions((directions.indexOf(accumulator.direction)+1+4)%4)), tail)
    case head :: tail if head == 'L' => marsRover(Position(accumulator.x, accumulator.y, directions((directions.indexOf(accumulator.direction)-1+4)%4)), tail)
  }
}

marsRover(Position(1, 2, 'N'), "LMLMLMLMM".toCharArray.toList)
marsRover(Position(3, 3, 'E'), "MMRMMRMRRM".toCharArray.toList)

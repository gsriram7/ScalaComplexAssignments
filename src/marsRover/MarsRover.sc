case class Coordinate(x:Int, y:Int)

val point = Coordinate(0, 0)

def moveWest(point:Coordinate):Coordinate = Coordinate(point.x, point.y-1)

def moveEast(point:Coordinate):Coordinate = Coordinate(point.x, point.y+1)

def moveNorth(point:Coordinate):Coordinate = Coordinate(point.x+1, point.y)

def moveSouth(point:Coordinate):Coordinate = Coordinate(point.x-1, point.y)




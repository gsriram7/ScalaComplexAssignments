def one() = "One"

def two() = "Two"

var res = Map(1->one(), 2->two())

println(res.get(1).get)


def p(x:Int) = x+2

val r = Map('a'->p _)

r('a')(5)

val dir = Array('N', 'E', 'W', 'S')

dir.indexOf('W')

val d2: Seq[Char] = dir

d2.indexOf('E')




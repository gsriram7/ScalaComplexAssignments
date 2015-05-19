def one() = "One"

def two() = "Two"

var res = Map(1->one(), 2->two())

println(res.get(1).get)


def p(x:Int) = x+2

val r = Map('a'->p _)

r('a')(5)
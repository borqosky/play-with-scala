import week4._


Expr.show(Sum(Number(1), Number(2)))

Expr.show(Sum(Prod(Number(2), Var("x")), Var("y")))

Expr.show(Prod(Sum(Number(2), Var("x")), Var("y")))

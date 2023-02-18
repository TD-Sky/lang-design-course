// 引入变量的声明与访问
type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Let(string, expr, expr)
  | Var(string)

let interpret = (expr: expr) => {
  let rec go = (expr, env: list<(string, int)>) => {
    switch expr {
    | Cst(i) => i
    | Add(a, b) => go(a, env) + go(b, env)
    | Mul(a, b) => go(a, env) * go(b, env)
    | Let(var, val, e) => go(e, list{(var, go(val, env)), ...env})
    | Var(x) => List.assoc(x, env)
    }
  }

  go(expr, list{})
}

module Test = {
  let test_interpret = () => {
    let expr = Let("x", Cst(2), Add(Var("x"), Mul(Cst(3), Cst(4))))
    let value = expr->interpret
    assert (value == 14)
  }
}

Test.test_interpret()

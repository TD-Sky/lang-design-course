open Belt

type instrution = Cst(int) | Add | Mul | Var(int) | Swap | Pop

module NameLess = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Let(expr, expr)
    | Var(int)
}

module Indexed = {
  type var = Local | Tmp

  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Let(expr, expr)
    | Var(int) // stack_index
}

// 打印指令栈
let print = (instructions: list<instrution>) => {
  let instructions =
    instructions
    ->List.map(instr => {
      switch instr {
      | Cst(i) => `Cst(${Int.toString(i)})`
      | Add => "Add"
      | Mul => "Mul"
      | Swap => "Swap"
      | Pop => "Pop"
      | Var(i) => `Var(${Int.toString(i)})`
      }
    })
    ->List.toArray

  Js.log(instructions)
}

let named_var_position = (list, elm: string): int => {
  let rec go = (list, index: int): int => {
    switch list {
    | list{} => raise(Not_found)
    | list{x, ...rest} =>
      if x == elm {
        index
      } else {
        go(rest, index + 1)
      }
    }
  }

  go(list, 0)
}

let to_nameless = (expr: Tiny1.expr): NameLess.expr => {
  let rec go = (expr: Tiny1.expr, cenv: list<string>): NameLess.expr => {
    switch expr {
    | Cst(i) => Cst(i)
    | Add(a, b) => Add(go(a, cenv), go(b, cenv))
    | Mul(a, b) => Mul(go(a, cenv), go(b, cenv))
    | Let(var, val, e) => Let(go(val, cenv), go(e, list{var, ...cenv}))
    | Var(i) => Var(named_var_position(cenv, i))
    }
  }

  go(expr, list{})
}

let local_var_position = (list: list<Indexed.var>, n: int): int => {
  let rec go = (list: list<Indexed.var>, n: int, index: int): int => {
    switch list {
    | list{} => raise(Not_found)
    | list{Indexed.Local, ...rest} =>
      if n == 0 {
        index
      } else {
        go(rest, n - 1, index + 1)
      }
    | list{Indexed.Tmp, ...rest} => go(rest, n, index + 1)
    }
  }

  go(list, n, 0)
}

let to_indexed = (expr: NameLess.expr): Indexed.expr => {
  let rec go = (expr: NameLess.expr, senv: list<Indexed.var>): Indexed.expr => {
    switch expr {
    | Cst(i) => Cst(i)
    | Add(a, b) => Add(go(a, senv), go(b, list{Tmp, ...senv}))
    | Mul(a, b) => Mul(go(a, senv), go(b, list{Tmp, ...senv}))
    | Let(val, e) => Let(go(val, senv), go(e, list{Local, ...senv}))
    | Var(i) => Var(local_var_position(senv, i)) // stack_index
    }
  }

  go(expr, list{})
}

let rec compile = (expr: Indexed.expr): list<instrution> => {
  switch expr {
  | Cst(i) => list{Cst(i)}
  | Add(a, b) => [compile(a), compile(b), list{Add}]->List.concatMany
  | Mul(a, b) => [compile(a), compile(b), list{Mul}]->List.concatMany
  | Let(val, e) => [compile(val), compile(e), list{Swap, Pop}]->List.concatMany
  | Var(stack_index) => list{Var(stack_index)}
  }
}

let run = (instrutions: list<instrution>): int => {
  let rec go = (instrutions: list<instrution>, stack: list<int>): int => {
    switch (instrutions, stack) {
    | (list{Cst(i), ...instrutions}, _) => go(instrutions, list{i, ...stack})
    | (list{Add, ...instrutions}, list{a, b, ...stack}) => go(instrutions, list{a + b, ...stack})
    | (list{Mul, ...instrutions}, list{a, b, ...stack}) => go(instrutions, list{a * b, ...stack})
    | (list{Var(n), ...instrutions}, _) =>
      go(instrutions, list{stack->List.getExn(n), ...stack})
    | (list{Swap, ...instrutions}, list{a, b, ...stack}) => go(instrutions, list{b, a, ...stack})
    | (list{Pop, ...instrutions}, list{_, ...stack}) => go(instrutions, stack)
    | (list{}, list{value, ..._stack}) => value
    | _ => Utils.panic("unexpected VM state")
    }
  }

  go(instrutions, list{})
}

module Test = {
  let test_stack_vm = () => {
    let expr: Tiny1.expr = Let("x", Cst(2), Add(Var("x"), Mul(Cst(3), Cst(4))))
    let instrutions = expr->to_nameless->to_indexed->compile
    let value = instrutions->run
    assert (value == 14)
  }
}

Test.test_stack_vm()

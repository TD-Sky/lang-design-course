open Belt

// 纯运算
type rec expr = Cst(int) | Add(expr, expr) | Mul(expr, expr)
type instruction = Cst(int) | Add | Mul

// 打印指令栈
let print = (instructions: list<instruction>) => {
  let instructions =
    instructions
    ->List.map(instr => {
      switch instr {
      | Cst(i) => `Cst(${Int.toString(i)})`
      | Add => "Add"
      | Mul => "Mul"
      }
    })
    ->List.toArray

  Js.log(instructions)
}

// 解释执行表达式
let rec interpret = (expr: expr): int => {
  switch expr {
  | Cst(i) => i
  | Add(a, b) => interpret(a) + interpret(b)
  | Mul(a, b) => interpret(a) * interpret(b)
  }
}

// 编译表达式至指令流
let rec compile = (expr: expr): list<instruction> => {
  switch expr {
  | Cst(i) => list{Cst(i)}
  | Add(a, b) => [compile(a), compile(b), list{Add}]->List.concatMany
  | Mul(a, b) => [compile(a), compile(b), list{Mul}]->List.concatMany
  }
}

// 执行指令流
let run = (instructions: list<instruction>): int => {
  let rec go = (instructions, stack: list<int>): int => {
    switch (instructions, stack) {
    | (list{Cst(i), ...instructions}, _) => go(instructions, list{i, ...stack})
    | (list{Add, ...instructions}, list{a, b, ...stack}) =>
      go(instructions, list{a + b, ...stack})
    | (list{Mul, ...instructions}, list{a, b, ...stack}) =>
      go(instructions, list{a * b, ...stack})
    | (list{}, list{value, ..._stack}) => value
    | _ => Utils.panic("unexpected VM state")
    }
  }

  go(instructions, list{})
}

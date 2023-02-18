exception Anyhow(string)

let panic = (msg: string) => {
  Js.log(`\nPanic: ${msg}`)
  raise(Anyhow(msg))
}

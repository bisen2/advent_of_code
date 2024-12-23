module AdventOfCode2024.Day17
#nowarn 3391

open FParsec
open Util

let sample = $"{dataFolder}/sample/Day17.txt"
let input = $"{dataFolder}/actual/Day17.txt"

type Register =
  { A: bigint
    B: bigint
    C: bigint }
  static member ofTuple (a,b,c) = { A = a; B = b; C = c }

let registerParser =
  tuple3
    (pstring "Register A: " >>. pint32 .>> newline)
    (pstring "Register B: " >>. pint32 .>> newline)
    (pstring "Register C: " >>. pint32 .>> newline)
  |>> Register.ofTuple

let programParser = pstring "Program: " >>. sepBy pint32 (pstring ",")

let parser = registerParser .>> newline .>>. programParser

let comboOperand (reg: Register) op =
  match op with
  | 0 | 1 | 2 | 3 -> bigint op
  | 4 -> reg.A
  | 5 -> reg.B
  | 6 -> reg.C
  | 7 -> failwith "Combo operand 7 is disallowed."
  | _ -> failwith $"Overflow! Combo operand {op} is out of range for a three-bit integer!"

[<TailCall>]
let rec procInstr prog ptr output reg =
  let combo = comboOperand reg
  match List.tryItem ptr prog, List.tryItem (ptr+1) prog with
  | Some 0, Some op -> // adv
      { reg with A = reg.A / pown (bigint 2) (combo op |> int) }
      |> procInstr prog (ptr+2) output
  | Some 1, Some op -> // bxl
      { reg with B = reg.B ^^^ op }
      |> procInstr prog (ptr+2) output
  | Some 2, Some op -> // bst
      { reg with B = (combo op) % (bigint 8) }
      |> procInstr prog (ptr+2) output
  | Some 3, Some op -> // jnz
      let ptr = if reg.A = 0 then ptr+2 else op
      procInstr prog ptr output reg
  | Some 4, Some op -> // bxc
      { reg with B = reg.B ^^^ reg.C }
      |> procInstr prog (ptr+2) output
  | Some 5, Some op -> // out
      procInstr prog (ptr+2) (combo op % (bigint 8) :: output) reg
  | Some 6, Some op -> // bdv
      { reg with B = reg.A / pown (bigint 2) (combo op |> int) }
      |> procInstr prog (ptr+2) output
  | Some 7, Some op -> // cdv
      { reg with C = reg.A / pown (bigint 2) (combo op |> int) }
      |> procInstr prog (ptr+2) output
  | _ -> List.rev output

module Part1 =

  let run file =
    let reg, prog =
      System.IO.File.ReadAllText file
      |> parse parser
    procInstr prog 0 [] reg
    |> List.map (sprintf "%A")
    |> String.concat ","

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let findSelfReplicating reg prog =
    let mutable possible = []
    let rec impl reg compIndex =
      for n in 0 .. 7 do
        let r = { reg with A = reg.A <<< 3 ||| n }
        let o = procInstr prog 0 [] r
        if prog |> List.map bigint |> List.lastN compIndex = o then
          if prog |> List.map bigint = o then
            possible <- r::possible
          else
            impl r (compIndex+1)
    impl { reg with A = 0 } 1
    possible

  let run file =
    let reg, prog =
      System.IO.File.ReadAllText file
      |> parse parser

    findSelfReplicating reg prog
    |> List.minBy (fun reg -> reg.A)
    |> _.A

  let runSample() = run sample
  let runInput() = run input

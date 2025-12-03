module AdventOfCode2025.Day1

open FParsec
open Util

let sampleFile = $"{dataFolder}/sample/Day1.txt"
let inputFile = $"{dataFolder}/actual/Day1.txt"

type Direction = L | R

let instructionParser =
  pchar 'L' >>% L
  <|>
  (pchar 'R' >>% R)
  .>>.
  pint32

let instructionsParser = many (instructionParser .>> spaces)

let applyStep state step =
  let newState =
    match step with
    | L -> state - 1
    | R -> state + 1
  if newState > 99 then newState - 100
  elif newState < 0 then newState + 100
  else newState

let applyInstruction state (dir, dist) =
  Seq.init dist (fun _ -> dir)
  |> Seq.fold applyStep state

module Part1 =

  let run =
    System.IO.File.ReadAllText
    >> parse instructionsParser
    >> Seq.scan applyInstruction 50
    >> Seq.where ((=) 0)
    >> Seq.length

  let runSample() = run sampleFile
  let runInput() = run inputFile

module Part2 =

  let run =
    System.IO.File.ReadAllText
    >> parse instructionsParser
    >> Seq.map (fun (dir, dist) -> Seq.init dist (fun _ -> dir))
    >> Seq.concat
    >> Seq.scan applyStep 50
    >> Seq.where ((=) 0)
    >> Seq.length

  let runSample() = run sampleFile
  let runInput() = run inputFile

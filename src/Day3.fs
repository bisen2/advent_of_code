module AdventOfCode2024.Day3

open FParsec
open Util

let sampleFile = $"{__SOURCE_DIRECTORY__}/../data/sample/Day3.txt"
let inputFile = $"{__SOURCE_DIRECTORY__}/../data/actual/Day3.txt"

let mulParser = pstring "mul(" >>. pint32 .>> pstring "," .>>. pint32 .>> pstring ")"

module Part1 =

  type ParseResult = Mul of int*int | Corrupt

  let parser = many (attempt (mulParser |>> Mul) <|> (anyChar >>% Corrupt))

  let run =
    System.IO.File.ReadAllText
    >> parse parser
    >> List.sumBy (function Mul (x,y) -> x * y | Corrupt -> 0)

  let runSample() = run sampleFile
  let runInput() = run inputFile

module Part2 =

  type ParseResult = Mul of int*int | Do | Dont | Corrupt

  let parser =
    choice
      [ attempt (mulParser |>> Mul)
        pstring "don't()" >>% Dont
        pstring "do()" >>% Do
        anyChar >>% Corrupt ]
    |> many

  let runProg input =
    let folder (enabled, soFar) instruction =
      match instruction with
      | Do -> (true, soFar)
      | Dont -> (false, soFar)
      | Mul (x,y) when enabled -> (enabled, soFar + (x * y))
      | _ -> (enabled, soFar)
    List.fold folder (true, 0) input
    |> snd

  let run = System.IO.File.ReadAllText >> parse parser >> runProg

  let runSample() = run sampleFile
  let runInput() = run inputFile

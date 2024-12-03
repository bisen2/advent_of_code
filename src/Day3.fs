module AdventOfCode2024.Day3

open FParsec
open Util

let sampleFile = $"{__SOURCE_DIRECTORY__}/../data/sample/Day3.txt"
let inputFile = $"{__SOURCE_DIRECTORY__}/../data/actual/Day3.txt"

module Part1 =

  type ParseResult = Mul of int*int | Corrupt

  let parser : Parser<ParseResult list,unit> =
    many (
      // attempt to match the "mul(x,y)" pattern
      attempt ((pstring "mul(" >>. pint32 .>> pstring "," .>>. pint32 .>> pstring ")") |>> Mul)
      // if can't match, then mark as a corrupt character and move on
      <|> (anyChar >>% Corrupt)
    )

  let run =
    System.IO.File.ReadAllText
    >> parse parser
    >> List.sumBy (function Mul (x,y) -> x * y | Corrupt -> 0)

  let runSample() = run sampleFile

  let runInput() = run inputFile

module Part2 =

  type ParseResult = Mul of int*int | Do | Dont | Corrupt

  let parser: Parser<ParseResult list, unit> =
    many (
      // attempt to match the "mul(x,y)" pattern
      attempt ((pstring "mul(" >>. pint32 .>> pstring "," .>>. pint32 .>> pstring ")") |>> Mul)
      // next, check for the "don't()" pattern
      <|> attempt (pstring "don't()" >>% Dont)
      // now, the "do()" pattern
      <|> attempt (pstring "do()" >>% Do)
      // otherwise, it is corrupt
      <|> (anyChar >>% Corrupt)
    )

  type ProgState = { Enabled: bool; SoFar: int }

  let runProg (input: ParseResult list) =
    let folder state instruction =
      match instruction with
      | Do -> { state with Enabled = true }
      | Dont -> { state with Enabled = false }
      | Mul (x,y) when state.Enabled -> { state with SoFar = state.SoFar + (x * y) }
      | _ -> state
    List.fold folder { Enabled = true; SoFar = 0 } input
    |> _.SoFar

  let run = System.IO.File.ReadAllText >> parse parser >> runProg

  let runSample() = run sampleFile
  let runInput() = run inputFile

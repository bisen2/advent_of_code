module AdventOfCode2025.Day3

open FParsec
open Util

let sampleFile = $"{dataFolder}/sample/Day3.txt"
let inputFile = $"{dataFolder}/actual/Day3.txt"

let pDigit = digit |>> string |>> System.Int32.Parse
let bankParser = many1 pDigit .>> spaces
let packParser = many bankParser

let findMaxDigitSet n (xs: List<_>) =
  let rec impl (xs: List<_>) n soFar =
    if n = 0 then List.rev soFar
    else
      let this = List.maxi xs[.. List.length xs - n]
      impl (xs[this.Index + 1 ..]) (n - 1) (this :: soFar)
  impl xs n []
  |> List.map (fun x -> x.Value.ToString())
  |> String.concat ""
  |> System.Int64.Parse

module Part1 =

  let run =
    System.IO.File.ReadAllText
    >> parse packParser
    >> List.map (findMaxDigitSet 2)
    >> List.sum

  let runSample() = run sampleFile
  let runInput() = run inputFile

module Part2 =

  let run =
    System.IO.File.ReadAllText
    >> parse packParser
    >> List.map (findMaxDigitSet 12)
    >> List.sum

  let runSample() = run sampleFile
  let runInput() = run inputFile

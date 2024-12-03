module AdventOfCode2023.Day1

open FParsec
open Util

let getCalibrationValue parser =
  parse parser
  >> List.choose id
  >> fun xs -> $"{List.head xs}{List.last xs}"
  >> System.Int32.Parse

module Part1 =

  let parser = (digit |>> Some) <|> (anyChar >>% None) |> many

  let run =
    System.IO.File.ReadAllLines
    >> Seq.toList
    >> List.sumBy (getCalibrationValue parser)

  let runSample() = run $"{dataFolder}/sample/Day1_Part1.txt"
  let runInput() = run $"{dataFolder}/actual/Day1.txt"

module Part2 =

  let parser =
    choice
      [ digit |>> Some
        pstring "one" >>% Some '1'
        pstring "two" >>% Some '2'
        pstring "three" >>% Some '3'
        pstring "four" >>% Some '4'
        pstring "five" >>% Some '5'
        pstring "six" >>% Some '6'
        pstring "seven" >>% Some '7'
        pstring "eight" >>% Some '8'
        pstring "nine" >>% Some '9'
        anyChar >>% None ]
    |> many

  let run =
    System.IO.File.ReadAllLines
    >> Seq.toList
    >> List.sumBy (getCalibrationValue parser)

  let runSample() = run $"{dataFolder}/sample/Day1_Part2.txt"
  let runInput() = run $"{dataFolder}/actual/Day1.txt"

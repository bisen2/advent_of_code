module AdventOfCode2024.Day1

open System
open FParsec
open Util

let sampleFile = "../data/Day1_Sample.txt"
let inputFile = "../data/Day1_Input.txt"

let parseFile = IO.File.ReadAllLines >> Seq.toList >> List.map (parse (pint32 .>> spaces .>>. pint32))

module Part1 =

  let run =
    parseFile
    >> List.unzip
    >> fun (xs, ys) -> List.sort xs, List.sort ys
    >> fun (xs, ys) -> List.zip xs ys
    >> List.map (fun (x, y) -> Math.Abs (x - y))
    >> List.sum

  let runSample() = run sampleFile

  let runInput() = run inputFile

module Part2 =

  let run file =
    let (xs, ys) = parseFile file |> List.unzip
    List.sumBy (fun x -> x * (ys |> List.filter (fun y -> y = x) |> List.length)) xs

  let runSample() = run sampleFile
  let runInput() = run inputFile

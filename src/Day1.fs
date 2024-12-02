module AdventOfCode2024.Day1

open FParsec
open Util

let sampleFile = $"{__SOURCE_DIRECTORY__}/../data/sample/Day1.txt"
let inputFile = $"{__SOURCE_DIRECTORY__}/../data/actual/Day1.txt"

let parseFile = System.IO.File.ReadAllLines >> Seq.toList >> List.map (parse (pint32 .>> spaces .>>. pint32))

module Part1 =

  let run =
    parseFile
    >> List.unzip
    >> fun (xs, ys) -> List.sort xs, List.sort ys
    >> fun (xs, ys) -> List.zip xs ys
    >> List.map (fun (x, y) -> System.Math.Abs (x - y))
    >> List.sum

  let runSample() = run sampleFile

  let runInput() = run inputFile

module Part2 =

  let run file =
    let (xs, ys) = parseFile file |> List.unzip
    List.sumBy (fun x -> x * (ys |> List.filter (fun y -> y = x) |> List.length)) xs

  let runSample() = run sampleFile
  let runInput() = run inputFile

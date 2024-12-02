module AdventOfCode2024.Day2

open System
open FParsec
open Util

let sampleFile = $"{__SOURCE_DIRECTORY__}/../data/Day2_Sample.txt"
let inputFile = $"{__SOURCE_DIRECTORY__}/../data/Day2_Input.txt"

let parseFile = System.IO.File.ReadAllLines >> Seq.toList >> List.map (parse (many (pint32 .>> spaces)))

module Part1 =

  let isStepIncreasing = List.pairwise >> List.map (fun (this, next) -> next - this > 0)
  let isStepDecreasing = List.pairwise >> List.map (fun (this, next) -> this - next > 0)
  let isStep1to3 =
    List.pairwise
    >> List.map (fun (this:int, next) ->
          let diff = Math.Abs(this - next)
          diff >= 1 && diff <= 3)

  let isMonoInc = isStepIncreasing >> List.exists not >> not
  let isMonoDec = isStepDecreasing >> List.exists not >> not
  let isDiffering1to3 = isStep1to3 >> List.exists not >> not

  let isSafe xs = (isMonoInc xs || isMonoDec xs) && isDiffering1to3 xs

  let run =
    parseFile
    >> List.map isSafe
    >> List.filter id
    >> List.length

  let runSample() = run sampleFile
  let runInput() = run inputFile

/// The current Day 2 Part 2 implementation is fairly silly. It just brute forces all possible
/// versions of the list with one item removed and checks if any of them are safe solutions.
module Part2 =

  let isSafe xs =
    [ 0 .. List.length xs - 1]
    |> List.map (fun i -> List.removeAt i xs)
    |> List.exists Part1.isSafe

  let run =
    parseFile
    >> List.map isSafe
    >> List.filter id
    >> List.length

  let runSample() = run sampleFile
  let runInput() = run inputFile

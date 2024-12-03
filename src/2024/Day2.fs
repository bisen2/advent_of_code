module AdventOfCode2024.Day2

open FParsec
open Util

let sampleFile = $"{dataFolder}/sample/Day2.txt"
let inputFile = $"{dataFolder}/actual/Day2.txt"

let parseFile = System.IO.File.ReadAllLines >> Seq.toList >> List.map (parse (many (pint32 .>> spaces)))

module Part1 =

  let meetsPred pred = List.pairwise >> List.map pred >> List.exists not >> not

  let isMonoInc = meetsPred (fun (this, next) -> next - this > 0)
  let isMonoDec = meetsPred (fun (this, next) -> this - next > 0)
  let isDiffering1to3 =
    let pred (this: int, next) =
      let diff = System.Math.Abs (this - next)
      diff >= 1 && diff <= 3
    meetsPred pred

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

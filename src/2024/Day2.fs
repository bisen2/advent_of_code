module AdventOfCode2024.Day2

open FParsec
open Util

let sampleFile = $"{dataFolder}/sample/Day2.txt"
let inputFile = $"{dataFolder}/actual/Day2.txt"

let parseFile = System.IO.File.ReadAllLines >> Seq.map (parse (many (pint32 .>> spaces)))

module Part1 =

  let meetsPred pred = Seq.pairwise >> Seq.map pred >> Seq.exists not >> not

  let isMonoInc = meetsPred (fun (this, next) -> next - this > 0)
  let isMonoDec = meetsPred (fun (this, next) -> this - next > 0)
  let isDiffering1to3 =
    let pred (this: int, next) =
      let diff = System.Math.Abs (this - next)
      diff >= 1 && diff <= 3
    meetsPred pred

  let isSafe (xs: seq<_>) = (isMonoInc xs || isMonoDec xs) && isDiffering1to3 xs

  let run =
    parseFile
    >> Seq.map isSafe
    >> Seq.filter id
    >> Seq.length

  let runSample() = run sampleFile
  let runInput() = run inputFile

/// The current Day 2 Part 2 implementation is fairly silly. It just brute forces all possible
/// versions of the list with one item removed and checks if any of them are safe solutions.
module Part2 =

  let isSafe xs =
    seq { 0 .. Seq.length xs - 1 }
    |> Seq.map (fun i -> Seq.removeAt i xs)
    |> Seq.exists Part1.isSafe

  let run =
    parseFile
    >> Seq.map isSafe
    >> Seq.filter id
    >> Seq.length

  let runSample() = run sampleFile
  let runInput() = run inputFile

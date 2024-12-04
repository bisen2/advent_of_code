module AdventOfCode2024.Day4

open Util

let sample = $"{dataFolder}/sample/Day4.txt"
let input = $"{dataFolder}/actual/Day4.txt"

module Part1 =

  let lookDir puzzle dir = [0..3] |> Seq.map dir |> Seq2.trySlice puzzle

  let isTarget maybeWord =
    match Option.map Seq.toList maybeWord with
    | Some [ 'X'; 'M'; 'A'; 'S' ] -> true
    | _ -> false

  let directions (y,x) =
    [ fun i -> (y, x+i)   // right
      fun i -> (y, x-i)   // left
      fun i -> (y+i, x)   // down
      fun i -> (y-i, x)   // up
      fun i -> (y+i, x+i) // diagonals
      fun i -> (y+i, x-i)
      fun i -> (y-i, x+i)
      fun i -> (y-i, x-i) ]

  let lookAround puzzle pos =
    directions pos
    |> List.map (lookDir puzzle)
    |> List.map isTarget
    |> List.sumBy (function true -> 1 | false -> 0)

  let run file =
    let puzzle = System.IO.File.ReadAllLines file |> Seq.map (Seq.map id)
    Seq2.iiwhere ((=) 'X') puzzle // find all 'X' locations
    |> Seq.map (lookAround puzzle) // look around them for words
    |> Seq.sum // count 'em up

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let lookDir puzzle dir = [-1 .. 1] |> Seq.map dir |> Seq2.trySlice puzzle

  let isTarget maybeWord =
    match Option.map Seq.toList maybeWord with
    | Some [ 'M'; 'A'; 'S' ] -> true
    | Some [ 'S'; 'A'; 'M' ] -> true
    | _ -> false

  let directions (y,x) =
    [ (fun i -> (y, x+i)), (fun i -> (y+i, x))
      (fun i -> (y+i, x+i)), (fun i -> (y+i, x-i)) ]

  let solutionsHere puzzle =
    directions
    >> List.map (fun (d1,d2) -> lookDir puzzle d1, lookDir puzzle d2)
    >> List.map (fun (a, b) -> isTarget a && isTarget b)
    >> List.sumBy (function true -> 1 | false -> 0)

  let run file =
    let puzzle = System.IO.File.ReadAllLines file |> Seq.map (Seq.map id)
    Seq2.iiwhere ((=) 'A') puzzle
    |> Seq.map (solutionsHere puzzle)
    |> Seq.sum

  let runSample() = run sample
  let runInput() = run input

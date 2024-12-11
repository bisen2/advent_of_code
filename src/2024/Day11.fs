module AdventOfCode2024.Day11

open Util

let sample = $"{dataFolder}/sample/Day11.txt"
let input = $"{dataFolder}/actual/Day11.txt"

let countStonesAfterNCached blinks stones =
  let mutable cache = Map<bigint*int, bigint> []

  let rec countSubstones iter stone =
    if iter<=0 then bigint.One
    else
      match Map.tryFind (stone, iter) cache with
      | Some count -> count
      | None ->
          let count =
            if stone = bigint.Zero then countSubstones (iter-1) bigint.One
            else
              let str = stone.ToString()
              if str.Length%2 = 0 then
                let left = bigint.Parse str[..str.Length/2-1]
                let right = bigint.Parse str[str.Length/2..]
                (countSubstones (iter-1) left) + (countSubstones (iter-1) right)
              else
                countSubstones (iter-1) (stone * bigint 2024)
          cache <- Map.add (stone, iter) count cache
          count
  stones |> Array.sumBy (countSubstones blinks)

module Part1 =

  let run =
    System.IO.File.ReadAllLines
    >> Seq.head
    >> _.Split()
    >> Array.map bigint.Parse
    >> countStonesAfterNCached 25

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let run =
    System.IO.File.ReadAllLines
    >> Seq.head
    >> _.Split()
    >> Array.map bigint.Parse
    >> countStonesAfterNCached 75

  let runSample() = run sample
  let runInput() = run input

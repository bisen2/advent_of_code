module AdventOfCode2024.Day8

open Util

let sample = $"{dataFolder}/sample/Day8.txt"
let input = $"{dataFolder}/actual/Day8.txt"

let getAntennas map =
  Seq2.iiwhere ((<>) '.') map
  |> Seq.groupBy (Seq2.item map)

module Part1 =

  let findAntinodes ((y1,x1), (y2,x2)) =
    let dx = x2 - x1
    let dy = y2 - y1
    [ (y2+dy, x2+dx); (y1-dy, x1-dx) ]

  let findAllAntinodes positions =
    Seq.allPairs positions positions // get all combinations of antenna locations
    |> Seq.filter (fun (p1,p2) -> p1 <> p2) // ignore the identity condition
    |> Seq.map findAntinodes
    |> Seq.concat
    |> Seq.distinct

  let run file =
    let map =
      System.IO.File.ReadAllLines file
      |> Seq.map (Seq.map id)
    let ylim = Seq.length map
    let xlim = Seq.length (Seq.head map)
    map
    |> getAntennas
    |> Seq.map (fun (id, positions) -> findAllAntinodes positions)
    |> Seq.concat
    |> Seq.distinct
    |> Seq.filter (fun (y,x) -> y >= 0 && y < ylim && x >= 0 && x < xlim)
    |> Seq.length

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let findAntinodes bound ((y1,x1), (y2,x2)) =
    let dx = x2 - x1
    let dy = y2 - y1
    // For now, we will just accept that this will generate a lot of antinodes off the edge of the map.
    // If we run into perf problems, we can make this more precise.
    [ for i in 0 .. bound -> (i*dy + y1, i*dx + x1) ]

  let findAllAntinodes bound positions =
    Seq.allPairs positions positions
    |> Seq.filter (fun (p1,p2) -> p1 <> p2) // A single antenna can't produce an antinode
    |> Seq.map (findAntinodes bound)
    |> Seq.concat
    |> Seq.distinct

  let run file =
    let map =
      System.IO.File.ReadAllLines file
      |> Seq.map (Seq.map id)
    let ylim = Seq.length map
    let xlim = Seq.length (Seq.head map)
    let bound = List.max [ ylim; xlim ]
    map
    |> getAntennas
    |> Seq.map (fun (id, positions) -> findAllAntinodes bound positions)
    |> Seq.concat
    |> Seq.distinct
    |> Seq.filter (fun (y,x) -> y >= 0 && y < ylim && x >= 0 && x < xlim)
    |> Seq.length

  let runSample() = run sample
  let runInput() = run input

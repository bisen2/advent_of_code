module AdventOfCode2024.Day10

open Util

let sample = $"{dataFolder}/sample/Day10.txt"
let input = $"{dataFolder}/actual/Day10.txt"

module Part1 =

  let rec findSummits map (x,y,z) =
    if z = 9 then [x,y,z]
    else
      [ (x+1,y); (x-1,y); (x,y+1); (x,y-1) ]
      |> List.choose (fun (x,y) -> Seq2.tryItem map (x,y) |> Option.map (fun z -> x,y,z))
      |> List.filter (fun (_, _, newZ) -> newZ = z + 1)
      |> List.map (findSummits map)
      |> List.concat

  let run file =
    let map =
      System.IO.File.ReadAllLines file
      |> Seq.map (Seq.map (fun c -> System.Int32.Parse (c.ToString())))
    map
    |> Seq2.iiwhere ((=) 0)
    |> Seq.map (fun (x,y) -> findSummits map (x,y,0))
    |> Seq.map Seq.distinct
    |> Seq.sumBy Seq.length

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let rec countPaths map (x,y,z) =
    if z = 9 then 1
    else
      [ (x+1,y); (x-1,y); (x,y+1); (x,y-1) ]
      |> List.choose (fun (x,y) -> Seq2.tryItem map (x,y) |> Option.map (fun z -> x,y,z))
      |> List.filter (fun (_,_,newZ) -> newZ = z + 1)
      |> List.sumBy (countPaths map)

  let run file =
    let map =
      System.IO.File.ReadAllLines file
      |> Seq.map (Seq.map (fun c -> System.Int32.Parse (c.ToString())))
    map
    |> Seq2.iiwhere ((=) 0)
    |> Seq.map (fun (x,y) -> countPaths map (x,y,0))
    |> Seq.sum

  let runSample() = run sample
  let runInput() = run input

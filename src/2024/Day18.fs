module AdventOfCode2024.Day18

open System
open System.Collections.Generic
open FParsec
open Util

let sample = $"{dataFolder}/sample/Day18.txt"
let input = $"{dataFolder}/actual/Day18.txt"

let parser = sepEndBy (pint32 .>> pstring "," .>>. pint32) newline

type State = Safe | Corrupt
type Node = { X: int; Y: int; Distance: int }

let render memBlock =
  [0 .. (Array2D.length2 memBlock) - 1]
  |> List.map (fun y -> memBlock[*,y])
  |> List.map (Array.map (function Safe -> "." | Corrupt -> "#"))
  |> List.map (String.concat "")
  |> String.concat Environment.NewLine

let buildGraph map =
  let dict = Dictionary<int*int,int>()
  Array2D.iiwhere (function Safe -> true | _ -> false) map
  |> Seq.iter (fun pos -> dict.Add(pos, Int32.MaxValue))
  dict

let dijkstras (graph: Dictionary<int*int,int>) =
  let updateDist (pos, dist) = if graph[pos] > dist then graph[pos] <- dist
  let adjacent pos =
    cardinals
    |> List.map (fun dir -> pos +. dir)
    |> List.filter (fun pos -> graph.ContainsKey pos)
  let rec loop (unvisited: seq<int*int>) =
    if Seq.isEmpty unvisited || Seq.all (fun pos -> graph[pos] = Int32.MaxValue) unvisited then ()
    else
      let this = Seq.minBy (fun pos -> graph[pos]) unvisited
      let unvisited = Seq.remove ((=) this) unvisited
      adjacent this
      |> Seq.filter (fun pos -> Seq.contains pos unvisited)
      |> Seq.map (fun pos -> pos, graph[this] + 1)
      |> Seq.iter updateDist
      loop unvisited
  graph[(0,0)] <- 0
  let unvisited = graph.Keys
  loop unvisited

let isSafePath (startPos: int*int) (endPos: int*int) (map: State[,]) =
  let mutable visited = Set.empty
  let isSafe pos = Array2D.tryItem map pos |> function Some Safe -> true | _ -> false
  // Note: Using `Seq` rather than `List` here is producing the incorrect result, maybe due to lazy evaluation?
  let rec impl pos =
    let safeNeighbors =
      cardinals
      |> List.map (fun dir -> pos +. dir)
      |> List.filter (fun pos -> Set.contains pos visited |> not)
      |> List.filter isSafe
    if Seq.contains endPos safeNeighbors then true
    else
      Seq.iter (fun pos -> visited <- Set.add pos visited) safeNeighbors
      safeNeighbors
      |> List.map impl
      |> List.exists id
  impl startPos

module Part1 =

  let run file bound iter =

    let memoryBlock = Array2D.init bound bound (fun _ _ -> Safe)

    System.IO.File.ReadAllText file
    |> parse parser
    |> List.take iter
    |> List.iter (fun (x,y) -> memoryBlock[x,y] <- Corrupt)

    let graph =
      memoryBlock
      |> buildGraph
    dijkstras graph

    graph[bound-1,bound-1]

  let runSample() = run sample 7 12
  let runInput() = run input 71 1024

module Part2 =

  let run file bound =

    let memoryBlock = Array2D.init bound bound (fun _ _ -> Safe)

    System.IO.File.ReadAllText file
    |> parse parser
    |> List.skipWhile (fun (x,y) ->
          memoryBlock[x,y] <- Corrupt
          isSafePath (0,0) (bound-1,bound-1) memoryBlock)
    |> List.head

  let runSample() = run sample 7
  let runInput() = run input 71

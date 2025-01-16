module AdventOfCode2024.Day20

open System.Collections.Generic
open Util
open Maze2D

let sample = $"{dataFolder}/sample/Day20.txt"
let input = $"{dataFolder}/actual/Day20.txt"

type CheatDir = Vert | Horiz

// Okay: I took a stab at just solving this by running Dijkstra's algorithm on variations of the maze, but the runtime is totally unrealistic and I am going to need to try a different approach to this problem.
// One thought is that we can find just the distance removed from a potential cheat, and see if it can skip more than 100 steps of the puzzle.
// For example, if we have a maze that looks like this:
// #####
// ..#..
// #.#.#
// #.#.#
// #.#.#
// #...#
// We can obviously see that performing a cheat on the second line will provide a big improvement to distance (and that each cheat along that penninsula will also provide an improvement).
// This can be determined without taking into account any of the rest of the puzzle, meaning that the runtime calculation of this will be much faster.
// At the most basic level, any cheat will create a loop and will provide a speed benefit of the size of the loop (minus 1-3?)
// So perhaps an effective algorithm would be to look for places that can generate a loop of a certain size by removing one wall segment.

// In order for a cheat to be viable, it must:
// - not be along an edge of the map
// - have empty spaces on opposite sides of it (left/right or up/down)

let isPotentialCheat (map: Space[,]) (pos: int*int) =
  [ (0,1); (0,-1); (1,0); (-1,0) ]
  |> List.map (fun dir -> Array2D.tryItem map (pos +. dir))
  |> function
      | [ Some NotWall; Some NotWall; _; _ ] -> Some Horiz
      | [ _; _; Some NotWall; Some NotWall ] -> Some Vert
      | _ -> None

let findPotentialCheats (map: Space[,]) =
  Array2D.iiwhere (function NotWall -> false | Wall -> true) map
  |> Seq.map (fun pos -> pos, isPotentialCheat map pos)
  |> Seq.choose (fun (pos, maybeDir) -> maybeDir |> function | Some x -> Some (pos, x) | None -> None)

/// Given a starting and ending position on the graph, find the shortest distance between them.
/// This distance will be the improvement that is possible by cheating here.
let cheatImprovement (graph: Dictionary<Position,Node>) (startPos, endPos) =
  let rec loop dist thesePoss visited =
    if Set.contains endPos thesePoss then dist
    else
      let nextPoss =
        thesePoss
        |> Set.map (fun pos -> graph[pos].AdjacentNodes |> Set.ofList)
        |> Set.unionMany
        |> fun next -> Set.difference next visited
      loop (dist+1) nextPoss (Set.union visited thesePoss)
  loop 0 (Set.singleton startPos) Set.empty - 2

module Part1 =

  let run file =
    let map: Space[,] =
      System.IO.File.ReadAllLines file
      |> parseMap
    // Timing: practically instantaneous to here
    let graph = parseGraph map
    // Timing: about one minute to here
    findPotentialCheats map
    // Timing: 1'20" to here
    |> Seq.map (fun (pos,dir) ->
          pos, match dir with
                | Horiz -> Position.OfInvertedTuple (pos+.(0,-1)), Position.OfInvertedTuple (pos+.(0,1))
                | Vert -> Position.OfInvertedTuple (pos+.(-1,0)), Position.OfInvertedTuple (pos+.(1,0)))
    |> Seq.map (fun (pos, se) -> pos, cheatImprovement graph se)
    |> Seq.filter (fun (pos,imp) -> imp >= 100)
    |> Seq.length
    // |> Seq.countBy snd
    // |> Seq.sortBy fst
    // |> Seq.iter (printfn "%A")

  let runSample() = run sample
  let runInput() = run input

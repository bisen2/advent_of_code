module AdventOfCode2024.Day16

open System
open System.Collections.Generic
open Plotly.NET
open Util
open Day16_Parsing

let sample = $"{dataFolder}/sample/Day16.txt"
let input = $"{dataFolder}/actual/Day16.txt"

let dijkstras (graph: Dictionary<int,Node>) =
  let start = graph.Values |> Seq.minBy (fun node -> node.X - node.Y)
  let dist (y1:int,x1:int) (y2,x2) = 1000 + Math.Abs(y2-y1) + Math.Abs(x2-x1)
  let updateDist (id, dist) = if graph[id].Distance > dist then graph[id].Distance <- dist
  let rec loop (unvisited: Node seq) =
    if Seq.isEmpty unvisited || Seq.all (fun node -> node.Distance = Int32.MaxValue) unvisited then ()
    else
      let thisNode = Seq.minBy (fun node -> node.Distance) unvisited
      let nowUnvisited = Seq.remove ((=) thisNode) unvisited
      do
        thisNode.AdjacentNodes
        |> Seq.choose (fun id -> Seq.tryFind (fun node -> node.Id = id) nowUnvisited)
        |> Seq.map (fun node -> node.Id, thisNode.Distance + dist thisNode.Position node.Position)
        |> Seq.iter updateDist
      loop nowUnvisited
  graph[start.Id].Distance <- 0
  let unvisited = graph.Values
  loop unvisited

let shortestPaths (graph: Dictionary<int,Node>) =
  let dist (y1:int,x1:int) (y2,x2) = 1000 + Math.Abs(y2-y1) + Math.Abs(x2-x1)
  let rec impl (endId: int) (soFar: int list) (currId: int) (currentDist: int) =
    // if we get to the end along an optimal path, return it
    if currId = endId && currentDist = graph[endId].Distance then [soFar]
    // if we get to the end along a non-optimal path, discard it.
    elif currId = endId then []
    else
      graph[currId].AdjacentNodes
      |> List.map (fun id -> id, currentDist + dist graph[currId].Position graph[id].Position)
      |> List.filter (fun (id, dist) -> graph[id].Distance = dist)
      |> List.map (fun (id, dist) -> impl endId (id::soFar) id dist)
      |> List.map List.concat
  let startNode = graph.Values |> Seq.minBy (fun node -> node.X - node.Y)
  let endNode = graph.Values |> Seq.maxBy (fun node -> node.X - node.Y)
  impl endNode.Id [] startNode.Id 0

module Part1 =

  let run file =
    let graph = new Dictionary<int,Node>()

    System.IO.File.ReadAllLines file
    |> Seq.map (parse parser)
    |> buildGraph
    |> List.iter (fun node -> graph.Add(node.Id, node))

    dijkstras graph

    render graph.Values
    |> Chart.show

    graph.Values
    |> Seq.maxBy (fun node -> node.X - node.Y)
    |> _.Distance

  let runSample() = run sample
  let runInput() = run input - 1000

module Part2 =

  let findBetween (y1,x1) (y2,x2) =
    let normalize x = if x = 0 then 0 else x / Math.Abs x
    let (dy,dx) = normalize (y2-y1), normalize (x2-x1)
    if dy <> 0 && dx <> 0 then [] // discard segments that are diagonal
    else
      let dist = Math.Abs(y2-y1) + Math.Abs(x2-x1)
      [ 0 .. dist ]
      |> List.map (fun i -> y1 + (i*dy), x1 + (i*dx))

  let seatsOnPath (graph: Dictionary<int,Node>) path =
    let startNode = graph.Values |> Seq.minBy (fun node -> node.X - node.Y)
    // path |> List.append [startNode.Id]
    startNode.Id :: (List.rev path)
    |> List.pairwise
    |> List.map (fun (id1,id2) -> findBetween graph[id1].Position graph[id2].Position)
    |> List.concat
    |> List.distinct

  let optimalSeats graph =
    shortestPaths graph
    |> List.map (seatsOnPath graph)
    |> List.concat
    |> List.distinct

  let run file =
    let graph = new Dictionary<int,Node>()

    System.IO.File.ReadAllLines file
    |> Seq.map (parse parser)
    |> buildGraph
    |> List.iter (fun node -> graph.Add(node.Id, node))

    dijkstras graph

    let seats = optimalSeats graph
    let ys, xs = List.unzip seats
    let seatChart =
      Chart.Point (xs, List.map (fun y -> -y) ys, Text="Optimal Seat")
      |> Chart.withMarkerStyle (Size = 5, Color = Color.fromKeyword Black)

    [ render graph.Values
      seatChart ]
    |> Chart.combine
    |> Chart.show

    seats
    |> List.length

  let runSample() = run sample
  let runInput() = run input

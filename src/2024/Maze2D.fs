module AdventOfCode2024.Maze2D

open System
open System.Collections.Generic
open FParsec
open Util

type Position =
  { X: int; Y: int }
  member this.Add (x,y) = { X = this.X + x; Y = this.Y + y }
  member this.ToTuple = (this.X, this.Y)
  static member OfTuple (x,y) = { X=x; Y=y }
  static member OfInvertedTuple (y,x) = { X=x; Y=y }

type Node =
  { Pos: Position
    AdjacentNodes: list<Position>
    IsStart: bool
    IsEnd: bool
    mutable Distance: int }
  static member Default =
    { Pos = { X = 0; Y = 0}
      AdjacentNodes = []
      IsStart = false
      IsEnd = false
      Distance = Int32.MaxValue }

type Space = Wall | Empty | Start | End

/// `NotWall` is now equivalent to `Empty | Start | End`
let (|NotWall|Wall|) input =
  match input with
  | Empty | Start | End -> NotWall
  | Wall -> Wall

module Parsing =

  let parser =
    choice
      [ pchar '.' >>% Empty
        pchar '#' >>% Wall
        pchar 'S' >>% Start
        pchar 'E' >>% End ]
    |> many

  let buildAdjacencyList nodes node =
    { node with
        AdjacentNodes =
          cardinals
          |> List.map (fun dir -> node.Pos.Add dir)
          |> List.filter (fun pos -> Seq.exists (fun node -> node.Pos = pos) nodes) }

  let buildAllAdjacencyLists nodes = Seq.map (buildAdjacencyList nodes) nodes

  let buildGraph map =
    let startNode =
      Array2D.iiwhere ((=) Start) map
      |> Seq.map (fun (y,x) -> { Node.Default with Pos = { X=x; Y=y}; IsStart=true })
    let endNode =
      Array2D.iiwhere ((=) End) map
      |> Seq.map (fun (y,x) -> { Node.Default with Pos = { X=x; Y=y}; IsEnd=true })
    let otherNodes =
      Array2D.iiwhere ((=) Empty) map
      |> Seq.map (fun (y,x) -> { Node.Default with Pos = { X=x; Y=y } })
    Seq.concat [ startNode; endNode; otherNodes ]
    |> buildAllAdjacencyLists

let parseMap lines =
  Seq.map (parse Parsing.parser) lines
  |> array2D

let parseGraph map =
  let graph = new Dictionary<Position,Node>()

  Parsing.buildGraph map
  |> Seq.iter (fun node -> graph.Add(node.Pos, node))

  graph

// let parse lines =
//   let graph = new Dictionary<Position,Node>()

//   lines
//   |> Seq.map (parse Parsing.parser)
//   |> Parsing.buildGraph
//   |> Seq.iter (fun node -> graph.Add(node.Pos, node))

//   graph

let dijkstras (graph: Dictionary<Position,Node>) =
  let start = graph.Values |> Seq.find (fun node -> node.IsStart = true)
  let updateDist (pos, dist) = if graph[pos].Distance > dist then graph[pos].Distance <- dist
  let rec loop (unvisited: Node seq) =
    if Seq.isEmpty unvisited || Seq.all (fun node -> node.Distance = Int32.MaxValue) unvisited then ()
    else
      let thisNode = Seq.minBy (fun node -> node.Distance) unvisited
      let nowUnvisited = Seq.remove (fun node -> node.Pos = thisNode.Pos) unvisited
      thisNode.AdjacentNodes
      |> Seq.choose (fun pos -> Seq.tryFind (fun node -> node.Pos = pos) nowUnvisited)
      |> Seq.map (fun node -> node.Pos, thisNode.Distance + 1)
      |> Seq.iter updateDist
      loop nowUnvisited
  graph[start.Pos].Distance <- 0
  loop graph.Values

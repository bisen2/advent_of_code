module AdventOfCode2024.Day16_Parsing

open System
open FParsec
open Plotly.NET
open Plotly.NET.LayoutObjects
open AdventOfCode2024.Util

[<CustomEquality; CustomComparison>]
type Node =
  { Id: int
    X: int
    Y: int
    AdjacentNodes: int list
    mutable Distance: int }

  override this.GetHashCode() = this.Id.GetHashCode()
  override this.Equals other =
    match other with
    | :? Node as n -> n.Id = this.Id
    | _ -> false
  interface IEquatable<Node> with
    member this.Equals other = other.Id.Equals this.Id
  interface IComparable with
    member this.CompareTo other =
      match other with
      | :? Node as n -> (this :> IComparable<_>).CompareTo n
      | _ -> -1
  interface IComparable<Node> with
    member this.CompareTo other = other.Distance.CompareTo this.Distance

  static member Default =
    { Id = 0
      X = 0
      Y = 0
      AdjacentNodes = []
      Distance = Int32.MaxValue }

  member this.Position = (this.Y, this.X)

type Space = Wall | Empty

let parser : Parser<_,unit> =
  choice
    [ pchar '.' >>% Empty
      pchar '#' >>% Wall
      pchar 'S' >>% Empty
      pchar 'E' >>% Empty ]
  |> many

// A point on the map is NOT a node if it is empty, it has exactly two empty neighbors,
// and the empty neighbors are directly opposite of one another.
let isNode map pos =
  let above = Seq2.item map (pos +. (-1,0))
  let below = Seq2.item map (pos +. (1,0))
  let left = Seq2.item map (pos +. (0,-1))
  let right = Seq2.item map (pos +. (0,1))
  match above, below, left, right with
  | Empty, Empty, Wall, Wall -> false
  | Wall, Wall, Empty, Empty -> false
  | _ -> true

let findNodes map =
  Seq2.iiwhere ((=) Empty) map
  |> Seq.filter (isNode map)
  |> Seq.mapi (fun i (y,x) -> { Node.Default with Id=i; X=x; Y=y })
  |> Seq.toList

let incr (x,y) =
  let incr a = if a = 0 then 0 else a+1
  incr x, incr y

let rec lookDirForAdjacency map (nodes: Node list) (node: Node) pos dir =
  match Seq2.tryItem map pos with
  | None -> []
  | Some Wall -> []
  | Some Empty ->
      match nodes |> List.tryFind (fun node -> node.Position = pos) with
      | Some node -> node::(lookDirForAdjacency map nodes node (pos +. dir) dir)
      | None -> lookDirForAdjacency map nodes node (pos +. dir) dir

let buildAdjacencyList map nodes node =
  let adjacentIds =
    cardinals
    |> List.map (fun dir -> lookDirForAdjacency map nodes node (node.Position +. dir) dir)
    |> List.concat
    |> List.map (fun node -> node.Id)
  { node with AdjacentNodes = adjacentIds }

let buildAllAdjacencyLists map nodes = List.map (buildAdjacencyList map nodes) nodes

let buildGraph map = findNodes map |> buildAllAdjacencyLists map

let node graph id = Seq.find (fun node -> node.Id = id) graph

let renderNodes graph =
  let renderNode (node: Node) =
    $"Node {node.Id}: (x,y)=({node.X},{node.Y}), Adjacent nodes: {node.AdjacentNodes}, Distance: {node.Distance}"
  graph
  |> List.map renderNode
  |> String.concat Environment.NewLine

let render (graph: Node seq) =
  let xs = graph |> Seq.map (fun node -> node.X)
  let ys = graph |> Seq.map (fun node -> - node.Y)
  let ids = graph |> Seq.map (fun node -> sprintf "Node %d. Distance %d" node.Id node.Distance)
  let distances = graph |> Seq.map (fun node -> node.Distance)
  let edges =
    graph
    |> Seq.map (fun node -> List.map (fun adjId -> node.Id, adjId) node.AdjacentNodes)
    |> Seq.concat
    |> Seq.map (fun (id1, id2) -> node graph id1, node graph id2)
    |> Seq.map (fun (n1,n2) ->
          Shape.init(
            ShapeType = StyleParam.ShapeType.Line,
            X0 = n1.X, X1 = n2.X, Y0 = -n1.Y, Y1 = -n2.Y,
            Opacity = 0.5,
            FillColor = Color.fromKeyword LightGrey))
  Chart.Point(
    xs, ys,
    MultiText = ids,
    MarkerColor = Color.fromColorScaleValues distances)
  |> Chart.withShapes edges
  |> Chart.withMarkerStyle (ShowScale = true, Size = 10)
  // |> Chart.show

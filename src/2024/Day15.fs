module AdventOfCode2024.Day15

open FParsec
open Util

let sample = $"{dataFolder}/sample/Day15.txt"
let input = $"{dataFolder}/actual/Day15.txt"

type Space = Empty | Robot | Box | Wall | LeftBox | RightBox

type Move =
  Up | Down | Left | Right
  member this.Indices =
    match this with
    | Up -> (-1,0)
    | Down -> (1,0)
    | Left -> (0,-1)
    | Right -> (0,1)
  static member (+) (m: Move, pos) =
    pos +. m.Indices
  static member (+) (a: Move, b: Move) =
    a.Indices +. b.Indices

let mapLineParser =
  let choices =
    choice
      [ pchar '.' >>% Empty
        pchar '@' >>% Robot
        pchar 'O' >>% Box
        pchar '#' >>% Wall
        pchar '[' >>% LeftBox
        pchar ']' >>% RightBox ]
  many choices .>> newline

let moveLineParser =
  let choices =
    choice
      [ pchar '^' >>% Up
        pchar 'v' >>% Down
        pchar '<' >>% Left
        pchar '>' >>% Right ]
  many choices

let parser =
  tuple2
    (manyTill mapLineParser newline |>> array2D)
    (sepBy moveLineParser newline |>> List.concat)

// Note: There is probably a nice way to combine both the canMove and generateMove functions into one
// but I don't really want to bother with backtracking if a move cannot be made. The performance is
// fine with an individual check and then move, so I am happy to leave it be.

let rec canMove (map: Space[,]) pos (dir: Move) =
  match Array2D.item pos map, dir with
  | Empty, _ -> true
  | Wall, _ -> false
  | Robot, _ -> canMove map (dir + pos) dir
  | Box, _ -> canMove map (dir + pos) dir
  | LeftBox, Left -> canMove map (dir + pos) dir
  | LeftBox, Right -> canMove map (dir + Right +. pos) dir
  | LeftBox, Up
  | LeftBox, Down ->
      let leftCanMove = canMove map (dir + pos) dir
      let rightCanMove = canMove map (dir + Right +. pos) dir
      leftCanMove && rightCanMove
  | RightBox, Right -> canMove map (dir + pos) dir
  | RightBox, Left -> canMove map (dir + Left +. pos) dir
  | RightBox, Up
  | RightBox, Down ->
      let leftCanMove = canMove map (dir + Left +. pos) dir
      let rightCanMove = canMove map (dir + pos) dir
      leftCanMove && rightCanMove

let rec generateMoves (map: Space[,]) pos (dir: Move) =
  let (y, x) = pos
  let (ny, nx) = dir + pos
  match Array2D.item (dir + pos) map with
  | Empty ->
      map[ny, nx] <- Array2D.item pos map
      map[y, x] <- Empty
  | Box ->
      generateMoves map (dir + pos) dir
      map[ny, nx] <- Array2D.item pos map
      map[y, x] <- Empty
  | LeftBox ->
      generateMoves map (dir + Right +. pos) dir
      generateMoves map (dir + pos) dir
      map[ny, nx] <- Array2D.item pos map
      map[y,x] <- Empty
  | RightBox ->
      generateMoves map (dir + Left +. pos) dir
      generateMoves map (dir + pos) dir
      map[ny,nx] <- Array2D.item pos map
      map[y, x] <- Empty
  | Wall -> failwith "Tried to move into a wall."
  | Robot -> failwith "Tried to move into a robot."

let moveFrom map pos dir =
  if canMove map pos dir then
    generateMoves map pos dir
    dir + pos
  else pos

let step map (y,x) move =
  if Array2D.get map y x <> Robot then
    failwithf "Error: Lost robot positioning. Expected it at (x,y)=%A on map %A" (x,y) map
  moveFrom map (y,x) move

let render map =
  let spaceToChar = function
    | Empty -> '.'
    | Wall -> '#'
    | Robot -> '@'
    | Box -> 'O'
    | LeftBox -> '['
    | RightBox -> ']'
  let charArray = Array2D.map spaceToChar map
  [ 0 .. charArray[*,0].Length - 1 ]
  |> List.map (fun y -> System.String.Concat charArray[y,*])
  |> String.concat System.Environment.NewLine

module Part1 =

  let run file =
    let map, moves =
      System.IO.File.ReadAllText file
      |> parse parser
    let startingRobotPosition = Array2D.pick ((=) Robot) map
    moves
    |> Seq.fold (step map) startingRobotPosition
    |> ignore
    // render map
    // |> printfn "%s"
    map
    |> Array2D.iiwhere ((=) Box)
    |> Seq.sumBy (fun (y,x) -> (100*y) + x)

  let runSample() = run sample
  let runInput()  = run input

module Part2 =

  let convertSpace = function
    | Wall -> [| Wall; Wall |]
    | Box -> [| LeftBox; RightBox |]
    | Empty -> [| Empty; Empty |]
    | Robot -> [| Robot; Empty |]
    | LeftBox -> [| LeftBox |]
    | RightBox -> [| RightBox |]

  let convert (map: Space[,]) =
    [| 0 .. map[*,0].Length - 1 |]
    |> Array.map (fun y -> map[y,*])
    |> Array.map (Array.collect convertSpace)
    |> array2D

  let run file =
    let originalMap, moves =
      System.IO.File.ReadAllText file
      |> parse parser
    let map = convert originalMap
    let startingRobotPosition = Array2D.pick ((=) Robot) map
    moves
    |> Seq.fold (step map) startingRobotPosition
    |> ignore
    // render map
    // |> printfn "%s"
    map
    |> Array2D.iiwhere ((=) LeftBox)
    |> Seq.sumBy (fun (y,x) -> (100*y) + x)

  let runSample() = run sample
  let runInput() = run input

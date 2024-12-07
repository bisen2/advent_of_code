module AdventOfCode2024.Day6

open Util

let sample = $"{dataFolder}/sample/Day6.txt"
let input = $"{dataFolder}/actual/Day6.txt"

type Direction = North | East | South | West
type Guard = { Position: int*int; Direction: Direction }

let getObstacles map = Seq2.iiwhere ((=) '#') map

let getGuard map =
  [ Seq2.iiwhere ((=) '^') map |> Seq.map (fun (y,x) -> y, x, North)
    Seq2.iiwhere ((=) '>') map |> Seq.map (fun (y, x) -> y, x, East)
    Seq2.iiwhere ((=) 'v') map |> Seq.map (fun (y, x) -> y, x, South)
    Seq2.iiwhere ((=) '<') map |> Seq.map (fun (y, x) -> y, x, West) ]
  |> Seq.concat
  |> Seq.head

let step obstacles guard =
  match guard with
  | y, x, North -> if Seq.contains (y-1, x) obstacles then (y, x, East) else (y-1, x, North)
  | y, x, East  -> if Seq.contains (y, x+1) obstacles then (y, x, South) else (y, x+1, East)
  | y, x, South -> if Seq.contains (y+1, x) obstacles then (y, x, West) else (y+1, x, South)
  | y, x, West  -> if Seq.contains (y, x-1) obstacles then (y, x, North) else (y, x-1, West)

let runGame (map: char seq seq) =
  let xlim = Seq.length (Seq.head map)
  let ylim = Seq.length map
  let obstacles = Seq2.iiwhere ((=) '#') map
  let startGuard = getGuard map

  let rec loop (y,x,dir) soFar =
    if x >= xlim || x < 0 || y >= ylim || y < 0 || Seq.contains (y,x,dir) soFar then
      soFar
    else
      let update = step obstacles (y,x,dir)
      loop update ((y,x,dir)::soFar)

  loop startGuard []

module Part1 =

  let run =
    System.IO.File.ReadAllLines
    >> Seq.map (Seq.map id) // this is silly, but needed for type inference
    // >> List.ofSeq
    >> runGame
    >> Seq.distinctBy (fun (y,x,_) -> y,x)
    >> Seq.length

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  type Result = OffMap | Loop

  let loop (xlim, ylim) obstacles start =
    let rec loop (y,x,dir) soFar =
      if x >= xlim || x < 0 || y >= ylim || y < 0 then OffMap
      elif Seq.contains (y,x,dir) soFar then Loop
      else
        let update = step obstacles (y,x,dir)
        loop update ((y,x,dir)::soFar)
    loop start []

  // we only need to check the path the guard will take without additional obstructions
  let run file =
    let map = System.IO.File.ReadAllLines file |> Seq.map (Seq.map id)
    let existingObstacles = Seq2.iiwhere ((=) '#') map
    let start = getGuard map
    let xlim = Seq.length (Seq.head map)
    let ylim = Seq.length map
    // Get path with no added obstacles, any new obstacles will only help along this path
    runGame map
    |> Seq.map (fun (y,x,_) -> y,x)
    |> Seq.distinct // no need to check positions we already have
    // Try adding an obstacle at each point
    |> Seq.map (fun x -> x, seq { x; yield! existingObstacles })// x::existingObstacles)
    // See how it changes the outcome
    |> Seq.map (fun (x, obstacles) -> x, loop (xlim, ylim) obstacles start)
    // Count up number of ways to achieve a loop outcome
    |> Seq.filter (function _, Loop -> true | _, OffMap -> false)
    |> Seq.length

  let runSample() = run sample
  let runInput() = run input

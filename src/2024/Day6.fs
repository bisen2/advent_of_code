module AdventOfCode2024.Day6

open Util

let sample = $"{dataFolder}/sample/Day6.txt"
let input = $"{dataFolder}/actual/Day6.txt"

type Direction = North | East | South | West
type Guard = { Position: int*int; Direction: Direction }

let getObstacles map = List2.iiwhere ((=) '#') map

let getGuard map =
  [ List2.iiwhere ((=) '^') map |> List.map (fun (y,x) -> y, x, North)
    List2.iiwhere ((=) '>') map |> List.map (fun (y, x) -> y, x, East)
    List2.iiwhere ((=) 'v') map |> List.map (fun (y, x) -> y, x, South)
    List2.iiwhere ((=) '<') map |> List.map (fun (y, x) -> y, x, West) ]
  |> List.concat
  |> List.head

let step obstacles guard =
  match guard with
  | y, x, North -> if List.contains (y-1, x) obstacles then (y, x, East) else (y-1, x, North)
  | y, x, East  -> if List.contains (y, x+1) obstacles then (y, x, South) else (y, x+1, East)
  | y, x, South -> if List.contains (y+1, x) obstacles then (y, x, West) else (y+1, x, South)
  | y, x, West  -> if List.contains (y, x-1) obstacles then (y, x, North) else (y, x-1, West)

let runGame (map: char list list) =
  let xlim = List.length (List.head map)
  let ylim = List.length map
  let obstacles = List2.iiwhere ((=) '#') map
  let startGuard = getGuard map

  let rec loop (y,x,dir) soFar =
    if x >= xlim || x < 0 || y >= ylim || y < 0 || List.contains (y,x,dir) soFar then
      soFar
    else
      let update = step obstacles (y,x,dir)
      loop update ((y,x,dir)::soFar)

  loop startGuard []

module Part1 =

  let run =
    System.IO.File.ReadAllLines
    >> List.ofSeq
    >> List.map List.ofSeq
    >> runGame
    >> List.distinctBy (fun (y,x,_) -> y,x)
    >> List.length

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  type Result = OffMap | Loop

  let loop (xlim, ylim) obstacles start =
    let rec loop (y,x,dir) soFar =
      if x >= xlim || x < 0 || y >= ylim || y < 0 then OffMap
      elif List.contains (y,x,dir) soFar then Loop
      else
        let update = step obstacles (y,x,dir)
        loop update ((y,x,dir)::soFar)
    loop start []

  // we only need to check the path the guard will take without additional obstructions
  let run file =
    let map = System.IO.File.ReadAllLines file |> List.ofSeq |> List.map List.ofSeq
    let existingObstacles = List2.iiwhere ((=) '#') map
    let start = getGuard map
    let xlim = List.length (List.head map)
    let ylim = List.length map
    // Get path with no added obstacles, any new obstacles will only help along this path
    runGame map
    |> List.map (fun (y,x,_) -> y,x)
    |> List.distinct // no need to check positions we already have
    // Try adding an obstacle at each point
    |> List.map (fun x -> x, x::existingObstacles)
    // See how it changes the outcome
    |> List.map (fun (x, obstacles) -> x, loop (xlim, ylim) obstacles start)
    // Count up number of ways to achieve a loop outcome
    |> List.filter (function _, Loop -> true | _, OffMap -> false)
    |> List.length

  let runSample() = run sample
  let runInput() = run input

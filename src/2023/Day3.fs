module AdventOfCode2023.Day3

open Util

type NumberPosition = { Y: int; XStart: int; XEnd: int }

let isDigit = System.Char.IsDigit
let isNotDigit = isDigit >> not
let isSymbol c = not (isDigit c || c = '.')

let findWhere cond schematic =
  let findInLine = List.choosei (fun i x -> if cond x then Some i else None)
  schematic
  |> List.mapi (fun y xs -> findInLine xs |> List.map (fun x -> (x,y)))
  |> List.concat

/// Given a position of a digit, find the bounds of its containing number
let getNumBounds (schematic: char list list) (x,y) =
  let xStart =
    List.tryFindIndexBack isNotDigit (schematic[y][..x])
    |> function Some xStart -> xStart + 1 | None -> 0
  let xEnd =
    List.tryFindIndex isNotDigit (schematic[y][x..])
    |> function Some xEnd -> x + xEnd - 1 | None -> (List.length schematic[y]) - 1
  { Y = y; XStart = xStart; XEnd = xEnd }

module Lookers =

  let lookLeft (schematic: char list list) (x,y) =
    if x > 0 && isDigit (schematic[y][x-1]) then
      Some (getNumBounds schematic (x-1, y))
    else None

  let lookRight (schematic: char list list) (x,y) =
    if x + 1 < List.length schematic[y] && isDigit (schematic[y][x+1]) then
      Some (getNumBounds schematic (x+1, y))
    else None

  let lookAbove (schematic: char list list) (x,y) =
    if y < List.length schematic && isDigit (schematic[y+1][x]) then
      Some (getNumBounds schematic (x, y+1))
    else None

  let lookUpperLeft (schematic: char list list) (x,y) =
    if y < List.length schematic && x > 0 && isDigit (schematic[y+1][x-1]) then
      Some (getNumBounds schematic (x-1, y+1))
    else None

  let lookUpperRight (schematic: char list list) (x,y) =
    if y < List.length schematic && x <> List.length schematic[y+1] && isDigit (schematic[y+1][x+1]) then
      Some (getNumBounds schematic (x+1, y+1))
    else None

  let lookBelow (schematic: char list list) (x, y) =
    if y > 0 && isDigit (schematic[y-1][x]) then
      Some (getNumBounds schematic (x, y-1))
    else None

  let lookLowerLeft (schematic: char list list) (x,y) =
    if y > 0 && x > 0 && isDigit (schematic[y-1][x-1]) then
      Some (getNumBounds schematic (x-1, y-1))
    else None

  let lookLowerRight (schematic: char list list) (x,y) =
    if y > 0 && x < List.length schematic[y-1] && isDigit (schematic[y-1][x+1]) then
      Some (getNumBounds schematic (x+1, y-1))
    else None

open Lookers

let findNumbersNear schematic (x,y) =
  [ lookLeft; lookRight; lookAbove; lookUpperLeft; lookUpperRight; lookBelow; lookLowerLeft; lookLowerRight ]
  |> List.map (fun f -> f schematic (x,y))
  |> List.choose id
  |> List.distinct
  |> List.map (fun pos -> schematic[pos.Y][pos.XStart .. pos.XEnd])
  |> List.map (List.toArray >> System.String)
  |> List.map (System.Int32.Parse)

module Part1 =

  let findSymbols = findWhere isSymbol

  let run file =
    let schematic = System.IO.File.ReadAllLines file |> Seq.toList |> List.map (Seq.toList) |> List.rev
    schematic
    |> findSymbols
    |> List.map (findNumbersNear schematic)
    |> List.concat
    |> List.sum

  let runSample() = run $"{dataFolder}/sample/Day3.txt"
  let runInput() = run $"{dataFolder}/actual/Day3.txt"

module Part2 =

  let findStars = findWhere ((=) '*')

  let findGearRatios schematic =
    findStars schematic
    |> List.map (findNumbersNear schematic)
    |> List.filter (fun xs -> List.length xs = 2)
    |> List.map (fun xs -> xs[0] * xs[1])

  let run =
    System.IO.File.ReadAllLines
    >> Seq.toList
    >> List.map (Seq.toList)
    >> List.rev
    >> findGearRatios
    >> List.sum

  let runSample() = run $"{dataFolder}/sample/Day3.txt"
  let runInput() = run $"{dataFolder}/actual/Day3.txt"

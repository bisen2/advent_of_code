module AdventOfCode2024.Day5

open FParsec
open Util

let sample = $"{dataFolder}/sample/Day5.txt"
let input = $"{dataFolder}/actual/Day5.txt"

let orderingParser = pint32 .>> pstring "|" .>>. pint32 .>> newline
let updateParser = sepBy pint32 (pstring ",") .>> newline

let parser: Parser<_,unit> = manyTill orderingParser newline .>>. many updateParser

let checkUpdateRule update (before,after) =
  match List.tryFindIndex ((=) before) update, List.tryFindIndex ((=) after) update with
  | Some iBefore, Some iAfter -> iBefore < iAfter
  | _ -> true

let checkUpdate rules update =
  rules
  |> List.map (checkUpdateRule update)
  |> List.none not

module Part1 =

  let run file =
    let (rules,updates) = System.IO.File.ReadAllText file |> parse parser
    updates
    |> List.filter (checkUpdate rules)
    |> List.sumBy List.middle

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let sorter rules x y =
    if List.contains (x,y) rules then -1
    elif List.contains (y,x) rules then 1
    else 0

  let run file =
    let (rules, updates) = System.IO.File.ReadAllText file |> parse parser
    updates
    |> List.filter (checkUpdate rules >> not)
    |> List.map (List.sortWith (sorter rules))
    |> List.sumBy List.middle

  let runSample() = run sample
  let runInput() = run input

module AdventOfCode2024.Day5

open FParsec
open Util

let sample = $"{dataFolder}/sample/Day5.txt"
let input = $"{dataFolder}/actual/Day5.txt"

let orderingParser = pint32 .>> pstring "|" .>>. pint32 .>> newline
let updateParser = sepBy pint32 (pstring ",") .>> newline

let parser: Parser<_,unit> = manyTill orderingParser newline .>>. many updateParser

let checkUpdateRule update (before,after) =
  match Seq.tryFindIndex ((=) before) update, Seq.tryFindIndex ((=) after) update with
  | Some iBefore, Some iAfter -> iBefore < iAfter
  | _ -> true

let checkUpdate rules update =
  rules
  |> Seq.map (checkUpdateRule update)
  |> Seq.none not

module Part1 =

  let run file =
    let (rules,updates) = System.IO.File.ReadAllText file |> parse parser
    updates
    |> Seq.filter (checkUpdate rules)
    |> Seq.sumBy Seq.middle

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let sorter rules x y =
    if Seq.contains (x,y) rules then -1
    elif Seq.contains (y,x) rules then 1
    else 0

  let run file =
    let (rules, updates) = System.IO.File.ReadAllText file |> parse parser
    updates
    |> Seq.filter (checkUpdate rules >> not)
    |> Seq.map (Seq.sortWith (sorter rules))
    |> Seq.sumBy Seq.middle

  let runSample() = run sample
  let runInput() = run input

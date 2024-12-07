module AdventOfCode2024.Day7

open FParsec
open Util

let sample = $"{dataFolder}/sample/Day7.txt"
let input = $"{dataFolder}/actual/Day7.txt"

let parser = pint64 .>> pstring ": " .>>. sepBy pint64 (pstring " ")

module Part1 =

  let rec findOutcomes operands =
    match operands with
    | x::[] -> [x]
    | x::xs ->
        let outcomes = findOutcomes xs
        let adds = List.map ((+) x) outcomes
        let muls = List.map ((*) x) outcomes
        adds @ muls
    | [] -> []

  let canBeTrue operands goal = Seq.contains goal (findOutcomes operands)

  let run file =
    System.IO.File.ReadAllLines file
    |> List.ofSeq
    |> List.map (parse parser)
    |> List.filter (fun (goal, operands) -> canBeTrue (List.rev operands) goal)
    |> List.sumBy fst

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let concat b a = System.Int64.Parse $"{a}{b}"

  let rec findOutcomes operands =
    match operands with
    | x::[] -> [x]
    | x::xs ->
        let outcomes = findOutcomes xs
        let adds = List.map ((+) x) outcomes
        let muls = List.map ((*) x) outcomes
        let cons = List.map (concat x) outcomes
        adds @ muls @ cons
    | [] -> []

  let canBeTrue operands goal = Seq.contains goal (findOutcomes operands)

  let run =
    System.IO.File.ReadAllLines
    >> Seq.map (parse parser)
    >> Seq.filter (fun (goal, operands) -> canBeTrue (List.rev operands) goal)
    >> Seq.sumBy fst

  let runSample() = run sample
  let runInput() = run input

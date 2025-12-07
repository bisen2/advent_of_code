module AdventOfCode2025.Day5

open FParsec
open Util

let sampleFile = $"{dataFolder}/sample/Day5.txt"
let inputFile = $"{dataFolder}/actual/Day5.txt"

type Range =
  { Lower: int64
    Upper: int64 }
  static member contains x range = x >= range.Lower && x <= range.Upper
  member this.Contains x = Range.contains x this
  static member OfTuple (lower, upper) = { Lower = lower; Upper = upper }

let rangeParser = pint64 .>> pchar '-' .>>. pint64 |>> Range.OfTuple
let parser =
  many (rangeParser .>> newline)
  .>> newline
  .>>. many (pint64 .>> newline)

let isInRanges = Range.contains >> Seq.exists

let condenseRanges: List<Range> -> List<Range> =
  let folder (acc: List<Range>) next =
    match acc with
    | [] -> [ next ]
    | current :: closed ->
        match current.Contains next.Lower, current.Contains next.Upper with
        | true, true -> current :: closed
        | true, false -> { current with Upper = next.Upper } :: closed
        | false, _ -> next :: current :: closed
  Seq.sortBy _.Lower
  >> Seq.fold folder []

module Part1 =

  let run =
    System.IO.File.ReadAllText
    >> parse parser
    >> fun (ranges, vals) -> Seq.filter (fun x -> x |> isInRanges <| ranges) vals
    >> Seq.length

  let runSample() = run sampleFile
  let runInput() = run inputFile

module Part2 =

  let run =
    System.IO.File.ReadAllText
    >> parse parser
    >> fst
    >> condenseRanges
    >> Seq.map (fun r -> r.Upper - r.Lower + 1L)
    >> Seq.sum

  let runSample() = run sampleFile
  let runInput() = run inputFile

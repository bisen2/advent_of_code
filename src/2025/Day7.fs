module AdventOfCode2025.Day7

open FParsec
open Util

let sampleFile = $"{dataFolder}/sample/Day7.txt"
let inputFile = $"{dataFolder}/actual/Day7.txt"

type Space =
  Tachyon | Empty | Splitter
  static member Render space =
    match space with
    | Tachyon -> "|"
    | Empty -> "."
    | Splitter -> "^"

let stepWithSplitCounter (manifold, splits) i =
  let mutable splits = splits
  let currIndices =
    // Get indices of tachyons in the previous row
    List.item (i-1) manifold
    |> List.indexed
    |> List.filter (snd >> function | Tachyon -> true | _ -> false)
    |> List.map fst
    // Convert to indices in the current row
    |> List.collect (fun j -> manifold[i][j] |> function Empty | Tachyon -> [j] | Splitter -> splits <- splits + 1; [j+1; j-1])
  let updatedCurr = manifold[i] |> List.mapi (fun i x -> if List.contains i currIndices then Tachyon else x )
  List.updateAt i updatedCurr manifold, splits

type Tachyon =
  { Position: int; Paths: int64 }
  static member ofStartRow row = { Position = List.findIndex (function Tachyon -> true | _ -> false) row; Paths = 1 }

let stepWithPathCounter (manifold: List<List<_>>) tachyons i =
  tachyons
  |> List.collect (fun tach -> manifold[i][tach.Position] |> function Empty | Tachyon -> [tach] | Splitter -> [ {tach with Position = tach.Position - 1}; {tach with Position = tach.Position + 1} ])
  |> List.groupBy _.Position
  |> List.map (fun (pos, tachs) -> { Position = pos; Paths = tachs |> List.sumBy _.Paths })

let renderManifold =
  List.map (List.map Space.Render >> String.concat "")
  >> String.concat System.Environment.NewLine

module Parsing =
  let spaceParser : Parser<_,unit> =
    let pTachyon = pchar 'S' >>% Tachyon
    let pEmpty = pchar '.' >>% Empty
    let pSplitter = pchar '^' >>% Splitter
    pTachyon <|> pEmpty <|> pSplitter

  let manifoldParser : Parser<_, unit> =
    let lineParser = many1 spaceParser .>> newline
    many lineParser .>> eof

module Part1 =

  let run =
    System.IO.File.ReadAllText
    >> parse Parsing.manifoldParser
    >> fun manifold -> manifold, List.length manifold
    >> fun (manifold, steps) ->
          List.fold stepWithSplitCounter (manifold, 0) [1..steps-1]
    >> snd
    // >> fst >> renderManifold

  let runSample() = run sampleFile
  let runInput() = run inputFile

module Part2 =

  let run =
    System.IO.File.ReadAllText
    >> parse Parsing.manifoldParser
    >> fun manifold -> manifold, List.length manifold, [Tachyon.ofStartRow manifold[0]]
    >> fun (manifold, steps, start) ->
        List.fold (stepWithPathCounter manifold) start [1..steps-1]
    >> List.sumBy _.Paths

  let runSample() = run sampleFile
  let runInput() = run inputFile

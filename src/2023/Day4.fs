module AdventOfCode2023.Day4

open FParsec
open Util

let cardParser : Parser<_,unit> =
  pstring "Card" >>. spaces >>. pint32 .>> pstring ":" .>> spaces
  .>>. manyTill (spaces >>. pint32 .>> spaces) (pstring "|")
  .>>. many (spaces >>. pint32 .>> spaces)

let matches (winners, ours) =
  Set.intersect (Set.ofList winners) (Set.ofList ours)
  |> Set.count

module Part1 =

  let points matches = pown 2 (matches - 1)

  let run =
    System.IO.File.ReadAllLines
    >> Seq.map (parse cardParser)
    >> Seq.map (fun ((_,winners),ours) -> matches (winners,ours))
    >> Seq.sumBy points

  let runSample() = run $"{dataFolder}/sample/Day4.txt"
  let runInput() = run $"{dataFolder}/actual/Day4.txt"

module Part2 =

  let makeCopies (id, numbers) =
    let numMatches = matches numbers
    if numMatches < 1 then
      []
    else
      [ id + 1 .. id + numMatches ]

  let rec processWins soFar toDo (original: Map<int, int list * int list>) =
    match toDo with
    | this::rest ->
        let numMatches = matches original[this]
        if numMatches < 1 then
          processWins (this::soFar) rest original
        else
          processWins (this::soFar) ([this+1 .. this+numMatches] @ rest) original
    | [] -> soFar

  let run =
    System.IO.File.ReadAllLines
    >> Seq.map (parse cardParser)
    >> Seq.map (fun ((id,winners), ours) -> id, (winners, ours))
    >> Map
    >> function m -> processWins [] (m.Keys |> Seq.toList) m
    >> List.length

  let runSample() = run $"{dataFolder}/sample/Day4.txt"
  let runInput() = run $"{dataFolder}/actual/Day4.txt"

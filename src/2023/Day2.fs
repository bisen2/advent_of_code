module AdventOfCode2023.Day2

open FParsec
open Util

type ColorPull = Red of int | Green of int | Blue of int

let colorPullParser =
  choice
    [ pint32 .>> pstring " red" |> attempt |>> Red
      pint32 .>> pstring " green" |> attempt |>> Green
      pint32 .>> pstring " blue" |> attempt |>> Blue ]

let roundParser = sepBy colorPullParser (pstring ", ")

let gameParser = pstring "Game " >>. pint32 .>> pstring ": " .>>. (sepBy roundParser (pstring "; "))

module Part1 =

  let isPullImpossible = function
    | Red x when x > 12 -> true
    | Green x when x > 13 -> true
    | Blue x when x > 14 -> true
    | _ -> false

  let isGamePossible =
    List.exists (fun round -> List.exists isPullImpossible round)
    >> not

  let run =
    System.IO.File.ReadAllLines
    >> Seq.toList
    >> List.map (parse gameParser)
    >> List.sumBy (fun (id,rounds) -> if isGamePossible rounds then id else 0)

  let runSample() = run $"{dataFolder}/sample/Day2.txt"
  let runInput() = run $"{dataFolder}/actual/Day2.txt"

module Part2 =

  let findMaxPulled (game: ColorPull list list) =
    let pickMax picker = List.map (fun round -> List.choose picker round |> List.tryMax) game |> List.max
    ( pickMax (function Red x -> Some x | _ -> None),
      pickMax (function Green x -> Some x | _ -> None),
      pickMax (function Blue x -> Some x | _ -> None) )

  let run =
    System.IO.File.ReadAllLines
    >> Seq.toList
    >> List.map (parse gameParser)
    >> List.map (fun (id,rounds) -> findMaxPulled rounds)
    >> List.sumBy (function (Some r, Some g, Some b) -> r*b*g | _ -> 0)

  let runSample() = run $"{dataFolder}/sample/Day2.txt"
  let runInput() = run $"{dataFolder}/actual/Day2.txt"

module AdventOfCode2025.Day6

open FParsec
open Util

let sampleFile = $"{dataFolder}/sample/Day6.txt"
let inputFile = $"{dataFolder}/actual/Day6.txt"

type Operation = Addition | Multiplication

type ParsedNumber =
  Number of int64 | Space
  static member Render = function Number n -> n.ToString() | Space -> "_"

module Parsing =
    let pSpaces = pchar ' ' |> many |>> ignore
    let pNum = pSpaces >>. pint64 .>> pSpaces
    let pNumRow = many pNum .>> newline
    let pAddition = pchar '+' >>% Addition
    let pMultiplication = pchar '*' >>% Multiplication
    let pOperation = pSpaces >>. pAddition <|> pMultiplication .>> pSpaces
    let pOpRow = many pOperation .>> newline
    let parser = many pNumRow .>>. pOpRow
    let pChar = digit |>> string |>> System.Int64.Parse |>> Number <|> (pchar ' ' >>% Space)
    let pCharRow = many pChar .>> newline

let solveProblem (op, inputs) =
  match op with
  | Addition -> List.fold (+) 0L inputs
  | Multiplication -> List.fold (*) 1L inputs

module Part1 =
  open Parsing

  let parser = many pNumRow .>>. pOpRow

  let run =
    System.IO.File.ReadAllText
    >> parse Parsing.parser
    >> fun (numRows, opRow) -> List.transpose numRows |> List.rev |> List.zip opRow
    >> List.map solveProblem
    >> List.sum

  let runSample() = run sampleFile
  let runInput() = run inputFile

module Part2 =
  open Parsing

  let parser = many pCharRow .>>. pOpRow

  let buildNumber =
    List.map (function Number x -> x.ToString() | Space -> "")
    >> String.concat ""
    >> function "" -> Space | x -> System.Int64.Parse x |> Number

  let buildInputs numbers =
    let folder inputs number =
      match number, inputs with
      | Space, [] -> [[]]
      | Space, _ -> [] :: inputs
      | Number n, [] -> [[n]]
      | Number n, curr::rest -> (n::curr) :: rest
    List.fold folder [] numbers

  let run =
    System.IO.File.ReadAllText
    >> parse parser
    >> fun (numRows, opRow) ->
        numRows
        |> List2D.padToSquare Space
        |> List2D.transpose
        |> List.rev
        |> List.map buildNumber
        |> buildInputs
        |> List.rev
        |> List.zip opRow
        |> List.map solveProblem
        |> List.sum

  let runSample() = run sampleFile
  let runInput() = run inputFile

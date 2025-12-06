module AdventOfCode2025.Day2

open FParsec
open Util

let sampleFile = $"{dataFolder}/sample/Day2.txt"
let inputFile = $"{dataFolder}/actual/Day2.txt"

let rangeParser : Parser<_,unit> =
  pint64 .>> pchar '-' .>>. pint64

let dbParser : Parser<_, unit> =
  many (rangeParser .>> (pchar ',' >>% () <|> spaces))

let isRepeatedHalves (x: string) =
  x.Substring(0, x.Length/2) = x.Substring(x.Length/2)

let rec isRepeatingPattern xs pattern =
  if Seq.length xs < 2 * Seq.length pattern then false
  else
    let head = Seq.take (Seq.length pattern) xs
    let tail = Seq.skip (Seq.length pattern) xs
    match head, tail with
    | head, _ when Seq.equal head pattern |> not -> false
    | head, tail when Seq.equal head pattern && Seq.equal tail pattern -> true
    | _, tail -> isRepeatingPattern tail pattern

let isRepeating (xs: seq<_>) =
  [1 .. Seq.length xs / 2]
  |> Seq.map (fun len -> Seq.take len xs)
  |> Seq.map (isRepeatingPattern xs)
  |> Seq.exists id


module Part1 =
  let getInvalidIds (start, stop) =
    [start .. stop]
    |> List.map _.ToString()
    |> List.filter isRepeatedHalves

  let run =
    System.IO.File.ReadAllText
    >> parse dbParser
    >> Seq.map getInvalidIds
    >> Seq.concat
    >> Seq.map int64
    >> Seq.sum

  let runSample() = run sampleFile
  let runInput() = run inputFile

module Part2 =
  let getInvalidIds (start, stop) =
    [start .. stop]
    |> List.map _.ToString()
    |> List.filter isRepeating

  let run =
    System.IO.File.ReadAllText
    >> parse dbParser
    >> Seq.map getInvalidIds
    >> Seq.concat
    >> Seq.map int64
    >> Seq.sum

  let runSample() = run sampleFile
  let runInput() = run inputFile

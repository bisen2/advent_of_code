module AdventOfCode2024.Day13

open FParsec
open Util

let sample = $"{dataFolder}/sample/Day13.txt"
let input = $"{dataFolder}/actual/Day13.txt"

type Pair =
  { X: bigint; Y: bigint }
  static member ofTuple (x:int,y:int) = { X = bigint x; Y = bigint y }

let aParser = pstring "Button A: X+" >>. pint32 .>> pstring ", Y+" .>>. pint32 |>> Pair.ofTuple
let bParser = pstring "Button B: X+" >>. pint32 .>> pstring ", Y+" .>>. pint32 |>> Pair.ofTuple
let prizeParser = pstring "Prize: X=" >>. pint32 .>> pstring ", Y=" .>>. pint32 |>> Pair.ofTuple
let machineParser = aParser .>> newline .>>. bParser .>> newline .>>. prizeParser .>> newline
let parser = sepBy machineParser newline

let tokens (n,m) = (bigint 3)*n + m

let solve ((a,b),p) =
  let goal (n,m) =
    let goal1 = (n*a.X) + (m*b.X) = p.X
    let goal2 = (n*a.Y) + (m*b.Y) = p.Y
    goal1 && goal2
  let m = ((p.X*a.Y) - (p.Y*a.X)) / ((b.X*a.Y) - (b.Y*a.X))
  let n = (p.X - (m*b.X)) / a.X
  if goal (n,m) then Some (n,m)
  else None

module Part1 =

  let run =
    System.IO.File.ReadAllText
    >> parse parser
    >> Seq.map (solve)
    >> Seq.choose id
    >> Seq.sumBy tokens

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let convert ((a,b),p) = (a,b), { X = p.X + bigint 10000000000000L; Y = p.Y + bigint 10000000000000L }

  let run =
    System.IO.File.ReadAllText
    >> parse parser
    >> Seq.map (convert >> solve)
    >> Seq.choose id
    >> Seq.sumBy tokens

  let runSample() = run sample
  let runInput() = run input

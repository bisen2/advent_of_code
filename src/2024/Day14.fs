module AdventOfCode2024.Day14

open FParsec
open Util

let sample = $"{dataFolder}/sample/Day14.txt"
let input = $"{dataFolder}/actual/Day14.txt"

type Pair =
  { X: int; Y: int }

  static member ofTuple (x,y) = { X = x; Y = y }
  static member Zero = { X = 0; Y = 0 }

  member this.translate (pair: Pair) = { X = this.X + pair.X; Y = this.Y + pair.Y }

type Quadrant = NW | NE | SW | SE | Boundary

type Robot =
  { Pos: Pair; Vel: Pair }

  static member ofTuple (pos, vel) = { Pos = pos; Vel = vel }
  static member Zero = { Pos = Pair.Zero; Vel = Pair.Zero }

  member this.Move (bounds: Pair) dt =
    let newX = (this.Pos.X + (this.Vel.X*dt)) % bounds.X
    let newY = (this.Pos.Y + (this.Vel.Y*dt)) % bounds.Y
    let x = if newX >= 0 then newX else bounds.X + newX
    let y = if newY >= 0 then newY else bounds.Y + newY
    { this with Pos = { X = x; Y = y } }

  member this.Quadrant (bounds: Pair) =
    match (this.Pos.X>bounds.X/2), (this.Pos.X<bounds.X/2), (this.Pos.Y>bounds.Y/2), (this.Pos.Y<bounds.Y/2) with
    | true, false, true, false -> NE
    | true, false, false, true -> SE
    | false, true, true, false -> NW
    | false, true, false, true -> SW
    | _ -> Boundary

let pairParser = pint32 .>> pstring "," .>>. pint32 |>> Pair.ofTuple
let robotParser = pstring "p=" >>. pairParser .>> pstring " v=" .>>. pairParser |>> Robot.ofTuple

let step (bounds: Pair) (robots: seq<Robot>) =
  robots |> Seq.map (fun r -> r.Move bounds 1)

let stepN bounds n (robots: seq<Robot>) =
  robots |> Seq.map (fun r -> r.Move bounds n)

module Part1 =

  let run bounds file =
    System.IO.File.ReadAllLines file
    |> Seq.map (parse robotParser)
    |> Seq.map (fun robot -> robot.Move bounds 100)
    |> Seq.countBy (fun robot -> robot.Quadrant bounds)
    |> Seq.choose (fun (q,c) -> match q with Boundary -> None | _ -> Some c)
    |> Seq.reduce (*)

  let runSample() = run { X = 11; Y = 7 } sample
  let runInput() = run { X = 101; Y = 103 } input

module Part2 =

  let printArrangement (bounds: Pair) (robots: seq<Robot>) =
    let map = Array.init bounds.Y (fun _ -> Array.init bounds.X (fun _ -> '.'))
    robots
    |> Seq.iter (fun r -> map[r.Pos.Y][r.Pos.X] <- '*')
    map
    |> Seq.map (System.String.Concat)
    |> String.concat System.Environment.NewLine

  /// I am defining a metric called 'clumpage' that describes how often robots are close together.
  /// In order to form a tree shape, they would need to have fairly high clumpage.
  /// Clumpage is essentially the average number of other robots at distance one.
  /// Originally, it included diagonals, but as I thought about the problem more, I didn't think
  /// the diagonals were helping much, and doubled the calculation time, so I took them out.
  let clumpage (robots: seq<Robot>) =
    let checkDir (this: Robot) (dir: Pair) = Seq.exists (fun r -> r.Pos = this.Pos.translate dir) robots
    let thisClumpage (this: Robot) =
      cardinals // @ antiCardinals
      |> Seq.map Pair.ofTuple
      |> Seq.sumBy (fun t -> if checkDir this t then 1 else 0)
    Seq.averageBy (thisClumpage >> float) robots

  let printInfo bounds (iter, robots, clumpage) =
    printfn "Iteration %d. Clumpage=%f" iter clumpage
    printfn "%s" (printArrangement bounds robots)

  let run bounds file =
    let robots =
      System.IO.File.ReadAllLines file
      |> Seq.map (parse robotParser)
    // If the christmas tree appears regularly, it must be periodic on both the horizonal and vertical axes.
    // If it's periodicity is 101*1.3=10,403, this should be the furthest we need to search for it, right?
    // If we could find a pattern in either the horiz or vert directions it could limit the search space.
    // Filtering 0..103 on clumpage (cardinal-only), iteration 31 looks of particular interest.
    // It shows some high density in a horizonal band. It might be worth checking how this behaves.
    // This high density band is definitely periodic on height - it occurs every 103rd iteration after 31.
    // Found it! There is a Christmas tree at REDACTED.
    [ 0..101 ]
    |> Seq.map (fun n -> 31 + n*103)
    |> Seq.map (fun n ->
          let robots = stepN bounds n robots
          n, robots, clumpage robots)
    |> Seq.filter (fun (i,rs,c) -> c > 2)
    |> Seq.iter (printInfo bounds)

  let runInput() = run { X = 101; Y = 103 } input

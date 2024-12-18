module AdventOfCode2024.Day12

open Util

let sample = $"{dataFolder}/sample/Day12.txt"
let input = $"{dataFolder}/actual/Day12.txt"

type SearchResult = NewFarm of (int*int) seq | AlreadyFound

let findAllFarms map =

  let rec findFarm (soFar, visited) pos =
    if Set.contains pos visited then (soFar, visited)
    else
      Seq.map ((+.) pos) cardinals
      |> Seq.filter (fun newPos -> Set.contains newPos visited |> not)
      |> Seq.filter (fun newPos -> Seq2.tryItem map newPos = Seq2.tryItem map pos)
      |> Seq.fold findFarm (Set.add pos soFar, Set.add pos visited)

  let folder foundFarms pos =
    if Set.exists (fun (c,s) -> Set.contains pos s) foundFarms then foundFarms // we've already found this farm
    else
      let newFarm = findFarm (Set.empty, Set.empty) pos |> fst
      if Set.isEmpty newFarm then foundFarms
      else Set.add (Seq2.item map pos, newFarm) foundFarms

  List.allPairs [0..Array.length map - 1] [0..Array.length(Array.head map) - 1]
  |> List.fold folder Set.empty

module Part1 =

  let calculateFencingPrice farm =
    let perimeter =
      cardinals
      |> Seq.sumBy (fun dir ->
          Set.filter (fun pos ->
            Set.contains (pos +. dir) farm |> not) farm
          |> Seq.length )
    perimeter * Set.count farm

  let run file =
    let map = System.IO.File.ReadAllLines file |> Array.map Seq.toArray
    findAllFarms map
    |> Seq.sumBy (fun (id,spaces) -> calculateFencingPrice spaces)

  let runSample() = run sample
  let runInput() = run input

module Part2 =

  let isCornerOnDiag map pos diag =
    let thisElem = Seq2.tryItem map pos
    let transl transl = thisElem = Seq2.tryItem map (pos +. transl)
    match transl diag, transl ((0,1) *. diag), transl ((1,0) *. diag) with
    // concave corner
    | false, true, true -> true
    // convex corner
    | false, false, false -> true
    // diagonal is a component of a different farm of the same plant
    | true, false, false -> true
    | _ -> false

  let countCorners map pos = List.sumBy (fun diag -> if isCornerOnDiag map pos diag then 1 else 0) antiCardinals

  let calculateFencingPrice map farm =
    Seq.sumBy (countCorners map) farm * Set.count farm

  let run file =
    let map = System.IO.File.ReadAllLines file |> Array.map Seq.toArray
    findAllFarms map
    // Note: need to use `seq` not `Set` here because we want to allow multiple farms
    // of the same plant with the same cost
    |> Seq.map (fun (id,spaces) -> id, calculateFencingPrice map spaces)
    |> Seq.sumBy snd

  let runSample() = run sample
  let runInput() = run input

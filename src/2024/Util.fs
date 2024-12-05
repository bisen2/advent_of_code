module AdventOfCode2024.Util

open FParsec

let dataFolder = $"{__SOURCE_DIRECTORY__}/../../data/2024"

let parse pattern input =
  match run pattern input with
  | Failure (error, _, _) -> failwith $"""Error "%s{error}" when parsing line "%s{input}"."""
  | Success (x,_,_) -> x

module List =

  let none cond = List.exists cond >> not

  let middle xs = List.item ((List.length xs) / 2) xs

/// Helper functions for working with `seq<_>`
module Seq =

  /// Given a `seq<_>`, finds the indices where `cond` is met.
  let iwhere cond = Seq.indexed >> Seq.filter (fun (_,x) -> cond x) >> Seq.map fst

/// Helper functions for working with `seq<seq<_>>`.
module Seq2 =

  /// Given a `seq<seq<_>>`, finds the indices where `cond` is met.
  let iiwhere cond =
    Seq.map (Seq.iwhere cond)
    >> Seq.mapi (fun i xs -> Seq.map (fun x -> (i,x)) xs)
    >> Seq.concat

  /// Given a `seq<seq<_>>`, tries to get the item at `source[i][j]`.
  /// Returns `None` if the item does not exist.
  let tryItem source (i,j) =
    match Seq.tryItem i source with
    | Some js -> Seq.tryItem j js
    | None -> None

  let trySlice source slice =
    let maybeSlice = Seq.map (tryItem source) slice
    if Seq.exists Option.isNone maybeSlice then
      None
    else
      Some (Seq.choose id maybeSlice)

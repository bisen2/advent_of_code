module AdventOfCode2024.Util

open FParsec

let dataFolder = $"{__SOURCE_DIRECTORY__}/../../data/2024"

let parse pattern input =
  match run pattern input with
  | Failure (error, _, _) -> failwith $"""Error "%s{error}" when parsing line "%s{input}"."""
  | Success (x,_,_) -> x

let cardinals = [ (0,1); (0,-1); (1,0); (-1,0) ]
let antiCardinals = [ (1,1); (1,-1); (-1,1); (-1,-1) ]

/// Helper functions for working with the `seq<_>` type.
module Seq =

  let iwhere cond = Seq.indexed >> Seq.filter (snd >> cond) >> Seq.map fst

  let none cond = Seq.exists cond >> not

  let all cond = Seq.exists (cond >> not) >> not

  let middle xs = Seq.item (Seq.length xs / 2) xs

/// Helper functions for working with the `seq<seq<_>>` type.
module Seq2 =

  /// Given a `seq<seq<_>>`, sees if any of its elements meet condition `cond`
  let exists cond =
    Seq.exists (Seq.exists cond)

  /// Given a `seq<seq<_>>`, sees if any of its elements are `value`
  let contains value = exists ((=) value)

  // Given a `seq<seq<_>>`, finds all indices where `cond` is met.
  let iiwhere cond =
    Seq.map (Seq.iwhere cond)
    >> Seq.mapi (fun i xs -> Seq.map (fun x -> (i,x)) xs)
    >> Seq.concat

  let item source (i,j) = Seq.item j (Seq.item i source)

  /// Given a `seq<seq<_>>`, tries to find the item at `source[i][j]`.
  /// Returns `None` if the item does not exist.
  let tryItem source (i,j) =
    match Seq.tryItem i source with
    | Some js -> Seq.tryItem j js
    | None -> None

  /// Given a `seq<seq<_>>`, tres to fnd the items at each of the given indices.
  /// Returns `None` if any of the items do not exist.
  let trySlice source slice =
    let maybeSlice = Seq.map (tryItem source) slice
    if Seq.exists Option.isNone maybeSlice then None
    else Some (Seq.choose id maybeSlice)

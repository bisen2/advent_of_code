module AdventOfCode2024.Util

open FParsec

let dataFolder = $"{__SOURCE_DIRECTORY__}/../../data/2024"

let parse pattern input =
  match run pattern input with
  | Failure (error, _, _) -> failwith $"""Error "%s{error}" when parsing line "%s{input}"."""
  | Success (x,_,_) -> x

/// Helper functions for working with the `list<_>` type.
module List =

  let iwhere cond = List.indexed >> List.filter (fun (_,x) -> cond x) >> List.map fst

  let none cond = List.exists cond >> not

  let middle xs = List.item ((List.length xs) / 2) xs

/// Helper functions for working with the `list<list<_>>` type.
module List2 =

  /// Given a `list<list<_>>`, finds all indices where `cond` is met.
  let iiwhere cond =
    List.map (List.iwhere cond)
    >> List.mapi (fun i xs -> List.map (fun x -> (i,x)) xs)
    >> List.concat

  /// Given a `list<list<_>>`, tries to get the item at `source[i][j]`.
  /// Returns `None` if the item does not exist.
  let tryItem source (i,j) =
    match List.tryItem i source with
    | Some js -> List.tryItem j js
    | None -> None

  /// Given a `list<list<_>>`, tries to find the items at each of the given indices.
  /// Returns `None` if any of the items do not exist.
  let trySlice source slice =
    let maybeSlice = List.map (tryItem source) slice
    if List.exists Option.isNone maybeSlice then
      None
    else
      Some (List.choose id maybeSlice)

module AdventOfCode2023.Util

open FParsec

let dataFolder = $"{__SOURCE_DIRECTORY__}/../../data/2023"

let parse pattern input =
  match run pattern input with
  | Failure (error,_,_) -> failwith $"""Error "%s{error}" when parsing line "%s{input}"."""
  | Success (x,_,_) -> x

module List =
  let tryMax xs =
    if List.length xs = 0 then None
    else Some (List.max xs)

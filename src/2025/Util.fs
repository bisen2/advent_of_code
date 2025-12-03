module AdventOfCode2025.Util

open FParsec

let dataFolder = $"{__SOURCE_DIRECTORY__}/../../data/2025"

let parse pattern input =
  match run pattern input with
  | Failure (error, _, _) -> failwith $"""Error "%s{error}" when parsing line "%s{input}"."""
  | Success (x, _, _) -> x

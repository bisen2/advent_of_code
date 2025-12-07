module AdventOfCode2025.Util

open FParsec

let dataFolder = $"{__SOURCE_DIRECTORY__}/../../data/2025"

let parse pattern input =
  match run pattern input with
  | Failure (error, _, _) -> failwith $"""Error "%s{error}" when parsing line "%s{input}"."""
  | Success (x, _, _) -> x

module Seq =

  /// Compares two sequences for member equality
  let equal xs ys =
    Seq.length xs = Seq.length ys
    &&
    Seq.zip xs ys
    |> Seq.exists (fun (x,y) -> x <> y)
    |> not

module List =

  /// Finds the index of the largest member of the sequence.
  /// In case of a tie, returns the index of the earliest instance.
  let maxi xs =
    match xs with
    | [] -> failwith "Input sequence was empty."
    | x :: xs ->
        Seq.fold (fun (i,max,maxi) x -> if x > max then i+1, x, i+1 else i+1, max, maxi) (0, x, 0) xs
        |> fun (_,max,maxi) -> {| Value = max; Index = maxi |}

module List2D =

  let padToSquare pad xss =
    let dim = xss |> List.map List.length |> List.max
    xss
    |> List.map (fun xs -> xs @ List.init (dim - List.length xs) (fun _ -> pad))

  /// Requires a square List2D - if your input is not square, run it through List2D.padToSquare first
  let transpose xss =
    let rec impl acc todo =
      match List.head todo with
      | [] -> acc
      | _ -> impl (List.map List.head todo :: acc) (List.map List.tail todo)
    impl [] xss

module Bracket = struct
  type shape = Round | Square | Curly
  type state = Open | Close
  type t = shape * state

  let parse = function
    | ')' -> Some (Close, Round)
    | ']' -> Some (Close, Square)
    | '}' -> Some (Close, Curly)
    | '{' -> Some (Open, Curly)
    | '[' -> Some (Open, Square)
    | '(' -> Some (Open, Round)
    | _ -> None
end

let tests = [ ("()", true); ("[()]", true); ("{", false) ]
let test f (arg, exp) = if f arg = exp then "PASS" else "FAIL"

let balanced chars =
  let rec balanced' (stack : Bracket.shape list) brackets =
    match (brackets, stack) with
    | [], [] -> true
    | (Bracket.Open, b) :: bs, _ -> balanced' (b :: stack) bs
    | (Close, b) :: bs, o :: st when o = b -> balanced' st bs
    | _ -> false
  in
  chars |> String.to_seq
  |> Seq.filter_map Bracket.parse
  |> List.of_seq |> balanced' []

let () =
  let args, exps = tests |> List.to_seq |> Seq.unzip in
  tests
  |> List.map (test balanced)
  |> List.to_seq |> Seq.zip args
  |> Seq.iter (fun (arg, result) -> Printf.printf "%s: %s\n" arg result)

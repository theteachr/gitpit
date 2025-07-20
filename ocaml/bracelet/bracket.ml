type shape =
  | Round
  | Square
  | Curly

type state =
  | Open
  | Close

type t = state * shape

let parse = function
  | ')' -> Some (Close, Round)
  | ']' -> Some (Close, Square)
  | '}' -> Some (Close, Curly)
  | '{' -> Some (Open, Curly)
  | '[' -> Some (Open, Square)
  | '(' -> Some (Open, Round)
  | _ -> None

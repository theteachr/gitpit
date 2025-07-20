let balanced source =
  let step stack bracket =
    match (stack, bracket) with
    | (Bracket.Open, o) :: rest, (Bracket.Close, c) when o = c -> rest
    | _ -> bracket :: stack
  in
  source
  |> Seq.filter_map Bracket.parse
  |> Seq.fold_left step []
  |> List.is_empty

let tests =
  [
    ("((", false);
    ("()", true);
    ("", true);
    (")(", false);
    ("))", false);
    ("]", false);
    ("[()]", true);
    ("{}()[]", true);
    ("{()}[]", true);
    ("a * (b + c) - {[a + c] * c}", true);
  ]

let passed (test, expected) =
  test |> String.to_seq |> Balance.balanced == expected

let print_failures failures =
  print_endline (Printf.sprintf "%d test(s) failed :(" (List.length failures));
  failures
  |> List.mapi (fun i (test, _) -> Printf.sprintf "%02d: %s" (i + 1) test)
  |> List.iter print_endline

let () =
  let failures = tests |> List.filter (Fun.negate passed) in
  if List.length failures > 0 then print_failures failures
  else print_endline "All tests passed :)"

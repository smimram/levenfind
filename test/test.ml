open Extlib

let () =
  print_string "Testing Levenshtein... ";
  assert (String.levenstein "kitten" "sitting" = 3);
  assert (String.levenstein "ca" "abc" = 3);
  assert (String.levenstein "uninformed" "uniformed" = 1);
  print_endline "done."

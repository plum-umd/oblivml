open OUnit2
open Oblivml
open Kind

let join1 ctx =
  (* U \/ A = U *)
  assert_equal ~cmp:equal (join Universal Affine) Universal

let tests =
  "kind" >::: [
    "join1" >:: join1
  ]

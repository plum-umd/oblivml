open OUnit2

let tests =
  "all" >::: [
    Kind.tests
  ]

let main () = run_test_tt_main tests

;;

main ()

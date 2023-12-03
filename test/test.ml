open OUnit2

let _ = print_endline "Date tests";
        run_test_tt_main Date_test.tests

let _ = print_endline "Parse tests";
        run_test_tt_main Parse_test.tests

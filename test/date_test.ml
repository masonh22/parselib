open OUnit2
open Parselib.Date

let equality_test name x1 x2 =
  name >:: (fun _ -> assert_equal x1 x2)

let date_equality name d1 d2 =
  name >:: (fun _ -> assert_equal d1 d2 ~printer:to_string)

let d1 = of_string "2022-12-22"
let d2 = of_string "2021-12-22"
let d3 = of_string "2022-11-22"
let d4 = of_string "2022-12-21"
let d5 = set_year 2222 d4

let df1 = of_fmt "YYYY-MM-DD" "2022-12-22"
let df2 = of_fmt "MM/DD/YY" "12/22/22"
let df3 = of_fmt "DD+MM+YYYY" "22+12+2022"

let ds1 = "2022-02-01"
let ds2 = "22-02-01"

let tests = "Date test suite" >::: [
      equality_test "equal" 0 (cmp d1 d1);
      equality_test "lt year" (-1) (cmp d2 d1);
      equality_test "lt month" (-1) (cmp d3 d1);
      equality_test "lt day" (-1) (cmp d4 d1);
      equality_test "gt year" 1 (cmp d1 d2);
      equality_test "gt month" 1 (cmp d1 d3);
      equality_test "gt day" 1 (cmp d1 d4);
      date_equality "format 1" d1 df1;
      date_equality "format 2" d1 df2;
      date_equality "format 3" d1 df3;
      equality_test "day" 22 (get_day d1);
      equality_test "month" 12 (get_month d1);
      equality_test "year" 2222 (get_year d5);
      equality_test "to_string" ds1 (of_string ds1 |> to_string);
      equality_test "to_fmt" ds1 (of_string ds1 |> to_fmt "YYYY-MM-DD");
      (* equality_test "to_fmt 2" ds2 (of_string ds1 |> to_fmt "YY-MM-DD"); *)
    ]

open OUnit2
open Parselib.Parse

let equality_test name s1 s2 =
  name >:: (fun _ -> assert_equal s1 s2)

let str_test name s1 s2 =
  name >:: (fun _ -> assert_equal s1 s2 ~printer:Fun.id)

(* TODO add date tests *)
(* TODO test other delimiters *)
let str = "hi there,val2,1234.56,123456,e1;e2;e3;e4;,end"

let fmt1 = FString @> FString @> FString @> FInt  @> (FList (Delim ";", FString)) @> FString
let parsed1 = ("hi there", "val2", "1234.56", 123456, ["e1";"e2";"e3";"e4"], "end")
let unfold1 (s1, (s2, (s3, (i4, (l5, s6))))) = (s1, s2, s3, i4, l5, s6)
let fold1 (s1, s2, s3, i4, l5, s6) = (s1, (s2, (s3, (i4, (l5, s6)))))

let fmt2 = FString #@ " " (FString @> FString @> FString @> FInt @> (FList (Delim ";", FString)) @> FString)
let parsed2 = ("hi", "there", "val2", "1234.56", 123456, ["e1";"e2";"e3";"e4"], "end")
let unfold2 (s1, (s2, (s3, (s4, (i5, (l6, s7)))))) = (s1, s2, s3, s4, i5, l6, s7)
let fold2 (s1, s2, s3, s4, i5, l6, s7) = (s1, (s2, (s3, (s4, (i5, (l6, s7))))))

let fmt3 = FString @> FString @> FFloat @> FInt  @> (FList (Delim ";", FString)) @> FString
let parsed3 = ("hi there", "val2", 1234.56, 123456, ["e1";"e2";"e3";"e4"], "end")
let unfold3 (s1, (s2, (f3, (i4, (l5, s6))))) = (s1, s2, f3, i4, l5, s6)
let fold3 (s1, s2, f3, i4, l5, s6) = (s1, (s2, (f3, (i4, (l5, s6)))))

(* test empty fields *)
let str2 = "abcd,15.3,,24,"
let unfold_empty (v1, (v2, (v3, (v4, v5)))) = (v1, v2, v3, v4, v5)
let fold_empty (v1, v2, v3, v4, v5) = (v1, (v2, (v3, (v4, v5))))

let fmt5 = FString @> FFloat @> FString @> FInt @> FString
let parsed5 = ("abcd", 15.3, "", 24, "")

let fmt7 = FString @> FFloat @> FNullable FInt @> FInt @> FNullable FInt
let parsed7 = ("abcd", 15.3, None, 24, None)

let fmt8 = FString @> FFloat @> FNullable FFloat @> FInt @> FNullable FFloat
let parsed8 = ("abcd", 15.3, None, 24, None)

let fmt9 = FString @> FFloat @> FNullable (FList (Delim ";", FInt)) @> FInt @> FNullable (FList (Delim ";", FInt))
let parsed9 = ("abcd", 15.3, None, 24, None)

let fmt10 = FString @> FFloat @> FNullable (FInt @> FInt) @> FInt @> FNullable (FInt @> FInt)
let parsed10 = ("abcd", 15.3, None, 24, None)

let fmt11 = FList (Delim ",", FNullable FString)
let parsed11 = [Some "abcd"; Some "15.3"; None; Some "24"]

(* test lists *)
let str3 = "100;200;4034;;532;;1235;543;21"
let str4 = "100;200;4034;;532;;1235;543;21;"
let fmt12 = FList (Delim ";;", FList (Delim ";", FInt))
let parsed12 = [[100; 200; 4034]; [532]; [1235; 543; 21]]

(* test seq *)
let fmt13 = FSeq (Delim ";", FNullable FInt)
let parsed13 = [Some 100; Some 200; Some 4034; None; Some 532; None; Some 1235; Some 543; Some 21]
let fmt14 = FSeq (Delim ";;", FSeq (Delim ";", FInt))
let parsed14 = parsed12

let mything = try parse_str fmt13 str3 () with
              | Not_found -> failwith "not found!"

let tests = "Parsing test suite" >::: [
      equality_test "basic 1" parsed1 (parse_str fmt1 str |> unfold1);
      equality_test "basic 2" parsed2 (parse_str fmt2 str |> unfold2);
      equality_test "basic 3" parsed3 (parse_str fmt3 str |> unfold3);
      (* emtpy fields *)
      equality_test "emtpy string" parsed5 (parse_str fmt5 str2 |> unfold_empty);
      equality_test "emtpy int" parsed7 (parse_str fmt7 str2 |> unfold_empty);
      equality_test "emtpy float" parsed8 (parse_str fmt8 str2 |> unfold_empty);
      equality_test "emtpy list" parsed9 (parse_str fmt9 str2 |> unfold_empty);
      equality_test "emtpy pair" parsed10 (parse_str fmt10 str2 |> unfold_empty);
      equality_test "emtpy list items" parsed11 (parse_str fmt11 str2);
      (* to_str *)
      equality_test "to_str basic 1" parsed1 (fold1 parsed1 |> to_str fmt1 |> parse_str fmt1 |> unfold1);
      equality_test "to_str basic 2" parsed2 (fold2 parsed2 |> to_str fmt2 |> parse_str fmt2 |> unfold2);
      equality_test "to_str basic 3" parsed3 (fold3 parsed3 |> to_str fmt3 |> parse_str fmt3 |> unfold3);
      equality_test "to_str empty string" parsed5 (fold_empty parsed5 |> to_str fmt5 |> parse_str fmt5 |> unfold_empty);
      equality_test "to_str empty string" parsed5 (fold_empty parsed5 |> to_str fmt5 |> parse_str fmt5 |> unfold_empty);
      equality_test "to_str empty int" parsed7 (fold_empty parsed7 |> to_str fmt7 |> parse_str fmt7 |> unfold_empty);
      equality_test "to_str empty float" parsed8 (fold_empty parsed8 |> to_str fmt8 |> parse_str fmt8 |> unfold_empty);
      equality_test "to_str empty list" parsed9 (fold_empty parsed9 |> to_str fmt9 |> parse_str fmt9 |> unfold_empty);
      equality_test "to_str empty pair" parsed10 (fold_empty parsed10 |> to_str fmt10 |> parse_str fmt10 |> unfold_empty);
      equality_test "to_str empty list items" parsed11 (parsed11 |> to_str fmt11 |> parse_str fmt11);
      (* lists *)
      equality_test "list 1" parsed12 (parse_str fmt12 str3);
      equality_test "list 2" parsed12 (parse_str fmt12 str4);
      (* seq *)
      str_test "seq 1 str" str3 (parse_str fmt13 str4 |> to_str fmt13);
      str_test "seq 2 str" str3 (parse_str fmt14 str3 |> to_str fmt14);
      equality_test "seq 1" parsed13 (parse_str fmt13 str4 |> list_of_seq);
      equality_test "seq 2" parsed14 (parse_str fmt14 str4 |> list_of_seq |> List.map list_of_seq);
    ]

type t = { mutable year: int; mutable month: int; mutable day: int }

let get_day { day; _ } = day
let get_month { month; _ } = month
let get_year { year; _ } = year

let set_day day date = { date with day }
let set_month month date = { date with month }
let set_year year date = { date with year }

let unix_epoch = { year=1970; month=1; day=1 }

(* assuming format is YEAR-MONTH-DAY for now *)
let of_string str =
  let str_list =
    String.split_on_char '-' str |> List.map int_of_string
  in
  match str_list with
  | [year; month; day] -> { year; month; day }
  | _ -> failwith "invalid date"

(** [string_of_int_len len num] is [num] represented as a string of length
    [len]. *)
let string_of_int_len len num =
  let str = string_of_int num in
  let len' = String.length str in
  if len' > len then
    (* the number is too long, chop off the most significant digits *)
    String.sub str (len' - len) len
  else if len' < len then
    (* the number is too short, add trailing zeros *)
    String.make (len - len') '0' ^ str
  else
    str

let to_string { year; month; day } =
  string_of_int_len 4 year
  ^ "-" ^ string_of_int_len 2 month
  ^ "-" ^ string_of_int_len 2 day

exception Invalid_format of string * string

(* This is terrible *)
let of_fmt fmt str =
  let exn = Invalid_format (fmt, str) in
  let date = { year=(-1); month=(-1); day=(-1) } in
  (* extract substrings from str in accumulator when we hit a format char (i.e.
     not YMD) *)
  let fold_fun acc fmt =
    match fmt, acc with
    | 'Y', (n, _, str) -> (n + 1, 'Y', str)
    | 'M', (n, _, str) -> (n + 1, 'M', str)
    | 'D', (n, _, str) -> (n + 1, 'D', str)
    | c, (n, p, str) ->
       if c <> String.get str n then raise exn else
         let num = String.sub str 0 n |> int_of_string in
         let str' = String.sub str (n + 1) (String.length str - n - 1) in
         (match p with
          | 'Y' ->
             let num' = if n < 4 then num + 2000 else num in
             date.year <- num'
          | 'M' -> date.month <- num
          | 'D' -> date.day <- num
          | _ -> raise exn);
         (0, ' ', str')
  in
  if String.length fmt <> String.length str then
    raise exn
  else
    (* need an extra char at the end of fmt so we update the date in f *)
    ignore (String.fold_left fold_fun (0, ' ', str ^ " ") (fmt ^ " "));
  date

(* TODO make {to,of}_fmt better *)
let to_fmt fmt { year; month; day } =
  (* extract substrings from str in accumulator when we hit a format char (i.e.
     not YMD) *)
  let fold_fun acc fmt_char =
    match fmt_char, acc with
    | 'Y', (n, _, str) -> (n + 1, 'Y', str)
    | 'M', (n, _, str) -> (n + 1, 'M', str)
    | 'D', (n, _, str) -> (n + 1, 'D', str)
    | c, (n, p, str) ->
       let delim =
         (* our special delim to let us know we're at the end of the string *)
         if Char.code c = 7 then "" else String.make 1 c in
       let str' =
         match p, n with
         | 'Y', 2 ->
            Printf.sprintf "%02d" (year mod 100) ^ delim
         | 'Y', 4 ->
            Printf.sprintf "%04d" year ^ delim
         | 'M', 2 -> Printf.sprintf "%02d" month ^ delim
         | 'D', 2 -> Printf.sprintf "%02d" day ^ delim
         | _ -> failwith "TODO bad date fmt (1)"
       in
       (0, ' ', str ^ str')
  in
  let (_, _, str) =
    String.fold_left fold_fun (0, ' ', "") (fmt ^ String.make 1 (Char.chr 7)) in
  str

let cmp { year=y1; month=m1; day=d1 } { year=y2; month=m2; day=d2 } =
  if y1 < y2 then -1
  else if y1 > y2 then 1
  else if m1 < m2 then -1
  else if m1 > m2 then 1
  else if d1 < d2 then -1
  else if d1 > d2 then 1
  else 0

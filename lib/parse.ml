type delim =
  | Delim of string

type _ format =
  | FDate : string -> Date.t format
  | FString : string format
  | FInt : int format
  | FFloat : float format
  | FCustom : (string -> 'a) -> 'a format
  | FNullable : 'a format -> 'a option format
  | FList : delim * 'a format -> 'a list format
  | FSeq : delim * 'a format -> 'a Seq.t format
  | FPair : 'a format * delim * 'b format -> ('a * 'b) format
  | FDropF : _ format * delim * 'b format -> 'b format
  | FDropB : 'a format * delim * _ format -> 'a format

let (@>) fm1 fm2 = FPair (fm1, Delim ",", fm2)
let (#@) fmt str = fun fmt' -> FPair (fmt, Delim str, fmt')

exception Parse_error of string
exception No_delim of string

(** parse_str *)
let rec parse_str : type t. t format -> string -> t =
  (* returns (str[0:n], str[n+m:])
   * NOTE: the nth character is not in either substring returned by this. *)
  let split_on str n m =
    let len = String.length str in
    try (String.sub str 0 n, String.sub str (n + m) (len - n - m)) with
    | Invalid_argument _ -> failwith "internal error"
  in
  (* returns (s1, s2) where s1 is the string before and s2 is the string after
     the delimiter *)
  let split_delim delim str =
    match delim with
    | Delim s -> begin
        let r = Str.regexp s in
        let pos =
          try Str.search_forward r str 0 with
          | Not_found -> raise (No_delim ("split_delim(" ^ s ^ "): " ^ str))
        in
        let len = String.length (Str.matched_string str) in
        split_on str pos len
      end
  in
  (* Splits a string on a delimiter into a list *)
  let split_delim_list delim str =
    match delim with
    | Delim s ->
       Str.split (Str.regexp s) str
  in

  fun fmt str ->
  match fmt with
  | FDate dfmt -> Date.of_fmt dfmt str
  | FString -> str
  | FInt -> begin
      try int_of_string str with
      | Failure s -> raise (Parse_error (s ^ ": " ^ str))
    end
  | FFloat -> begin
      try float_of_string str with
      | Failure s -> raise (Parse_error (s ^ ": " ^ str))
    end
  | FCustom converter -> converter str
  | FNullable fmt' ->
     if str = "" then None else
       Some (parse_str fmt' str)
  | FList (delim, fmt') ->
     split_delim_list delim str |> List.map (parse_str fmt')
  | FSeq (delim, fmt') ->
     Seq.unfold (fun str' ->
         if String.length str' = 0 then None else
           let (str1, str2) =
             try split_delim delim str' with
             | No_delim _ -> (str', "")
           in
           Some (parse_str fmt' str1, str2)) str
  | FPair (fmt1, delim, fmt2) ->
     let str1, str2 = split_delim delim str in
     (parse_str fmt1 str1, parse_str fmt2 str2)
  | FDropF (fmt1, delim, fmt2) ->
     let _, str2 = split_delim delim str in
     parse_str fmt2 str2
  | FDropB (fmt1, delim, fmt2) ->
     let str1, _ = split_delim delim str in
     parse_str fmt1 str1

(** to_str *)
let rec to_str : type t. t format -> t -> string =
  let add_delim delim str =
    match delim with
    | Delim s -> str ^ s
  in
  let get_delim = function
    | Delim s -> s
  in

  fun fmt data ->
  match fmt, data with
  | FDate dfmt, date -> Date.to_fmt dfmt date
  | FString, str -> str
  | FInt, i -> string_of_int i
  | FFloat, f -> string_of_float f
  | FNullable fmt', data' -> begin
      match data' with
      | None -> ""
      | Some d -> to_str fmt' d
    end
  | FList (delim, fmt'), lst ->
     String.concat (get_delim delim) (List.map (to_str fmt') lst)
  | FSeq (delim, fmt'), seq ->
     Seq.fold_lefti
       (fun acc i elt ->
         let str' = to_str fmt' elt in
         if i = 0 then str' else add_delim delim acc ^ str')
       "" seq
  | FPair (fmt1, delim, fmt2), (d1, d2) ->
     let s1 = to_str fmt1 d1 in
     let s2 = to_str fmt2 d2 in
     add_delim delim s1 ^ s2
  | FCustom _, _ -> failwith "FCustom to_str unimplemented"
  | FDropF _, _ -> failwith "FDropF to_str unimplemented"
  | FDropB _, _ -> failwith "FDropB to_str unimplemented"

(** to_file *)
let to_file fmt data file =
  let out = open_out file in
  (* TODO optimize this *)
  output_string out (to_str fmt data);
  close_out out

(** list_of_seq *)
let list_of_seq seq =
  Seq.fold_left (fun lst elt -> elt :: lst) [] seq |> List.rev

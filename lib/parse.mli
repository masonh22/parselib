type delim =
  | Delim of string (* can be a regex *)

(* describes the format for a line of data *)
type _ format =
  | FDate : string -> Date.t format
  | FString : string format
  | FInt : int format (* string converted to an int *)
  | FFloat : float format (* decimal converted to a float *)
  | FCustom : (string -> 'a) -> 'a format
  | FNullable : 'a format -> 'a option format
  | FList : delim * 'a format -> 'a list format
  | FSeq : delim * 'a format -> 'a Seq.t format
  | FPair : 'a format * delim * 'b format -> ('a * 'b) format
  | FDropF : _ format * delim * 'b format -> 'b format
  | FDropB : 'a format * delim * _ format -> 'a format

(* syntactic sugar for writing formats *)
val (@>) : 'a format -> 'b format -> ('a * 'b) format (* comma separated *)
val (#@) : 'a format -> string -> 'b format -> ('a * 'b) format

(* generic parse error *)
exception Parse_error of string
(* couldn't find delimiter *)
exception No_delim of string

(** [parse_str fmt str] is the string [str] parsed into format [fmt].
    raises [Parse_error] if [str] cannot be parsed with with format [fmt]. *)
val parse_str : 'a format -> string -> 'a

(** [to_str fmt data] is a string representation of [data]. Doesn't add trailing
 *  delimiters for lists, sequences, and pairs. *)
val to_str : 'a format -> 'a -> string

(** [list_of_seq seq] is a list made up of elements of [seq] *)
val list_of_seq : 'a Seq.t -> 'a list

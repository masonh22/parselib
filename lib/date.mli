(** type representing a date *)
type t

(** getters *)
val get_day : t -> int
val get_month : t -> int
val get_year : t -> int

(** setters *)
val set_day : int -> t -> t
val set_month : int -> t -> t
val set_year : int -> t -> t

(** January 1, 1970 *)
val unix_epoch : t


(** parse a string into a date (year-month-day) *)
val of_string : string -> t

val to_string : t -> string

exception Invalid_format of string * string

(** parse a string into a date formatted by the format string.
    e.g. MM-DD-YY, DD/MM/YYYY.
 *)
val of_fmt : string -> string -> t

val to_fmt : string -> t -> string

(** compare two dates *)
val cmp : t -> t -> int

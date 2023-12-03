(** [read_lines ic] is a sequence of lines from input channel [ic]. *)
val read_lines : in_channel -> string Seq.t

(** [read_file file] is the contents of file [file] in a string. *)
val read_file : string -> string

(** [read_bytes len ic] is a sequence of byte buffers of length at most [len]
    from input channel [ic]. *)
val read_bytes : int -> in_channel -> (bytes * int) Seq.t

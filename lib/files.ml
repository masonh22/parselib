let read_lines =
  Seq.unfold
    (fun ic ->
      Option.map (fun s -> (s, ic))
        (In_channel.input_line ic))

let read_file file =
  let open In_channel in
  open_text file |> input_all

let read_bytes len =
  Seq.unfold
    (fun ic ->
      let buf = Bytes.create len in
      let n = In_channel.input ic buf 0 len in
      if n = 0 then None else Some ((buf, n), ic))

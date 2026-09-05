(* The few UTF-8 questions the CLI asks of a string, on the decoder of the
   standard library: each step is a character or a maximal malformed
   subpart, with its byte offset. *)

let fold (f : 'a -> int -> Uchar.utf_decode -> 'a) (init : 'a) (str : string)
    : 'a =
  let len = String.length str in
  let rec go (i : int) (acc : 'a) : 'a =
    if i >= len then acc
    else
      let dec = String.get_utf_8_uchar str i in
      go (i + Uchar.utf_decode_length dec) (f acc i dec)
  in
  go 0 init

let is_valid : string -> bool = String.is_valid_utf_8

let length (str : string) : int =
  fold (fun (n : int) (_ : int) (_ : Uchar.utf_decode) -> n + 1) 0 str

let code_point_offsets (str : string) : int array =
  fold (fun (acc : int list) (i : int) (_ : Uchar.utf_decode) -> i :: acc) [] str
  |> List.cons (String.length str)
  |> List.rev |> Array.of_list

let sanitize (str : string) : string =
  if is_valid str then str
  else
    let buf = Buffer.create (String.length str) in
    (* a malformed subpart decodes to Uchar.rep, U+FFFD *)
    fold
      (fun () (_ : int) (dec : Uchar.utf_decode) ->
        Buffer.add_utf_8_uchar buf (Uchar.utf_decode_uchar dec))
      () str;
    Buffer.contents buf

type t = string list

let clean (segments : string list) : string list =
  List.filter
    (fun (segment : string) ->
      not (String.equal segment "" || String.equal segment "."))
    segments

let of_segments (segments : string list) : t = clean segments

let of_string (path : string) : t = clean (String.split_on_char '/' path)

(* "/" here is the Go import-path separator (always forward slash,
   OS-independent), not an OS file-path separator; deliberately kept as
   "/" regardless of platform. *)
let to_string (path : t) : string = String.concat "/" path

let segments (path : t) : string list = path

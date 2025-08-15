module S = String

type single_line = string

let is_if_start (l : single_line) : bool =
  let s = S.trim l in
  S.starts_with ~prefix:"#if" s ||
  S.starts_with ~prefix:"#ifdef" s ||
  S.starts_with ~prefix:"#ifndef" s

let is_if_middle (l : single_line) : bool =
  let s = S.trim l in
  S.starts_with ~prefix:"#else" s ||
  S.starts_with ~prefix:"#elif" s

let is_if_end (l : single_line) : bool =
  let s = S.trim l in
  S.starts_with ~prefix:"#endif" s

let lines (s : string) : single_line list =
  S.split_on_char '\n' s

let unlines (ls : single_line list) : string =
  S.concat "\n" ls

type ctx = If | Else

let keep_all (s : string) : string =
  let go (l : single_line) : single_line =
    if is_if_start l || is_if_middle l || is_if_end l
      then ""
      else l
  in
  s
  |> lines
  |> List.map go
  |> unlines

let positive (s : string) : string =
  let go (ctx : ctx list) (l : single_line) : ctx list * single_line =
    (* invariant: never put If on Else *)
    match ctx with
    | If :: _ | [] when is_if_start l -> (If :: ctx), ""
    (* If we're under #else, we put Else even for #if, because we don't print *)
    | Else :: _ when is_if_start l -> (Else :: ctx), ""
    | If :: ctx' when is_if_middle l -> (Else :: ctx'), ""
    | _ when is_if_middle l -> ctx, ""
    | _ :: ctx' when is_if_end l -> ctx', ""
    | If :: _ | [] -> ctx, l
    | _ -> ctx, ""
  in
  s
  |> lines
  |> List.fold_left_map go []
  |> snd
  |> unlines

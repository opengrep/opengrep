type t

val empty : t

(* The crates whose manifest ([Cargo.toml], Scarb's [Scarb.toml]) holds a
   [package] name; a file under [src] whose stem is one of [root_stems] is the
   crate's root module. *)
val discover :
  manifest_name:string -> root_stems:string list -> project_root:Fpath.t ->
  Fpath.t list -> t

val module_qn_of_file : t -> Fpath.t -> Names.Module_qn.t option

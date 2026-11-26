type t

val empty : t

(* The crates whose manifest ([Cargo.toml], Scarb's [Scarb.toml]) holds a
   [package] name. *)
val discover :
  manifest_name:string -> project_root:Fpath.t -> Fpath.t list -> t

val module_qn_of_file : t -> Fpath.t -> Names.Module_qn.t option

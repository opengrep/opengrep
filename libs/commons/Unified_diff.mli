(* The hunks of a line diff from old_ to new_ in the unified format without
   context lines and without file headers, e.g.
     @@ -2 +2 @@
     -sys.exit(1)
     +somethingElse(1)
*)
val lines : old_:string -> new_:string -> string list

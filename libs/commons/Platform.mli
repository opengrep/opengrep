(* note that Sys.os_type uses "unix" for both Darwin and Linux *)
type kernel = Darwin | Linux | OtherKernel of string

(* We need Cap.exec because the function is calling 'uname' internally.
 * You should avoid using this function and prefer if possible
 * Sys.os_type, Sys.{unix,win32,cygwin}
 *)

val kernel : < Cap.exec > -> kernel

(* alias for Sys.win32 *)
val is_windows : bool

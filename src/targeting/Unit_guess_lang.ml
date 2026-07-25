(*
   Unit tests for Guess_lang
*)
open Lang
open Fpath_.Operators

type exec = Exec | Nonexec

(* TODO: simplify and use Testo.create ~expected_outcome:Should_fail *)
type success = OK | XFAIL

let t = Testo.create

(*
   For these tests, the file doesn't need to exist.
*)
let name_tests : (string * Lang.t * Fpath.t * success) list =
  [
    (* name, language, file name, expected result *)
    ("js", Js, "foo.js", OK);
    ("js relative path", Js, "./a/b.c/foo.js", OK);
    ("js absolute path", Js, "/a/b.c/foo.js", OK);
    ("js double extension", Js, "foo.bar.js", OK);
    ("min js", Js, "foo.min.js", OK);
    ("not js", Js, "foo.bar", XFAIL);
    ("jsx", Js, "foo.jsx", OK);
    ("typescript", Ts, "foo.ts", OK);
    ("typescript .d.ts", Ts, "foo.d.ts", OK);
    ("typescript .mts", Ts, "foo.mts", OK);
    ("typescript .cts", Ts, "foo.cts", OK);
    ("typescript .d.mts", Ts, "foo.d.mts", OK);
    ("typescript .d.cts", Ts, "foo.d.cts", OK);
    ("crystal", Crystal, "foo.cr", OK);
    ("spaces", Ruby, " a b  c.rb", OK);
  ]
  |> List_.map (fun (name, (lang : Lang.t), path, expect) ->
         (name, lang, Fpath.v path, expect))

let contents_tests : (string * Lang.t * string * string * exec * success) list =
  [
    (* name, language, file name, file contents, executable?, expected result *)
    ("correct extension nonexec", Js, "foo1.js", "", Exec, OK);
    ("correct extension exec", Js, "foo2.js", "", Exec, OK);
    ("wrong extension exec", Js, "foo3.bar", "", Exec, XFAIL);
    ("bash non-executable", Bash, "hello1.bash", "", Nonexec, OK);
    ("bash exec", Bash, "hello2", "#!/anything/bash\n", Exec, OK);
    ("sh exec", Bash, "hello3", "#!/bin/sh\n# hello!", Exec, OK);
    ("bash exec env", Bash, "hello4", "#! /usr/bin/env bash\n", Exec, OK);
    ("other exec env", Bash, "hello5", "#!/usr/bin/env bashxxxx\n", Exec, XFAIL);
    ("env -S", Bash, "hello6", "#!/usr/bin/env -Sbash -eu\n", Exec, OK);
    ("hack with .hack extension", Hack, "foo.hack", "", Nonexec, OK);
    ( "hack without extension",
      Hack,
      "foo",
      "#!/usr/bin/env hhvm\nxxxx",
      Exec,
      OK );
    ( "hack with .php extension and shebang",
      Hack,
      "foo.php",
      "#!/usr/bin/env hhvm\nxxxx",
      Nonexec,
      OK );
    ( "hack with .php extension no shebang",
      Hack,
      "foo.php",
      "<?hh\n",
      Nonexec,
      OK );
    ("php", Php, "foo.php", "", Nonexec, OK);
    ("crystal exec", Crystal, "hello_cr", "#!/usr/bin/env crystal\n", Exec, OK);
    ( "aspnet handler in csharp",
      Csharp,
      "handler1.ashx",
      "<%@ WebHandler Language=\"C#\" Class=\"H\" %>\n",
      Nonexec,
      OK );
    ( "aspnet handler in csharp is not vb",
      Vb,
      "handler2.ashx",
      "<%@ WebHandler Language=\"C#\" Class=\"H\" %>\n",
      Nonexec,
      XFAIL );
    ( "aspnet web service in vb",
      Vb,
      "service1.asmx",
      "<%@ WebService Language=\"VB\" Class=\"S\" %>\n",
      Nonexec,
      OK );
    ( "aspnet web service in vb is not csharp",
      Csharp,
      "service2.asmx",
      "<%@ WebService Language=\"VB\" Class=\"S\" %>\n",
      Nonexec,
      XFAIL );
    ( "aspnet handler without language is vb",
      Vb,
      "handler3.ashx",
      "<%@ WebHandler Class=\"H\" %>\n",
      Nonexec,
      OK );
    ( "aspnet handler without language is not csharp",
      Csharp,
      "handler4.ashx",
      "<%@ WebHandler Class=\"H\" %>\n",
      Nonexec,
      XFAIL );
    ( "aspnet handler without directive is not csharp",
      Csharp,
      "handler5.ashx",
      "class H {}\n",
      Nonexec,
      XFAIL );
    ( "aspnet handler without directive is not vb",
      Vb,
      "handler6.ashx",
      "class H {}\n",
      Nonexec,
      XFAIL );
    ( "aspnet handler in jscript is not csharp",
      Csharp,
      "handler7.ashx",
      "<%@ WebHandler Language=\"JScript\" Class=\"H\" %>\n",
      Nonexec,
      XFAIL );
    ( "aspnet handler in jscript is not vb",
      Vb,
      "handler8.ashx",
      "<%@ WebHandler Language=\"JScript\" Class=\"H\" %>\n",
      Nonexec,
      XFAIL );
    ( "aspnet handler with unterminated directive",
      Vb,
      "handler9.ashx",
      "<%@ WebHandler Class=\"H\"\n",
      Nonexec,
      XFAIL );
    ( "aspnet handler header forms",
      Csharp,
      "handler10.ashx",
      "\xEF\xBB\xBF<%@ Assembly Name=\"System.Web\" %>\r\n\
       <% @ webhandler\n language='cs' class=H description=\"a %> b\" %>",
      Nonexec,
      OK );
    ( "aspnet handler with unquoted language",
      Vb,
      "handler11.ashx",
      "<%@ WebHandler Language=vb Class=H %>",
      Nonexec,
      OK );
    ( "aspnet page is not a handler",
      Csharp,
      "page1.aspx",
      "<%@ Page Language=\"C#\" %>\n",
      Nonexec,
      XFAIL );
  ]

let mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o777

(*
   Create a temporary file with the specified name, in a local tmp folder.
   We don't delete the files when we're done because it's easier when
   troubleshooting tests.
*)
let with_file name contents exec f =
  let dir = Fpath.v "tmp" in
  mkdir !!dir;
  let path = dir / name in
  let oc = open_out_bin !!path in
  (match exec with
  | Exec -> Unix.chmod !!path 0o755
  | Nonexec -> ());
  Common.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      output_string oc contents;
      close_out oc;
      f path)

let test_name_only lang path expectation =
  match (expectation, Guess_lang.inspect_file lang path) with
  | OK, Ok _
  | XFAIL, Error _ ->
      ()
  | _ -> assert false

let test_with_contents lang name contents exec expectation =
  with_file name contents exec (fun path ->
      match (expectation, Guess_lang.inspect_file lang path) with
      | OK, Ok _
      | XFAIL, Error _ ->
          ()
      | _ -> assert false)

(* An upper-case extension selects a handler file exactly when it selects a
   C# file: Filename.check_suffix ignores case on Windows only. *)
let test_handler_extension_case () =
  with_file "Upper1.CS" "" Nonexec (fun (cs_file : Fpath.t) ->
      with_file "Upper2.ASHX" "<%@ WebHandler Language=\"C#\" Class=\"H\" %>\n"
        Nonexec (fun (handler_file : Fpath.t) ->
          let is_csharp (file : Fpath.t) : bool =
            Result.is_ok (Guess_lang.inspect_file Csharp file)
          in
          assert (Bool.equal (is_csharp cs_file) (is_csharp handler_file))))

let test_inspect_file =
  List_.map
    (fun (test_name, lang, path, expectation) ->
      t test_name (fun () -> test_name_only lang path expectation))
    name_tests
  @ List_.map
      (fun (test_name, lang, file_name, contents, exec, expectation) ->
        t test_name (fun () ->
            test_with_contents lang file_name contents exec expectation))
      contents_tests
  @ [ t "aspnet handler extension case as for csharp" test_handler_extension_case ]

let tests =
  Testo.categorize_suites "Guess_lang"
    [ Testo.categorize "inspect_file" test_inspect_file ]

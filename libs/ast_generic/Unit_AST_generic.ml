module G = AST_generic

let t = Testo.create

let sid (binding : int) : G.SId.t =
  G.SId.of_tok ~binding ~file:"test.ml" (Tok.unsafe_fake_tok "x")

let id_info_with_payloads ~(type_name : string) ~(callee : int)
    ~(const : G.const_type) ~(hidden : bool) : G.id_info =
  let info = G.empty_id_info () in
  let ty = G.ty_builtin (type_name, Tok.unsafe_fake_tok type_name) in
  info.G.id_type := Some ty;
  info.G.id_instance_type := Some ty;
  info.G.id_callee_definition := Some (sid callee);
  info.G.id_svalue := Some (G.Cst const);
  info.G.id_flags :=
    IdFlags.make ~hidden ~case_insensitive:false ~final:false ~static:false;
  info

let test_id_info_payloads_ignored () =
  let resolved : G.resolved_name option = Some (G.Global, sid 1) in
  let a =
    id_info_with_payloads ~type_name:"int" ~callee:2 ~const:G.Cint
      ~hidden:false
  in
  let b =
    id_info_with_payloads ~type_name:"string" ~callee:3 ~const:G.Cstr
      ~hidden:true
  in
  a.G.id_resolved := resolved;
  b.G.id_resolved := resolved;
  if G.compare_id_info a b <> 0 then assert false;
  if not (G.equal_id_info a b) then assert false

let test_id_info_resolved_distinguished () =
  let a = G.empty_id_info () in
  let b = G.empty_id_info () in
  a.G.id_resolved := Some (G.Global, sid 1);
  b.G.id_resolved := Some (G.Global, sid 2);
  if G.compare_id_info a b = 0 then assert false

let tests =
  Testo.categorize "AST_generic"
    [
      t "id_info_payloads_ignored" test_id_info_payloads_ignored;
      t "id_info_resolved_distinguished" test_id_info_resolved_distinguished;
    ]

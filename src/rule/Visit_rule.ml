open Rule

(* currently used in Check_rule.ml metachecker *)

(* A more generic formula visitor than the specialized one for xpatterns below *)
let visit_formula f (formula : formula) : unit =
  let rec aux formula =
    f formula;
    (match formula.f with
    | P _ -> ()
    | Anywhere (_, formula)
    | Inside (_, formula)
    | Not (_, formula) ->
        aux formula
    | Or (_, xs)
    | And (_, xs) ->
        xs |> List.iter aux);
    formula.conditions
    |> List.iter (fun (_, cond) ->
           match cond with
           | CondNestedFormula (_, _, formula) -> aux formula
           | CondEval _
           | CondName _
           | CondType _
           | CondRegexp _
           | CondAnalysis _ ->
               ())
  in
  aux formula


let visit_xpatterns (func : Xpattern.t -> inside:bool -> 'acc -> 'acc)
    (formula : formula)
    (acc : 'acc) : 'acc =
  let rec aux inside acc formula =
    match formula.f with
    | P p -> func p ~inside acc
    | Anywhere (_, subf)
    | Inside (_, subf) ->
        aux true acc subf
    | Not (_, x) ->
        aux inside acc x
    | Or (_, xs)
    | And (_, xs) ->
        List.fold_left (fun acc x -> aux inside acc x) acc xs
  in
  aux false acc formula


let xpatterns_of_rule (rule : Rule.t) : Xpattern.t list =
  let xs = formulas_of_mode rule.mode in
  let collect xpat ~inside:_ acc = xpat :: acc in
  List.fold_left (fun acc f -> visit_xpatterns collect f acc) [] xs
  |> List.rev  (* reverse to preserve original order *)

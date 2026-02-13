(* Unified Language Configuration
 *
 * Consolidates language-specific settings for taint analysis and call graphs:
 * - HOF (Higher-Order Function) configurations
 * - Constructor patterns
 *)

(* ========================================================================== *)
(* HOF Configuration Types *)
(* ========================================================================== *)

type hof_kind =
  | MethodHOF of {
      methods : string list;
      arity : int;
      taint_arg_index : int;
    }
  | FunctionHOF of {
      functions : string list;
      arity : int;
      callback_index : int;
      data_index : int;
    }
  | ReturningFunctionHOF of {
      methods : string list;
    }

(* ========================================================================== *)
(* Collection Model Types *)
(* ========================================================================== *)

type collection_model_kind =
  | ArgTaintsThis of {
      methods : string list;
      arity : int;
      taint_arg_index : int;
      returns_this : bool;
    }
  | ThisTaintsReturn of {
      methods : string list;
      arity : int;
    }

(* ========================================================================== *)
(* Unified Language Configuration *)
(* ========================================================================== *)

type t = {
  hof_configs : hof_kind list;
  collection_configs : collection_model_kind list;
  constructor_names : string list;
  uses_new_keyword : bool;
}

(* ========================================================================== *)
(* Language Configurations *)
(* ========================================================================== *)

let empty = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
}

let python = {
  hof_configs = [
    FunctionHOF { functions = ["map"; "filter"]; arity = 2; callback_index = 0; data_index = 1 };
  ];
  collection_configs = [
    (* list.append(item), set.add(item) - item taints this *)
    ArgTaintsThis { methods = ["append"; "add"; "insert"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* list.extend(iterable), dict.update - iterable taints this *)
    ArgTaintsThis { methods = ["extend"; "update"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* list.pop(), dict.get(key), dict.pop(key) - this taints return *)
    ThisTaintsReturn { methods = ["pop"]; arity = 0 };
    ThisTaintsReturn { methods = ["get"; "pop"; "setdefault"]; arity = 1 };
    ThisTaintsReturn { methods = ["get"; "pop"; "setdefault"]; arity = 2 };
    (* Iteration helpers *)
    ThisTaintsReturn { methods = ["copy"; "keys"; "values"; "items"]; arity = 0 };
  ];
  constructor_names = ["__init__"];
  uses_new_keyword = false;
}

let ruby = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "each"; "select"; "filter"; "flat_map"; "collect"; "find"; "detect"];
      arity = 1;
      taint_arg_index = 0;
    };
    ReturningFunctionHOF {
      methods = ["map"; "each"; "select"; "filter"; "flat_map"; "collect"; "find"; "detect"];
    };
  ];
  collection_configs = [
    (* Array.push, Array.<<, Array.append - item taints this *)
    ArgTaintsThis { methods = ["push"; "append"; "unshift"; "prepend"]; arity = 1; taint_arg_index = 0; returns_this = true };
    (* Hash merge!, update taints this *)
    ArgTaintsThis { methods = ["merge!"; "update"]; arity = 1; taint_arg_index = 0; returns_this = true };
    (* Array.pop, Array.shift, Hash.fetch - this taints return *)
    ThisTaintsReturn { methods = ["pop"; "shift"; "first"; "last"]; arity = 0 };
    ThisTaintsReturn { methods = ["fetch"; "dig"; "slice"]; arity = 1 };
    ThisTaintsReturn { methods = ["fetch"; "dig"]; arity = 2 };
    ThisTaintsReturn { methods = ["to_s"; "join"; "flatten"]; arity = 0 };
  ];
  constructor_names = ["initialize"];
  uses_new_keyword = false;
}

let javascript = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "flatMap"; "filter"; "forEach"; "find"; "findIndex"; "some"; "every"];
      arity = 1;
      taint_arg_index = 0;
    };
    MethodHOF { methods = ["reduce"; "reduceRight"]; arity = 2; taint_arg_index = 1 };
  ];
  collection_configs = [
    (* Map.set(key, value) - value taints this, returns this (fluent) *)
    ArgTaintsThis { methods = ["set"]; arity = 2; taint_arg_index = 1; returns_this = true };
    (* Array.push(item) - item taints this, returns new length *)
    ArgTaintsThis { methods = ["push"; "unshift"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* Set.add(item) - item taints this, returns this (fluent) *)
    ArgTaintsThis { methods = ["add"]; arity = 1; taint_arg_index = 0; returns_this = true };
    (* Map.get(key), Array.pop(), Array.shift() - this taints return *)
    ThisTaintsReturn { methods = ["get"]; arity = 1 };
    ThisTaintsReturn { methods = ["pop"; "shift"; "at"]; arity = 0 };
    ThisTaintsReturn { methods = ["at"]; arity = 1 };
    (* String methods that return modified strings *)
    ThisTaintsReturn { methods = ["toString"; "valueOf"; "join"]; arity = 0 };
  ];
  constructor_names = ["constructor"];
  uses_new_keyword = true;
}

let typescript = {
  javascript with
  hof_configs = javascript.hof_configs;
}
(* TypeScript inherits collection_configs from javascript *)

let java = {
  hof_configs = [
    MethodHOF { methods = ["map"; "filter"; "forEach"; "flatMap"]; arity = 1; taint_arg_index = 0 };
  ];
  collection_configs = [
    (* HashMap/Map: put(key, value) - value (arg 1) taints this, returns previous value (not this) *)
    ArgTaintsThis { methods = ["put"; "putIfAbsent"]; arity = 2; taint_arg_index = 1; returns_this = false };
    (* List: add(item) - item (arg 0) taints this, returns boolean *)
    ArgTaintsThis { methods = ["add"; "addFirst"; "addLast"; "push"; "offer"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* List: add(index, item) - item (arg 1) taints this, returns void *)
    ArgTaintsThis { methods = ["add"; "set"]; arity = 2; taint_arg_index = 1; returns_this = false };
    (* StringBuilder: append(str) - str (arg 0) taints this, RETURNS THIS for fluent chaining *)
    ArgTaintsThis { methods = ["append"; "insert"]; arity = 1; taint_arg_index = 0; returns_this = true };
    (* Collection accessors: get(key/index) - this taints return *)
    ThisTaintsReturn { methods = ["get"; "getFirst"; "getLast"; "peek"; "poll"; "pop"; "remove"]; arity = 1 };
    (* No-arg accessors *)
    ThisTaintsReturn { methods = ["toString"; "getFirst"; "getLast"; "peek"; "poll"; "pop"]; arity = 0 };
    (* Iterator: next() - this taints return *)
    ThisTaintsReturn { methods = ["next"]; arity = 0 };
  ];
  constructor_names = ["<init>"];
  uses_new_keyword = true;
}

let kotlin = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "filter"; "forEach"; "flatMap"; "find"; "any"; "all"];
      arity = 0;
      taint_arg_index = 0;
    };
    MethodHOF {
      methods = ["map"; "filter"; "forEach"; "flatMap"; "find"; "any"; "all"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    (* MutableList.add, MutableSet.add - item taints this *)
    ArgTaintsThis { methods = ["add"; "addFirst"; "addLast"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* MutableMap.put(key, value) - value taints this *)
    ArgTaintsThis { methods = ["put"; "putIfAbsent"]; arity = 2; taint_arg_index = 1; returns_this = false };
    (* StringBuilder.append - returns this *)
    ArgTaintsThis { methods = ["append"]; arity = 1; taint_arg_index = 0; returns_this = true };
    (* get, removeAt, removeLast - this taints return *)
    ThisTaintsReturn { methods = ["get"; "getOrNull"; "getOrDefault"]; arity = 1 };
    ThisTaintsReturn { methods = ["first"; "last"; "removeFirst"; "removeLast"]; arity = 0 };
    ThisTaintsReturn { methods = ["toString"]; arity = 0 };
  ];
  constructor_names = ["<init>"; "init"; "constructor"];
  uses_new_keyword = false;
}

let scala = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "filter"; "foreach"; "flatMap"; "find"; "exists"; "forall"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    (* mutable collections: += or add *)
    ArgTaintsThis { methods = ["append"; "prepend"; "addOne"; "add"]; arity = 1; taint_arg_index = 0; returns_this = true };
    (* mutable Map: put(key, value) or update(key, value) *)
    ArgTaintsThis { methods = ["put"; "update"; "addOne"]; arity = 2; taint_arg_index = 1; returns_this = false };
    (* accessors *)
    ThisTaintsReturn { methods = ["head"; "last"; "apply"; "get"]; arity = 0 };
    ThisTaintsReturn { methods = ["apply"; "get"; "getOrElse"]; arity = 1 };
    ThisTaintsReturn { methods = ["mkString"; "toString"]; arity = 0 };
  ];
  constructor_names = ["<init>"];
  uses_new_keyword = false;
}

let csharp = {
  hof_configs = [
    MethodHOF {
      methods = ["Select"; "Where"; "ForEach"; "SelectMany"; "First"; "Any"; "All"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    (* List.Add, HashSet.Add - item taints this *)
    ArgTaintsThis { methods = ["Add"; "Push"; "Enqueue"; "Insert"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* Dictionary.Add(key, value) - value taints this *)
    ArgTaintsThis { methods = ["Add"; "TryAdd"]; arity = 2; taint_arg_index = 1; returns_this = false };
    (* List[i], Dictionary[key], Queue.Dequeue, Stack.Pop - this taints return *)
    ThisTaintsReturn { methods = ["Pop"; "Dequeue"; "Peek"]; arity = 0 };
    ThisTaintsReturn { methods = ["ElementAt"; "GetValueOrDefault"]; arity = 1 };
    ThisTaintsReturn { methods = ["ToString"]; arity = 0 };
  ];
  constructor_names = [".ctor"];
  uses_new_keyword = true;
}

let go = {
  hof_configs = [];
  collection_configs = [
    (* Go uses map indexing m[k]=v, not methods, but for sync.Map *)
    ArgTaintsThis { methods = ["Store"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ThisTaintsReturn { methods = ["Load"]; arity = 1 };
  ];
  constructor_names = [];
  uses_new_keyword = false;
}

let rust = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "for_each"; "filter"; "flat_map"; "find"; "any"; "all"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    (* Vec.push - item taints this *)
    ArgTaintsThis { methods = ["push"; "push_front"; "push_back"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* HashMap.insert(key, value) - value taints this *)
    ArgTaintsThis { methods = ["insert"]; arity = 2; taint_arg_index = 1; returns_this = false };
    (* Vec.pop, HashMap.get, HashMap.remove - this taints return *)
    ThisTaintsReturn { methods = ["pop"; "pop_front"; "pop_back"]; arity = 0 };
    ThisTaintsReturn { methods = ["get"; "get_mut"; "remove"]; arity = 1 };
    ThisTaintsReturn { methods = ["into_iter"; "iter"; "iter_mut"]; arity = 0 };
  ];
  constructor_names = ["new"];
  uses_new_keyword = false;
}

let swift = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "filter"; "forEach"; "flatMap"; "compactMap"; "first"; "contains"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    (* Array.append - item taints this *)
    ArgTaintsThis { methods = ["append"; "insert"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* Dictionary updateValue(value, forKey:) - value taints this *)
    ArgTaintsThis { methods = ["updateValue"]; arity = 2; taint_arg_index = 0; returns_this = false };
    (* Array subscript, popLast, removeFirst - this taints return *)
    ThisTaintsReturn { methods = ["popLast"; "removeFirst"; "removeLast"; "first"; "last"]; arity = 0 };
    ThisTaintsReturn { methods = ["remove"]; arity = 1 };
  ];
  constructor_names = ["init"];
  uses_new_keyword = false;
}

let php = {
  hof_configs = [
    FunctionHOF { functions = ["array_map"]; arity = 2; callback_index = 0; data_index = 1 };
    FunctionHOF { functions = ["array_filter"; "array_walk"]; arity = 2; callback_index = 1; data_index = 0 };
  ];
  collection_configs = [];  (* PHP collections are mostly handled via builtin functions *)
  constructor_names = ["__construct"];
  uses_new_keyword = true;
}

let cpp = {
  hof_configs = [
    FunctionHOF { functions = ["for_each"]; arity = 3; callback_index = 2; data_index = 0 };
    FunctionHOF { functions = ["transform"]; arity = 4; callback_index = 3; data_index = 0 };
  ];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
}

let c = {
  cpp with
  hof_configs = cpp.hof_configs;
}

let ocaml_lang = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
}

let lua = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
}

let dart = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = ["constructor"];
  uses_new_keyword = false;
}

let elixir = {
  hof_configs = [
    FunctionHOF {
      functions = ["Enum.map"; "Enum.each"; "Enum.filter"; "Enum.flat_map"; "Enum.find"];
      arity = 2;
      callback_index = 1;
      data_index = 0;
    };
  ];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
}

let julia = {
  hof_configs = [
    FunctionHOF { functions = ["map"; "foreach"; "filter"]; arity = 2; callback_index = 0; data_index = 1 };
  ];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
}

let clojure = {
  hof_configs = [
    (* Core Clojure HOFs: (map f coll), (filter pred coll), etc. *)
    (* Both unqualified and namespace-qualified versions *)
    FunctionHOF {
      functions = ["map"; "mapv"; "filter"; "filterv"; "keep"; "keep-indexed"; 
                   "map-indexed"; "mapcat"; "remove"; "every?"; "some"; "not-any?"; "not-every?";
                   "clojure.core/map"; "clojure.core/mapv"; "clojure.core/filter"; "clojure.core/filterv";
                   "clojure.core/keep"; "clojure.core/keep-indexed";
                   "clojure.core/map-indexed"; "clojure.core/mapcat"; "clojure.core/remove";
                   "clojure.core/every?"; "clojure.core/some"; "clojure.core/not-any?"; "clojure.core/not-every?"];
      arity = 2;
      callback_index = 0;  (* Callback is first arg *)
      data_index = 1;      (* Data is second arg *)
    };
    (* Apply and partial application *)
    FunctionHOF {
      functions = ["apply"; "partial"; "clojure.core/apply"; "clojure.core/partial"];
      arity = 2;
      callback_index = 0;
      data_index = 1;
    };
    (* Threading HOFs that take a function *)
    FunctionHOF {
      functions = ["iterate"; "repeatedly"; "clojure.core/iterate"; "clojure.core/repeatedly"];
      arity = 2;
      callback_index = 0;
      data_index = 1;
    };
    (* Reduce with 2-arity: (reduce f coll) *)
    FunctionHOF {
      functions = ["reduce"; "clojure.core/reduce"];
      arity = 2;
      callback_index = 0;
      data_index = 1;
    };
    (* Reduce with 3-arity: (reduce f init coll) *)
    FunctionHOF {
      functions = ["reduce"; "reductions"; "clojure.core/reduce"; "clojure.core/reductions"];
      arity = 3;
      callback_index = 0;
      data_index = 2;  (* Collection is third arg when init is provided *)
    };
  ];
  collection_configs = [
    (* Collection accessors that return tainted data from a tainted collection *)
    ThisTaintsReturn { methods = ["first"; "second"; "last"; "peek"]; arity = 0 };
    ThisTaintsReturn { methods = ["nth"; "get"]; arity = 1 };
    ThisTaintsReturn { methods = ["get"]; arity = 2 };
  ];
  constructor_names = [];
  uses_new_keyword = false;
}

let apex = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = ["<init>"];
  uses_new_keyword = true;
}

let vb = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = ["New"];
  uses_new_keyword = true;
}

(* ========================================================================== *)
(* Lookup Function *)
(* ========================================================================== *)

let get (lang : Lang.t) : t =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 -> python
  | Lang.Ruby -> ruby
  | Lang.Js -> javascript
  | Lang.Ts -> typescript
  | Lang.Java -> java
  | Lang.Kotlin -> kotlin
  | Lang.Scala -> scala
  | Lang.Csharp -> csharp
  | Lang.Go -> go
  | Lang.Rust -> rust
  | Lang.Swift -> swift
  | Lang.Php -> php
  | Lang.Cpp -> cpp
  | Lang.C -> c
  | Lang.Ocaml -> ocaml_lang
  | Lang.Lua -> lua
  | Lang.Dart -> dart
  | Lang.Elixir -> elixir
  | Lang.Julia -> julia
  | Lang.Clojure -> clojure
  | Lang.Apex -> apex
  | Lang.Vb -> vb
  | _ -> empty

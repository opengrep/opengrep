(* Per-language taint/call-graph settings (HOF configs, constructor patterns). *)

type hof_result =
  | Input_elements
  | Callback_results
  | Nothing

type hof_kind =
  | MethodHOF of {
      methods : string list;
      arity : int;
      taint_arg_index : int;
      result : hof_result;
    }
  | FunctionHOF of {
      functions : string list;
      arity : int;
      callback_index : int;
      data_index : int;
      taint_arg_index : int;
      result : hof_result;
    }
  | ReturningFunctionHOF of {
      methods : string list;
      result : hof_result;
    }

(* What a collection method does with its argument, or what it returns. *)
type collection_model_kind =
  | ArgIsElement of {
      methods : string list;
      arity : int;
      taint_arg_index : int;
      returns_this : bool;
    }
  | ArgElementsAreElements of {
      methods : string list;
      arity : int;
      taint_arg_index : int;
      returns_this : bool;
    }
  | ReturnsElement of {
      methods : string list;
      arity : int;
    }
  | ReturnsWholeValue of {
      methods : string list;
      arity : int;
    }
  | ReturnsSameElements of {
      methods : string list;
      arity : int;
    }
  | ElementProperty of { properties : string list }
      (* A property whose read gives an element of the receiver. *)

type construction_form =
  | Bare_call
  | New_keyword
  | New_method of string

type method_sets =
  | Shared_by_class_and_instance
  | Separate_for_class_and_instance

type method_dispatch =
  | Dynamic
  | Static
  | Dynamic_when_overridable

type receiver_parameter =
  | Declares_method
  | Declares_extension

type multiple_results =
  | No_multiple_results
  | Declared_result_types
  | Returned_expression_list

type reflection = Lang_reflection.t = {
  callable_literals : bool;
  send_methods : string list;
  method_object : string option;
  attribute_lookup : string option;
  apply_function : string list;
  symbol_lookup : string list;
}

type t = {
  hof_configs : hof_kind list;
  collection_configs : collection_model_kind list;
  constructor_names : string list;
  construction : construction_form;
  method_sets : method_sets;
  method_dispatch : method_dispatch;
  receiver_parameter : receiver_parameter;
  class_is_callable_value : bool;
  constructor_reference_names : string list;
  reflection : reflection;
  block_pass_operator : bool;
  (* Methods invoking `self` as a function (Runnable.run, Proc#call): a Fun-shaped receiver call becomes a direct lambda invocation. *)
  invoke_methods : string list;
  class_accessor_methods : string list;
  (* [true] makes [extract_calls] skip nested fdefs/lambdas; unsafe where they need the enclosing scope ([self] in Python methods). *)
  skip_nested_in_extract_calls : bool;
  implicit_capture_mode : AST_generic.capture_mode;
  (* Go: a function declares multiple result types (specification, "Return statements"); Lua: a return lists multiple expressions (reference manual 3.4.12). *)
  multiple_results : multiple_results;
}

let empty = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Static;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.none;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_value;
  multiple_results = No_multiple_results;
}

let python = {
  hof_configs = [
    FunctionHOF { functions = ["map"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0; result = Callback_results };
    FunctionHOF { functions = ["filter"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0; result = Input_elements };
  ];
  collection_configs = [
    ArgIsElement { methods = ["append"; "add"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgIsElement { methods = ["insert"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgElementsAreElements { methods = ["extend"; "update"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ReturnsElement { methods = ["pop"]; arity = 0 };
    ReturnsElement { methods = ["get"; "pop"; "setdefault"]; arity = 1 };
    ReturnsElement { methods = ["get"; "pop"; "setdefault"]; arity = 2 };
    ReturnsSameElements { methods = ["copy"; "values"]; arity = 0 };
    ReturnsWholeValue { methods = ["keys"; "items"]; arity = 0 };
  ];
  constructor_names = ["__init__"];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic;
  receiver_parameter = Declares_method;
  class_is_callable_value = true;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Python;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let ruby = {
  hof_configs = [
    MethodHOF { methods = ["map"; "flat_map"; "collect"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF {
      methods = ["each"; "select"; "filter"; "find"; "detect"];
      arity = 1;
      taint_arg_index = 0;
      result = Input_elements;
    };
    ReturningFunctionHOF { methods = ["map"; "flat_map"; "collect"]; result = Callback_results };
    ReturningFunctionHOF { methods = ["each"; "select"; "filter"; "find"; "detect"]; result = Input_elements };
  ];
  collection_configs = [
    ArgIsElement { methods = ["push"; "append"; "unshift"; "prepend"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ArgElementsAreElements { methods = ["merge!"; "update"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ReturnsElement { methods = ["pop"; "shift"; "first"; "last"]; arity = 0 };
    ReturnsElement { methods = ["fetch"; "dig"]; arity = 1 };
    ReturnsWholeValue { methods = ["slice"]; arity = 1 };
    ReturnsElement { methods = ["fetch"; "dig"]; arity = 2 };
    ReturnsWholeValue { methods = ["to_s"; "join"; "flatten"]; arity = 0 };
    ReturnsWholeValue { methods = ["join"]; arity = 1 };
  ];
  constructor_names = ["initialize"];
  construction = New_method "new";
  method_sets = Separate_for_class_and_instance;
  method_dispatch = Dynamic;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Ruby;
  block_pass_operator = true;
  invoke_methods = ["call"];
  class_accessor_methods = ["class"];
  (* Safe: RSpec specs are anonymous-lambda nests with no [self.X] inheritance. *)
  skip_nested_in_extract_calls = true;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let crystal = { ruby with reflection = Lang_reflection.of_lang Lang.Crystal }

let javascript = {
  hof_configs = [
    MethodHOF { methods = ["map"; "flatMap"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["filter"; "find"]; arity = 1; taint_arg_index = 0; result = Input_elements };
    MethodHOF { methods = ["forEach"; "findIndex"; "some"; "every"]; arity = 1; taint_arg_index = 0; result = Nothing };
    MethodHOF { methods = ["reduce"; "reduceRight"]; arity = 2; taint_arg_index = 1; result = Callback_results };
  ];
  collection_configs = [
    ArgIsElement { methods = ["set"]; arity = 2; taint_arg_index = 1; returns_this = true };
    ArgIsElement { methods = ["push"; "unshift"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgIsElement { methods = ["add"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ReturnsElement { methods = ["get"]; arity = 1 };
    ReturnsElement { methods = ["pop"; "shift"]; arity = 0 };
    ReturnsElement { methods = ["at"]; arity = 1 };
    ReturnsSameElements { methods = ["valueOf"]; arity = 0 };
    ReturnsWholeValue { methods = ["toString"; "join"]; arity = 0 };
    ReturnsWholeValue { methods = ["join"]; arity = 1 };
  ];
  constructor_names = ["constructor"];
  construction = New_keyword;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Js;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let typescript = {
  javascript with
  hof_configs = javascript.hof_configs;
}

let java = {
  hof_configs = [
    MethodHOF { methods = ["map"; "flatMap"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["filter"]; arity = 1; taint_arg_index = 0; result = Input_elements };
    MethodHOF { methods = ["forEach"]; arity = 1; taint_arg_index = 0; result = Nothing };
  ];
  collection_configs = [
    ArgIsElement { methods = ["put"; "putIfAbsent"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgIsElement { methods = ["add"; "addFirst"; "addLast"; "push"; "offer"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgIsElement { methods = ["add"; "set"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgIsElement { methods = ["append"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ArgIsElement { methods = ["insert"]; arity = 2; taint_arg_index = 1; returns_this = true };
    ReturnsElement { methods = ["get"; "getFirst"; "getLast"; "peek"; "poll"; "pop"; "remove"]; arity = 1 };
    ReturnsElement { methods = ["getFirst"; "getLast"; "peek"; "poll"; "pop"]; arity = 0 };
    ReturnsWholeValue { methods = ["toString"]; arity = 0 };
    ReturnsElement { methods = ["next"]; arity = 0 };
  ];
  constructor_names = ["<init>"];
  construction = New_keyword;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic_when_overridable;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = ["new"];
  reflection = Lang_reflection.of_lang Lang.Java;
  block_pass_operator = false;
  invoke_methods = ["run"; "call"; "apply"; "accept"; "invoke"];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_value;
  multiple_results = No_multiple_results;
}

let kotlin = {
  hof_configs = [
    MethodHOF { methods = ["map"; "flatMap"]; arity = 0; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["filter"; "find"]; arity = 0; taint_arg_index = 0; result = Input_elements };
    MethodHOF { methods = ["forEach"; "any"; "all"]; arity = 0; taint_arg_index = 0; result = Nothing };
    MethodHOF { methods = ["map"; "flatMap"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["filter"; "find"]; arity = 1; taint_arg_index = 0; result = Input_elements };
    MethodHOF { methods = ["forEach"; "any"; "all"]; arity = 1; taint_arg_index = 0; result = Nothing };
  ];
  collection_configs = [
    ArgIsElement { methods = ["add"; "addFirst"; "addLast"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgIsElement { methods = ["put"; "putIfAbsent"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgIsElement { methods = ["append"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ReturnsElement { methods = ["get"; "getOrNull"]; arity = 1 };
    ReturnsElement { methods = ["getOrDefault"]; arity = 2 };
    ReturnsElement { methods = ["first"; "last"; "removeFirst"; "removeLast"]; arity = 0 };
    ReturnsWholeValue { methods = ["toString"]; arity = 0 };
  ];
  constructor_names = ["<init>"; "init"; "constructor"];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic_when_overridable;
  receiver_parameter = Declares_extension;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Kotlin;
  block_pass_operator = false;
  invoke_methods = ["invoke"];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let scala = {
  hof_configs = [
    MethodHOF { methods = ["map"; "flatMap"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["filter"; "find"]; arity = 1; taint_arg_index = 0; result = Input_elements };
    MethodHOF { methods = ["foreach"; "exists"; "forall"]; arity = 1; taint_arg_index = 0; result = Nothing };
  ];
  collection_configs = [
    ArgIsElement { methods = ["append"; "prepend"; "addOne"; "add"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ArgIsElement { methods = ["put"; "update"; "addOne"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ElementProperty { properties = ["head"; "last"] };
    ReturnsElement { methods = ["apply"; "get"; "getOrElse"]; arity = 1 };
    ReturnsWholeValue { methods = ["mkString"; "toString"]; arity = 0 };
  ];
  constructor_names = ["<init>"];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = ["apply"];
  reflection = Lang_reflection.of_lang Lang.Scala;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let csharp = {
  hof_configs = [
    MethodHOF { methods = ["Select"; "SelectMany"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["Where"; "First"]; arity = 1; taint_arg_index = 0; result = Input_elements };
    MethodHOF { methods = ["ForEach"; "Any"; "All"]; arity = 1; taint_arg_index = 0; result = Nothing };
  ];
  collection_configs = [
    ArgIsElement { methods = ["Add"; "Push"; "Enqueue"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgIsElement { methods = ["Insert"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgIsElement { methods = ["Add"; "TryAdd"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ReturnsElement { methods = ["Pop"; "Dequeue"; "Peek"]; arity = 0 };
    ReturnsElement { methods = ["ElementAt"; "GetValueOrDefault"]; arity = 1 };
    ReturnsWholeValue { methods = ["ToString"]; arity = 0 };
  ];
  constructor_names = [".ctor"];
  construction = New_keyword;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic_when_overridable;
  receiver_parameter = Declares_extension;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Csharp;
  block_pass_operator = false;
  invoke_methods = ["Invoke"];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let go = {
  (* Go declares no bare-name HOF configuration, because a bare name would match unrelated calls across the whole corpus.
     Auto-detection handles a function reference passed as an argument. *)
  hof_configs = [];
  collection_configs = [
    ArgIsElement { methods = ["Store"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ReturnsWholeValue { methods = ["Load"]; arity = 1 };
  ];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Static;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Go;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = Declared_result_types;
}

let rust = {
  hof_configs = [
    MethodHOF { methods = ["map"; "flat_map"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["filter"; "find"]; arity = 1; taint_arg_index = 0; result = Input_elements };
    MethodHOF { methods = ["for_each"; "any"; "all"]; arity = 1; taint_arg_index = 0; result = Nothing };
  ];
  collection_configs = [
    ArgIsElement { methods = ["push"; "push_front"; "push_back"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgIsElement { methods = ["insert"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ReturnsElement { methods = ["pop"; "pop_front"; "pop_back"]; arity = 0 };
    ReturnsElement { methods = ["get"; "get_mut"; "remove"]; arity = 1 };
    ReturnsSameElements { methods = ["into_iter"; "iter"; "iter_mut"]; arity = 0 };
  ];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Static;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Rust;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let swift = {
  hof_configs = [
    MethodHOF { methods = ["map"; "flatMap"; "compactMap"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["filter"; "first"]; arity = 1; taint_arg_index = 0; result = Input_elements };
    MethodHOF { methods = ["forEach"; "contains"]; arity = 1; taint_arg_index = 0; result = Nothing };
  ];
  collection_configs = [
    ArgIsElement { methods = ["append"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgIsElement { methods = ["insert"]; arity = 2; taint_arg_index = 0; returns_this = false };
    ArgIsElement { methods = ["updateValue"]; arity = 2; taint_arg_index = 0; returns_this = false };
    ReturnsElement { methods = ["popLast"; "removeFirst"; "removeLast"]; arity = 0 };
    ElementProperty { properties = ["first"; "last"] };
    ReturnsElement { methods = ["remove"]; arity = 1 };
  ];
  constructor_names = ["init"];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic_when_overridable;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = ["init"];
  reflection = Lang_reflection.of_lang Lang.Swift;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let php = {
  hof_configs = [
    FunctionHOF { functions = ["array_map"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0; result = Callback_results };
    FunctionHOF { functions = ["array_filter"]; arity = 2; callback_index = 1; data_index = 0; taint_arg_index = 0; result = Input_elements };
    FunctionHOF { functions = ["array_walk"]; arity = 2; callback_index = 1; data_index = 0; taint_arg_index = 0; result = Nothing };
  ];
  collection_configs = [];
  constructor_names = ["__construct"];
  construction = New_keyword;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Php;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_value;
  multiple_results = No_multiple_results;
}

let cpp = {
  hof_configs = [
    FunctionHOF { functions = ["for_each"]; arity = 3; callback_index = 2; data_index = 0; taint_arg_index = 0; result = Nothing };
    FunctionHOF { functions = ["transform"]; arity = 4; callback_index = 3; data_index = 0; taint_arg_index = 0; result = Callback_results };
  ];
  collection_configs = [];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic_when_overridable;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Cpp;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_value;
  multiple_results = No_multiple_results;
}

let c = {
  cpp with
  hof_configs = cpp.hof_configs;
  method_dispatch = Static;
}

let ocaml_lang = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Ocaml;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_value;
  multiple_results = No_multiple_results;
}

let lua = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Lua;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = Returned_expression_list;
}

let dart = {
  hof_configs = [
    MethodHOF { methods = ["map"; "expand"]; arity = 1; taint_arg_index = 0; result = Callback_results };
    MethodHOF { methods = ["where"; "firstWhere"; "lastWhere"]; arity = 1; taint_arg_index = 0; result = Input_elements };
    MethodHOF {
      methods = ["forEach"; "any"; "every"; "removeWhere"; "retainWhere"];
      arity = 1;
      taint_arg_index = 0;
      result = Nothing;
    };
    (* reduce(combine) - combine(value, element), the element (arg 1) comes
       from the collection *)
    MethodHOF { methods = ["reduce"]; arity = 1; taint_arg_index = 1; result = Callback_results };
  ];
  collection_configs = [
    (* List.add, Set.add, List.addAll, Map.addEntries - item taints this *)
    ArgIsElement { methods = ["add"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgElementsAreElements { methods = ["addAll"; "addEntries"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* List.insert(index, item) - item taints this *)
    ArgIsElement { methods = ["insert"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgElementsAreElements { methods = ["insertAll"]; arity = 2; taint_arg_index = 1; returns_this = false };
    (* StringBuffer.write/writeln/writeAll - str taints this *)
    ArgIsElement { methods = ["write"; "writeln"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgElementsAreElements { methods = ["writeAll"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* accessors - this taints return *)
    ReturnsElement { methods = ["removeLast"]; arity = 0 };
    ReturnsWholeValue { methods = ["toString"; "join"]; arity = 0 };
    ReturnsSameElements { methods = ["toList"; "toSet"]; arity = 0 };
    ReturnsElement { methods = ["removeAt"; "elementAt"]; arity = 1 };
    ReturnsWholeValue { methods = ["remove"; "join"]; arity = 1 };
  ];
  (* Dart constructors are class-named (User.User), which is_constructor
     covers via the class-name equality check *)
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = ["new"];
  reflection = Lang_reflection.of_lang Lang.Dart;
  block_pass_operator = false;
  (* Function objects: f.call(args) invokes the closure f *)
  invoke_methods = ["call"];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let elixir = {
  hof_configs = [
    FunctionHOF {
      functions = ["Enum.map"; "Enum.flat_map"];
      arity = 2;
      callback_index = 1;
      data_index = 0;
      taint_arg_index = 0;
      result = Callback_results;
    };
    FunctionHOF {
      functions = ["Enum.filter"; "Enum.find"];
      arity = 2;
      callback_index = 1;
      data_index = 0;
      taint_arg_index = 0;
      result = Input_elements;
    };
    FunctionHOF {
      functions = ["Enum.each"];
      arity = 2;
      callback_index = 1;
      data_index = 0;
      taint_arg_index = 0;
      result = Nothing;
    };
  ];
  collection_configs = [];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Static;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Elixir;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_value;
  multiple_results = No_multiple_results;
}

let julia = {
  hof_configs = [
    FunctionHOF { functions = ["map"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0; result = Callback_results };
    FunctionHOF { functions = ["filter"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0; result = Input_elements };
    FunctionHOF { functions = ["foreach"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0; result = Nothing };
  ];
  collection_configs = [];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Static;
  receiver_parameter = Declares_method;
  class_is_callable_value = true;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Julia;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let clojure = {
  hof_configs = [
    FunctionHOF {
      functions = ["map"; "keep"; "some"; "mapv"; "mapcat"];
      arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0;
      result = Callback_results;
    };
    FunctionHOF {
      functions = ["filter"; "remove"; "filterv"];
      arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0;
      result = Input_elements;
    };
    FunctionHOF {
      functions = ["every?"];
      arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0;
      result = Nothing;
    };
    FunctionHOF {
      functions = ["reduce"];
      arity = 3; callback_index = 0; data_index = 2; taint_arg_index = 1;
      result = Callback_results;
    };
    FunctionHOF {
      functions = ["reduce"];
      arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 1;
      result = Callback_results;
    };
  ];
  collection_configs = [];
  constructor_names = [];
  construction = Bare_call;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Static;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Clojure;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_value;
  multiple_results = No_multiple_results;
}

let apex = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = ["<init>"];
  construction = New_keyword;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic_when_overridable;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Apex;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_value;
  multiple_results = No_multiple_results;
}

let vb = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = ["New"];
  construction = New_keyword;
  method_sets = Shared_by_class_and_instance;
  method_dispatch = Dynamic_when_overridable;
  receiver_parameter = Declares_method;
  class_is_callable_value = false;
  constructor_reference_names = [];
  reflection = Lang_reflection.of_lang Lang.Vb;
  block_pass_operator = false;
  invoke_methods = [];
  class_accessor_methods = [];
  skip_nested_in_extract_calls = false;
  implicit_capture_mode = AST_generic.Capture_by_reference;
  multiple_results = No_multiple_results;
}

let r = {
  empty with
  reflection = Lang_reflection.of_lang Lang.R;
  implicit_capture_mode = AST_generic.Capture_by_reference;
}

let bash = { empty with implicit_capture_mode = AST_generic.Capture_by_reference }

let jsonnet = { empty with implicit_capture_mode = AST_generic.Capture_by_value }

let lisp = { empty with implicit_capture_mode = AST_generic.Capture_by_reference }

let move = { empty with implicit_capture_mode = AST_generic.Capture_by_value }

let cairo = { empty with implicit_capture_mode = AST_generic.Capture_by_value }

let vue = { empty with implicit_capture_mode = AST_generic.Capture_by_reference }

let get (lang : Lang.t) : t =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 -> python
  | Lang.Ruby -> ruby
  | Lang.Crystal -> crystal
  | Lang.Js -> javascript
  | Lang.Ts -> typescript
  | Lang.Java -> java
  | Lang.Kotlin -> kotlin
  | Lang.Scala -> scala
  | Lang.Csharp -> csharp
  | Lang.Go -> go
  | Lang.Rust -> rust
  | Lang.Swift -> swift
  | Lang.Php
  | Lang.Hack ->
      php
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
  | Lang.R -> r
  | Lang.Bash
  | Lang.Dockerfile ->
      bash
  | Lang.Jsonnet -> jsonnet
  | Lang.Lisp
  | Lang.Scheme ->
      lisp
  | Lang.Move_on_sui
  | Lang.Move_on_aptos ->
      move
  | Lang.Cairo -> cairo
  | Lang.Vue -> vue
  | Lang.Circom
  | Lang.Html
  | Lang.Json
  | Lang.Promql
  | Lang.Protobuf
  | Lang.Ql
  | Lang.Solidity
  | Lang.Terraform
  | Lang.Xml
  | Lang.Yaml ->
      empty

let is_element_property (lang : Lang.t) (name : string) : bool =
  (get lang).collection_configs
  |> List.exists (function
       | ElementProperty { properties } ->
           List.exists (String.equal name) properties
       | ArgIsElement _
       | ArgElementsAreElements _
       | ReturnsElement _
       | ReturnsWholeValue _
       | ReturnsSameElements _ ->
           false)

let uses_new_keyword (lang : Lang.t) : bool =
  match (get lang).construction with
  | New_keyword -> true
  | Bare_call
  | New_method _ -> false

let constructs_by_bare_call (lang : Lang.t) : bool =
  match (get lang).construction with
  | Bare_call -> true
  | New_keyword
  | New_method _ -> false

let construction_method (lang : Lang.t) : string option =
  match (get lang).construction with
  | New_method (name : string) -> Some name
  | Bare_call
  | New_keyword -> None

(* Languages where one scope holds several concrete functions of one name
   and arity told apart by parameter types. Elsewhere such definitions are
   pattern clauses (Elixir, Clojure) or redefinitions, never a group. *)
let overloads_by_type (lang : Lang.t) : bool =
  match lang with
  | Lang.Java
  | Lang.Kotlin
  | Lang.Scala
  | Lang.Csharp
  | Lang.Swift
  | Lang.Cpp
  | Lang.Apex ->
      true
  | _ -> false

type class_declaration =
  | Plain_class
  | Enum_class
  | Record_class
  | Annotation_class
  | Struct_class

type implicit_supertypes = {
  of_every_class : string list list;
  of_declaration : class_declaration -> string list list;
}

let implicit_supertypes (lang : Lang.t) : implicit_supertypes =
  let none (_ : class_declaration) : string list list = [] in
  let unknown = { of_every_class = []; of_declaration = none } in
  match lang with
  | Lang.Java ->
      {
        of_every_class = [ [ "Object" ]; [ "java"; "lang"; "Object" ] ];
        of_declaration =
          (function
          | Enum_class ->
              [
                [ "Enum" ];
                [ "java"; "lang"; "Enum" ];
                [ "Comparable" ];
                [ "java"; "lang"; "Comparable" ];
                [ "Serializable" ];
                [ "java"; "io"; "Serializable" ];
                [ "Constable" ];
                [ "java"; "lang"; "constant"; "Constable" ];
              ]
          | Record_class -> [ [ "Record" ]; [ "java"; "lang"; "Record" ] ]
          | Annotation_class ->
              [ [ "Annotation" ]; [ "java"; "lang"; "annotation"; "Annotation" ] ]
          | Plain_class
          | Struct_class ->
              []);
      }
  | Lang.Kotlin ->
      {
        of_every_class = [ [ "Any" ]; [ "kotlin"; "Any" ] ];
        of_declaration =
          (function
          | Enum_class ->
              [
                [ "Enum" ];
                [ "kotlin"; "Enum" ];
                [ "Comparable" ];
                [ "kotlin"; "Comparable" ];
                [ "Serializable" ];
                [ "java"; "io"; "Serializable" ];
              ]
          | Annotation_class -> [ [ "Annotation" ]; [ "kotlin"; "Annotation" ] ]
          | Plain_class
          | Record_class
          | Struct_class ->
              []);
      }
  | Lang.Apex -> { of_every_class = [ [ "Object" ] ]; of_declaration = none }
  | Lang.Csharp ->
      {
        of_every_class =
          [ [ "object" ]; [ "Object" ]; [ "System"; "Object" ]; [ "dynamic" ] ];
        of_declaration =
          (function
          | Struct_class -> [ [ "ValueType" ]; [ "System"; "ValueType" ] ]
          | Enum_class -> [ [ "Enum" ]; [ "System"; "Enum" ] ]
          | Plain_class
          | Record_class
          | Annotation_class ->
              []);
      }
  | Lang.Scala ->
      {
        of_every_class =
          [
            [ "Any" ];
            [ "scala"; "Any" ];
            [ "AnyRef" ];
            [ "scala"; "AnyRef" ];
            [ "Object" ];
            [ "java"; "lang"; "Object" ];
          ];
        of_declaration =
          (function
          | Record_class ->
              [
                [ "Product" ];
                [ "scala"; "Product" ];
                [ "Serializable" ];
                [ "scala"; "Serializable" ];
                [ "java"; "io"; "Serializable" ];
                [ "Equals" ];
                [ "scala"; "Equals" ];
              ]
          | Plain_class
          | Enum_class
          | Annotation_class
          | Struct_class ->
              []);
      }
  | Lang.Swift -> { of_every_class = [ [ "Any" ]; [ "AnyObject" ] ]; of_declaration = none }
  | Lang.Cpp -> { of_every_class = []; of_declaration = none }
  | _ -> unknown

(* Where a program can declare a user-defined implicit conversion, which
   makes an argument applicable to a parameter of another type. *)
type user_defined_conversions =
  | No_user_defined_conversions
  | Declared_in_source_or_target_class
  | Views_searched_as_implicit_parameters

let user_defined_conversions (lang : Lang.t) : user_defined_conversions =
  match lang with
  (* JLS 5.3: an invocation context converts by identity, widening,
     boxing and unboxing only. Kotlin converts a function literal to a
     functional interface (SAM conversion), Apex widens Integer to Long,
     Double and Decimal and converts between Id and String; no
     declaration in either converts a value to another type. *)
  | Lang.Java
  | Lang.Kotlin
  | Lang.Apex ->
      No_user_defined_conversions
  (* A literal initialises a type that conforms to a standard library
     literal protocol; no declaration converts a value to another type. *)
  | Lang.Swift -> No_user_defined_conversions
  (* C++: converting constructors of the target class ([class.conv.ctor])
     and conversion functions of the source class ([class.conv.fct]).
     C# 10.5.2 and VB.NET conversion operators: the operator converts
     from or to its containing type. *)
  | Lang.Cpp
  | Lang.Csharp
  | Lang.Vb ->
      Declared_in_source_or_target_class
  (* Rust: deref coercion by the [Deref] implementation of the source
     type. PHP: [__toString] of the source class, for a [string]
     parameter in coercive typing mode. Dart: the implicit tear-off of
     the source class's [call] method for a function type. Crystal:
     [to_unsafe] of the source type, for a parameter of a C function. *)
  | Lang.Rust
  | Lang.Php
  | Lang.Dart
  | Lang.Crystal ->
      Declared_in_source_or_target_class
  (* Scala 7.3: a view is an implicit value of function type, or an
     implicit method, found as an implicit parameter is (7.2): among the
     identifiers accessible without a prefix, which include imports, and
     among the implicit members of the companion modules of the classes
     associated with the type (the implicit scope). *)
  | Lang.Scala -> Views_searched_as_implicit_parameters
  (* C 6.5.2.2 converts an argument as if by assignment (6.5.16.1). Go
     assignability, OCaml, Hack, Julia (whose [convert] is not applied to
     arguments; a [ccall] argument is converted to the C argument type),
     TypeScript's structural assignability, Solidity, Circom and Move have
     no declared implicit conversion. *)
  | Lang.C
  | Lang.Go
  | Lang.Ocaml
  | Lang.Hack
  | Lang.Julia
  | Lang.Ts
  | Lang.Solidity
  | Lang.Circom
  | Lang.Move_on_sui
  | Lang.Move_on_aptos ->
      No_user_defined_conversions
  (* Cairo: whether deref coercion (Cairo Book 12.3) applies to a
     function's arguments is not confirmed from the specification. *)
  | Lang.Cairo -> No_user_defined_conversions
  (* Dynamically typed: a parameter's annotation, where the language has
     one, does not convert an argument (Python 8.7: annotations do not
     change the semantics of a function). *)
  | Lang.Bash
  | Lang.Clojure
  | Lang.Elixir
  | Lang.Js
  | Lang.Jsonnet
  | Lang.Lisp
  | Lang.Lua
  | Lang.Python
  | Lang.Python2
  | Lang.Python3
  | Lang.R
  | Lang.Ruby
  | Lang.Scheme
  | Lang.Vue ->
      No_user_defined_conversions
  (* No functions with parameters. *)
  | Lang.Dockerfile
  | Lang.Html
  | Lang.Json
  | Lang.Promql
  | Lang.Protobuf
  | Lang.Ql
  | Lang.Terraform
  | Lang.Xml
  | Lang.Yaml ->
      No_user_defined_conversions

(* Where the supertypes of a type are declared. *)
type supertype_declarations =
  | In_the_type_declaration
  | In_partial_type_declarations
  | Outside_the_type_declaration

let supertype_declarations (lang : Lang.t) : supertype_declarations =
  match lang with
  (* JLS 8.1.4, 8.1.5: the [extends] and [implements] clauses of the class
     declaration. Kotlin specification, "Classifier declaration": the
     supertype specifiers of the declaration. Scala 5.1: the parents of the
     template. C++ [class.derived.general]: the base-clause of the class
     definition. *)
  | Lang.Java
  | Lang.Kotlin
  | Lang.Scala
  | Lang.Cpp ->
      In_the_type_declaration
  (* C# 15.2.7: the base interfaces of a partial type are the union of those
     of its parts. VB.NET partial types: not confirmed from the
     specification. *)
  | Lang.Csharp
  | Lang.Vb ->
      In_partial_type_declarations
  (* Swift, "Adding Protocol Conformance with an Extension": an extension
     adopts a protocol for an existing type. Rust Reference,
     "Implementations": a trait implementation is written anywhere in the
     crate of the trait or of the type. *)
  | Lang.Swift
  | Lang.Rust ->
      Outside_the_type_declaration
  (* Not confirmed from the specification: Ruby and Crystal classes reopened
     with an [include], Go methods declared anywhere in the package,
     TypeScript declaration merging, a JavaScript prototype set by
     [Object.setPrototypeOf], Elixir and Clojure protocol
     implementations, Cairo trait implementations. *)
  | Lang.Ruby
  | Lang.Crystal
  | Lang.Go
  | Lang.Ts
  | Lang.Js
  | Lang.Vue
  | Lang.Elixir
  | Lang.Clojure
  | Lang.Cairo ->
      Outside_the_type_declaration
  (* Not confirmed from the specification: the class, contract or struct
     declaration lists its supertypes (Apex, PHP, Dart, Python, Hack,
     Julia, Solidity, Move), or the language declares no supertypes. *)
  | Lang.Apex
  | Lang.Php
  | Lang.Dart
  | Lang.Python
  | Lang.Python2
  | Lang.Python3
  | Lang.Hack
  | Lang.Julia
  | Lang.Solidity
  | Lang.Move_on_sui
  | Lang.Move_on_aptos
  | Lang.C
  | Lang.Ocaml
  | Lang.Circom
  | Lang.Bash
  | Lang.Jsonnet
  | Lang.Lisp
  | Lang.Lua
  | Lang.R
  | Lang.Scheme
  | Lang.Dockerfile
  | Lang.Html
  | Lang.Json
  | Lang.Promql
  | Lang.Protobuf
  | Lang.Ql
  | Lang.Terraform
  | Lang.Xml
  | Lang.Yaml ->
      In_the_type_declaration

(* The declarations that define a user-defined implicit conversion:
   a C++ converting constructor ([class.conv.ctor]), a C++ conversion
   function ([class.conv.fct]), a C# conversion operator (15.10.4). *)
type conversion_declaration =
  | Converting_constructor
  | Conversion_function
  | Conversion_operator

(* The forms the front ends give these declarations. A converting
   constructor is found among the class's constructors. A C++ conversion
   function carries the identifier [operator], its conversion-function-id
   without the type, which is its return type; a C# [implicit operator]
   carries the reserved member name [op_Implicit]. No other front end
   gives a declared conversion a form listed here. *)
let conversion_declarations (lang : Lang.t) : conversion_declaration list =
  match lang with
  | Lang.Cpp -> [ Converting_constructor; Conversion_function ]
  | Lang.Csharp -> [ Conversion_operator ]
  | _ -> []

let conversion_member_name (declaration : conversion_declaration) :
    string option =
  match declaration with
  | Converting_constructor -> None
  | Conversion_function -> Some "operator"
  | Conversion_operator -> Some "op_Implicit"

(* An [explicit] constructor or conversion function takes no part in the
   copy-initialisation of a parameter ([over.match.copy]); a converting
   constructor takes part when it is callable with the single argument
   that copy-initialisation from one expression supplies ([over.match.copy]). *)
let defines_implicit_conversion (declaration : conversion_declaration)
    (entity : AST_generic.entity option)
    (fdef : AST_generic.function_definition) : bool =
  let has (keyword : AST_generic.keyword_attribute) : bool =
    match entity with
    | Some (entity : AST_generic.entity) ->
        AST_generic_helpers.has_keyword_attr keyword entity.AST_generic.attrs
    | None -> false
  in
  let callable_with_a_single_argument (parameters : AST_generic.parameter list)
      : bool =
    match parameters with
    | AST_generic.Param _ :: rest ->
        List.for_all
          (fun (parameter : AST_generic.parameter) ->
            match parameter with
            | AST_generic.Param { AST_generic.pdefault = Some _; _ } -> true
            | _ -> false)
          rest
    | _ -> false
  in
  (not (has AST_generic.Explicit))
  &&
  match declaration with
  | Converting_constructor ->
      has AST_generic.Ctor
      && callable_with_a_single_argument (Tok.unbracket fdef.AST_generic.fparams)
  | Conversion_function
  | Conversion_operator ->
      true

(* C++ [basic.fundamental] and C 6.2.5: bool, the character types, the
   integer and the floating point types are the arithmetic types, and an
   implicit conversion exists between any two of them ([conv.integral],
   [conv.double], [conv.fpint], [conv.bool]; C 6.3.1). *)
let standard_conversions_between_arithmetic_types (lang : Lang.t) : bool =
  match lang with
  | Lang.C
  | Lang.Cpp ->
      true
  | _ -> false

(* C++ [over.ics.rank], ranking implicit conversion sequences: a standard
   conversion sequence is a better conversion sequence than a user-defined
   conversion sequence. C# ranks two conversions by exact match (12.6.4.6)
   and then by the better conversion target (12.6.4.7), not by their
   kind. *)
let ranks_implicit_conversion_sequences (lang : Lang.t) : bool =
  match lang with
  | Lang.Cpp -> true
  | _ -> false

let member_lookup (lang : Lang.t) : Member_lookup.strategy =
  let single_inheritance (superclass : Member_lookup.superclass)
      ~(interface_bodies_inherited : bool) ~(mixins : Member_lookup.mixins) :
      Member_lookup.strategy =
    Member_lookup.Single_inheritance
      { superclass; interface_bodies_inherited; mixins }
  in
  match lang with
  | Lang.Solidity -> Member_lookup.C3 { bases_listed_most_base_first = true }
  | Lang.Scala -> Member_lookup.Scala_class_linearisation
  | Lang.Ruby
  | Lang.Crystal ->
      Member_lookup.Ruby_ancestor_chain
  | Lang.Java ->
      single_inheritance Member_lookup.Written_as_extends
        ~interface_bodies_inherited:true
        ~mixins:Member_lookup.Applied_in_the_chain
  | Lang.Kotlin ->
      single_inheritance Member_lookup.Class_supertype_specifier
        ~interface_bodies_inherited:true
        ~mixins:Member_lookup.Applied_in_the_chain
  | Lang.Swift ->
      single_inheritance Member_lookup.First_parent_if_class
        ~interface_bodies_inherited:true
        ~mixins:Member_lookup.Applied_in_the_chain
  | Lang.Csharp ->
      single_inheritance Member_lookup.First_parent_if_class
        ~interface_bodies_inherited:false
        ~mixins:Member_lookup.Applied_in_the_chain
  | Lang.Apex
  | Lang.Vb
  | Lang.Dart
  | Lang.Js
  | Lang.Ts
  | Lang.Vue
  | Lang.Lua ->
      single_inheritance Member_lookup.Written_as_extends
        ~interface_bodies_inherited:false
        ~mixins:Member_lookup.Applied_in_the_chain
  | Lang.Php
  | Lang.Hack ->
      single_inheritance Member_lookup.Written_as_extends
        ~interface_bodies_inherited:false
        ~mixins:Member_lookup.Flattened_into_the_class
  | Lang.Go -> Member_lookup.Go_embedding_promotion
  | Lang.Cpp -> Member_lookup.Cpp_member_lookup
  | Lang.Rust -> Member_lookup.Rust_method_probing
  | _ -> Member_lookup.C3 { bases_listed_most_base_first = false }

type dereference = {
  traits : string list list;
  target : string;
}

(* Rust: a method call on a value whose type implements one of [traits]
   continues the method search in the type bound to [target] in that impl. *)
let dereference (lang : Lang.t) : dereference option =
  match lang with
  | Lang.Rust ->
      Some
        {
          traits = [ [ "std"; "ops"; "Deref" ]; [ "core"; "ops"; "Deref" ] ];
          target = "Target";
        }
  | _ -> None

(* Rust 2021: the traits the standard prelude brings into every module. *)
let prelude_traits (lang : Lang.t) : string list list =
  match lang with
  | Lang.Rust ->
      [
        [ "std"; "marker"; "Copy" ];
        [ "std"; "marker"; "Send" ];
        [ "std"; "marker"; "Sized" ];
        [ "std"; "marker"; "Sync" ];
        [ "std"; "marker"; "Unpin" ];
        [ "std"; "ops"; "Drop" ];
        [ "std"; "ops"; "Fn" ];
        [ "std"; "ops"; "FnMut" ];
        [ "std"; "ops"; "FnOnce" ];
        [ "std"; "borrow"; "ToOwned" ];
        [ "std"; "clone"; "Clone" ];
        [ "std"; "cmp"; "PartialEq" ];
        [ "std"; "cmp"; "PartialOrd" ];
        [ "std"; "cmp"; "Eq" ];
        [ "std"; "cmp"; "Ord" ];
        [ "std"; "convert"; "AsRef" ];
        [ "std"; "convert"; "AsMut" ];
        [ "std"; "convert"; "Into" ];
        [ "std"; "convert"; "From" ];
        [ "std"; "convert"; "TryFrom" ];
        [ "std"; "convert"; "TryInto" ];
        [ "std"; "default"; "Default" ];
        [ "std"; "iter"; "Iterator" ];
        [ "std"; "iter"; "Extend" ];
        [ "std"; "iter"; "IntoIterator" ];
        [ "std"; "iter"; "DoubleEndedIterator" ];
        [ "std"; "iter"; "ExactSizeIterator" ];
        [ "std"; "iter"; "FromIterator" ];
        [ "std"; "string"; "ToString" ];
      ]
  | _ -> []

(* Rust: the module path a use path written in [module_path] denotes: from
   the crate root after [crate], from the module after [self], from its
   parent after [super], else from the module. *)
let use_path_from (lang : Lang.t) ~(module_path : string list)
    (path : string list) : string list =
  match (lang, path) with
  | Lang.Rust, "crate" :: rest -> rest
  | Lang.Rust, "self" :: rest -> module_path @ rest
  | Lang.Rust, "super" :: rest -> (
      match List.rev module_path with
      | _ :: parent -> List.rev parent @ rest
      | [] -> rest)
  | _ -> module_path @ path

type metatable = {
  set_metatable : string;
  index_key : string;
}

(* Lua: [set_metatable (t, m)] gives [t] the metatable [m] and returns [t];
   a key missing from [t] is read from the table in [m]'s [index_key]
   field. *)
let metatable (lang : Lang.t) : metatable option =
  match lang with
  | Lang.Lua -> Some { set_metatable = "setmetatable"; index_key = "__index" }
  | _ -> None

(* Swift: a member a protocol extension declares that is not a requirement
   of the protocol is dispatched statically. *)
let extension_members_dispatch_statically (lang : Lang.t) : bool =
  match lang with
  | Lang.Swift -> true
  | _ -> false

let interfaces_are_structural (lang : Lang.t) : bool =
  match lang with
  | Lang.Go -> true
  | _ -> false

let companion_object_has_own_name (lang : Lang.t) : bool =
  match lang with
  | Lang.Scala -> true
  | _ -> false

let super_is_builtin_call (lang : Lang.t) : bool =
  match lang with
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      true
  | _ -> false

let self_is_defining_class (lang : Lang.t) : bool =
  match lang with
  | Lang.Php
  | Lang.Hack ->
      true
  | _ -> false

let is_callable_reference (lang : Lang.t) (name : AST_generic.name) : bool =
  match (lang, name) with
  | ( Lang.Kotlin,
      AST_generic.IdQualified
        {
          AST_generic.name_middle = None;
          name_top = None;
          name_last = _, None;
          _;
        } ) ->
      true
  | _ -> false

let type_name_value_is_instance (lang : Lang.t) : bool =
  match lang with
  | Lang.Rust -> true
  | _ -> false

let method_receiver_is_first_parameter (lang : Lang.t) : bool =
  match lang with
  | Lang.Lua -> true
  | _ -> false

let has_primary_constructor : Lang.t -> bool =
  Visit_function_defs.has_primary_constructor

let bracket_member_access (lang : Lang.t) : bool =
  match lang with
  | Lang.Js
  | Lang.Ts
  | Lang.Lua ->
      true
  | _ -> false

let method_overridable (lang : Lang.t) (attrs : AST_generic.attribute list) :
    bool option =
  let keyword (wanted : AST_generic.keyword_attribute) : bool =
    List.exists
      (fun (attr : AST_generic.attribute) ->
        match attr with
        | AST_generic.KeywordAttr (found, _) ->
            AST_generic.equal_keyword_attribute found wanted
        | _ -> false)
      attrs
  in
  let declared_overridable () : bool =
    keyword AST_generic.Abstract || keyword AST_generic.Virtual
    || keyword AST_generic.Override
  in
  match lang with
  | Lang.Java
  | Lang.Swift ->
      Some
        (not
           (keyword AST_generic.Private || keyword AST_generic.Static
          || keyword AST_generic.Final))
  | Lang.Csharp
  | Lang.Kotlin
  | Lang.Vb ->
      Some ((not (keyword AST_generic.Final)) && declared_overridable ())
  | Lang.Apex -> Some (declared_overridable ())
  | Lang.Cpp ->
      if keyword AST_generic.Final then Some false
      else if declared_overridable () then Some true
      else None
  | _ -> Some false

let hof_method_names (lang : Lang.t) : string list =
  (get lang).hof_configs |> List.concat_map (function
    | MethodHOF { methods; _ }
    | ReturningFunctionHOF { methods; _ } -> methods
    | FunctionHOF _ -> [])

let hof_function_specs (lang : Lang.t) : (string list * int) list =
  (get lang).hof_configs |> List.filter_map (function
    | FunctionHOF { functions; callback_index; _ } ->
      Some (functions, callback_index)
    | MethodHOF _ | ReturningFunctionHOF _ -> None)

(* [a || b] and [a && b] evaluate to one of their operands, not to a
   boolean: ECMA 262 13.13, Python reference 6.11, Lua reference 3.4.5, and
   Ruby's [||], [&&], [or] and [and]. *)
let logical_operators_return_operand (lang : Lang.t) : bool =
  match lang with
  | Lang.Js
  | Lang.Ts
  | Lang.Python
  | Lang.Python2
  | Lang.Python3
  | Lang.Ruby
  | Lang.Lua ->
      true
  | _ -> false

type augmented_assignment =
  | Rebinds
  | Updates_in_place_except of string list
      (** the builtin immutable types, for which it builds a new object *)

(* Python performs [x op= e] in place when the type of [x] supports it
   (reference 7.2.1); elsewhere [x op= e] assigns [x op e] to [x]. *)
let augmented_assignment (lang : Lang.t) : augmented_assignment =
  match lang with
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      Updates_in_place_except
        [ "int"; "float"; "complex"; "bool"; "str"; "bytes"; "tuple"; "frozenset" ]
  | _ -> Rebinds

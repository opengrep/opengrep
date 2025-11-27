import common
import grammar
import json

import ocaml_combinators
import grammar_inspection

def indent(text, n=2):
    prefix = ' ' * n
    return '\n'.join(prefix + line for line in text.splitlines()) + '\n'

ocaml_keywords = {
    "and", "as", "assert", "asr",
    "begin", "class", "constraint", "do",
    "done", "downto", "else", "end",
    "exception", "external", "false", "for",
    "fun", "function", "functor", "if",
    "in", "include", "inherit", "initializer",
    "land", "lazy", "let", "lor",
    "lsl", "lsr", "lxor", "match",
    "method", "mod", "module", "mutable",
    "new", "nonrec", "object", "of",
    "open", "or", "private", "rec",
    "sig", "struct", "then", "to",
    "true", "try", "type", "val",
    "virtual", "when", "while", "with"
}

symbol_names = {
    '(': 'lparen',
    ')': 'rparen',
    '{': 'lbrace',
    '}': 'rbrace',
    '[': 'lbrack',
    ']': 'rbrack',
    ',': 'comma',
    '.': 'dot',
    ':': 'colon',
    ';': 'semi',
    '?': 'qmark',
    '!': 'bang',
    '+': 'plus',
    '-': 'minus',
    '*': 'star',
    '/': 'slash',
    '%': 'percent',
    '=': 'eq',
    '<': 'lt',
    '>': 'gt',
    '&': 'amp',
    '|': 'pipe',
    '^': 'caret',
    '~': 'tilde',
    '#': 'hash',
    '@': 'at',
    '$': 'dollar',
    '`': 'backtick',
    '\\': 'backslash',
    '"': 'dquote',
    "'": 'squote',
}

def replace_symbols(s):
    last_symbol = False
    res = ""
    for ch in s:
        if ch in symbol_names:
            if res:
                res += "_"
            res += symbol_names[ch]
            last_symbol = True
        elif ch.isalnum() or ch == '_':
            if last_symbol:
                res += "_"
                last_symbol = False
            res += ch
    if not res:
        return "token"
    elif res[0].isdigit():
        res = 'token_' + res
        return res
    elif res[0].isupper():
        return res[0].lower() + res[1:]
    else:
        return res

def ocaml_ident(s):
    if s.isidentifier() and s in ocaml_keywords:
        return s + "_"
    else:
        return replace_symbols(s)[:55]

def name_item(item):
    match item["type"]:
        case "rule_ref":
            return ocaml_ident(item["name"])
        case "terminal":
            return ocaml_ident(item["value"])
        case "group":
            return ('_'.join(map(name_item, item["body"]))[:55])
        case "modifier":
            match item["modifier"]:
                case "@lookahead":
                    return "lookahead_" + name_item(item["base"])
                case "@lookahead_not":
                    return "lookahead_not_" + name_item(item["base"])
                case "*" | "+":
                    return name_item(item["base"]) + "s"
                case "?":
                    return name_item(item["base"]) + "_opt"
                case _:
                    raise Exception("Unknown modifier when naming")

def name_prod(prod):
    return '_'.join(map(name_item, prod))

def rename_prod(prod):
    names_so_far = dict()
    for i in prod:
        n = name_item(i)
        if n in names_so_far:
            names_so_far[n] = names_so_far[n] + 1
        else:
            names_so_far[n] = 1
        i["ocaml_name"] = n + str(names_so_far[n])


# parser generator

def to_ocaml_string_literal(s):
    return json.dumps(s).upper()

def generate_item_body(item):
    match item["type"]:
        case "rule_ref":
            return ocaml_ident(item["name"])
       # case "terminal" if item["value"] == "<IDENT>":
       #     return "token_type Identifier"
        case "terminal":
            return f'token {to_ocaml_string_literal(item["value"])}'
        case "group":
            return "\n" + indent(generate_prod(item["body"]))
        case "modifier":
            return generate_item(item["base"])

def modifier_combinator(modifier):
    match modifier:
        case "?":
            return "optional"
        case "*":
            return "list_of"
        case "+":
            return "ne_list_of"
        case "@lookahead":
            return "look_ahead"
        case "@lookahead_not":
            return "look_ahead_not"

def generate_item(item):
    match item["type"]:
        case "rule_ref" | "terminal" | "group":
            return f"let* {item["ocaml_name"]} = {generate_item_body(item)} in\n"
        case "modifier":
            match item["modifier"]:
                case "?" | "*" | "+":
                    # cases for nice formatting
                    match item["base"]["type"]:
                        case "group":
                            return f'let* {item["ocaml_name"]} = {modifier_combinator(item["modifier"])} {generate_item_body(item["base"])}in\n'
                        case "rule_ref":
                            return f'let* {item["ocaml_name"]} = {modifier_combinator(item["modifier"])} {generate_item_body(item["base"])} in\n'
                        case _:
                            return f'let* {item["ocaml_name"]} = {modifier_combinator(item["modifier"])} ({generate_item_body(item["base"])}) in\n'
                case "@lookahead" | "@lookahead_not" :
                    if not "value" in item["base"]:
                        print("LOOKAHEAD ERROR")
                        print(item)
                    return f'let* _ = {modifier_combinator(item["modifier"])} "{item["base"]["value"]}" in\n'
                case _:
                    raise Exception("Unknown modifier (generate_item)")

def item_boilerplate_with_type(name, item):
    match item["type"]:
        case "rule_ref" | "group":
            return name
        case "terminal":
            return f"xToken({name})"
        case "modifier":
            base = item_boilerplate_with_type(name, item["base"])
            if not base:
                return ""
            match item["modifier"]:
                case "?":
                    return f"xOptional({base})"
                case "*" | "+":
                    return f"xList({base})"
                case "@lookahead" | "@lookahead_not" :
                    return ""

def generate_item_boilerplate(item):
    name = item["ocaml_name"]
    match item["type"]:
        case "rule_ref" | "group":
            return name
        case "terminal":
            return f"xToken({name})"
        case "modifier":
            match item["modifier"]:
                case "?":
                    match item["base"]["type"]:
                        case "rule_ref" | "group":
                            return f"xOptional({name})"
                        case "terminal":
                            return f"xOptional(Option.map (fun x -> xToken x) {name})"
                case "+" | "*":
                    match item["base"]["type"]:
                        case "rule_ref" | "group":
                            return f"xList({name})"
                        case "terminal":
                            return f"xList(List.map (fun x -> xToken x) {name})"
                case "@lookahead" | "@lookahead_not"  :
                    return ""

def generate_prod(prod):
    rename_prod(prod)
    res = ""
    for i in prod:
        res += generate_item(i)
    res += 'pure (xGroup(['
    res += "; ".join([i for i in map(generate_item_boilerplate, prod) if i])
    res += ']))\n'
    return "begin\n" + indent(res) + "end\n"

def safe_ocaml_comment(s):
    return s.replace("*)", "* )")

def generate_named_production(rule_name, idx, prod):
    rename_prod(prod)
    res = "(* " + safe_ocaml_comment(f"{rule_name} -> {grammar.show_production(prod)}") + " *)\n"
    for i in prod:
        res += generate_item(i)
    res += f'pure (xRule "{rule_name}" {str(idx)} ['
    res += "; ".join([i for i in map(generate_item_boilerplate, prod) if i])
    res += '])\n'
    return res

def generate_named_production_alt(rule_name, idx, prod):
    res = "begin\n"
    res += indent(generate_named_production(rule_name, idx, prod))
    res += "end;\n"
    return res

def generate_rule(name, ps):
    n = ocaml_ident(name)
    res = ""
    if len(ps) == 1:
        res += generate_named_production(name, 0, ps[0])
    else:
        res += "choice [\n"
        for idx, p in enumerate(ps):
            res += indent(generate_named_production_alt(name, idx, p))
        res += "]\n"
    return res

def ocaml_func_header(i):
    if i == 0:
        return "let rec"
    else:
        return "and"

def generate_grammar(g):
    res = ""
    for idx, (n, ps) in enumerate(g.items()):
        res += f"\n{ocaml_func_header(idx)} {ocaml_ident(n)} : G.any parser = fun __n -> (\n"
        res += indent(generate_rule(n, ps))
        res += ") __n\n"
    return res

g = grammar.from_string("""
  digit: '0' | '1' | '2';
  expr: add_expr | expr (digit digit)? expr | 'sign'? (expr?)?;
  add_expr: mult_expr ('+' add_expr)*;
  mult_expr: base ('*' mult_expr)*;
  base: digit | '(' expr ')';
  """)

def go(f):
    g = grammar.from_file(f)
    if not grammar_inspection.inspect_grammar(g):
        return
    res = ocaml_combinators.text()
    res += "\n(* parser *)\n"
    res += generate_grammar(g)
    with open("Vbnet_parser.ml", "w", encoding="utf-8") as f:
        f.write(res)
    print("DONE!")
    print("parser saved to Vbnet_parser.ml")

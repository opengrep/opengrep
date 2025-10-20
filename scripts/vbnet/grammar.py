import common
from lark import Lark, Transformer, v_args, exceptions
import sys
import re
from pprint import pprint

# parser of antlr grammars

grammar = r"""
    start: rules

    dir_token_type: "%token_type" LINE

    dir_token_match: "%token_match" LINE

    dir_token_ghost: "%token_ghost" LINE

    dir_inline_code: "% " LINE

    rules: rule*

    rule: NAME ":" productions ";"

    productions: production ("|" production)*

    production: modified_item*

    modified_item: base_item modifier?
                 | "@lookahead" "(" production ")" -> lookahead
                 | "@lookahead_not" "(" production ")" -> lookahead_not

    base_item: terminal
             | string_terminal
             | group
             | NAME -> rule_ref

    terminal: /'([^'\\]|\\.)+'/

    string_terminal: /"([^"\\]|\\.)+"/

    group: "(" production ")"

    modifier: MODIFIER

    MODIFIER: "?" | "*" | "+"

    NAME: /[a-zA-Z_][a-zA-Z0-9_]*/

    COMMENT: /\/\/[^\n]*/

    LINE: /[^\n]+/

    %ignore COMMENT
    %import common.WS
    %ignore WS
"""

@v_args(inline=True)
class GrammarTransformer(Transformer):
    def start(self, rules):
        return {r["rule"] : r["productions"] for r in rules}

    def rules(self, *rules):
        return list(rules)

    def rule(self, name, productions):
        return {"rule": name.value, "productions": productions}

    def productions(self, *productions):
        return list(productions)

    def production(self, *items):
        return list(items)

    def base_item(self, item):
        return item

    def modified_item(self, base, modifier=None):
        if modifier:
            return {"type": "modifier", "modifier": modifier, "base" : base}
        return base

    def lookahead(self, production):
        if len(production) == 1:
            return {"type": "modifier", "modifier": "@lookahead", "base": production[0]}
        else:
            return {"type": "modifier", "modifier": "@lookahead", "base": {"type": "group", "body": production}}

    def lookahead_not(self, production):
        if len(production) == 1:
            return {"type": "modifier", "modifier": "@lookahead_not", "base": production[0]}
        else:
            return {"type": "modifier", "modifier": "@lookahead_not", "base": {"type": "group", "body": production}}

    def modifier(self, token):
        return token.value

    def terminal(self, raw):
        value = bytes(raw[1:-1], "utf-8").decode("unicode_escape")
        return {"type": "terminal", "value": value}

    def string_terminal(self, raw):
        value = bytes(raw[1:-1], "utf-8").decode("unicode_escape")
        return {"type": "group",
                "body": [{"type": "terminal", "value": v} for v in value]}

    def rule_ref(self, token):
        return {"type": "rule_ref", "name": token.value}

    def group(self, production):
        return {"type": "group", "body": production}

parser = Lark(grammar, parser="lalr", transformer=GrammarTransformer())

def parse_string(s):
    try:
        return parser.parse(s)
    except exceptions.UnexpectedToken as e:
        if "SEMICOLON" in e.expected:
            common.err("incorrect input grammar")
            print()
            print(f"Syntax error at line {e.line}, column {e.column}: Did you forget a {common.color.PURPLE}';'{common.color.END}?\n")
            print(e.get_context(s))
        else:
            common.err("incorrect input grammar")
            print()
            print(f"Syntax error at line {e.line}, column {e.column}: Unexpected token '{e.token}'. Expected one of: {e.expected}\n")
            print(e.get_context(s))
    except exceptions.UnexpectedInput as e:
        common.err("incorrect input grammar")
        print()
        print(f"Parse error at line {e.line}, column {e.column}: {e}")

def from_file(filename):
    with open(filename, "r") as file:
        contents = file.read()
        return parse_string(contents)

def from_string(s):
    return parse_string(s)

example_grammar_with_parse_error = """
    expr: base | expr '+' expr;
    base: digit+ | '(' expr ')'
    digit: '0' | '1' | '2';
    """

# show grammar

def escape(s):
    escaped = s.encode('unicode_escape').decode('utf-8')
    return "'" + escaped.replace("'", r"\\'") + "'"

def show_item(item):
    match item["type"]:
        case "rule_ref":
            return item["name"]
        case "terminal":
            return escape(item["value"])
        case "group":
            return "(" + ' '.join(map(show_item, item["body"])) + ")"
        case "modifier":
            match item["modifier"]:
                case "@lookahead" | "@lookahead_not" | "@lookback":
                    return item["modifier"] + "(" + show_item(item["base"]) + ")"
                case "*" | "+" | "?":
                    return show_item(item["base"]) + item["modifier"]
                case _:
                    raise Exception("Unknown modifier when printing a grammar")

def show_production(prod):
    return ' '.join(map(show_item, prod))

def show_grammar(g):
    for n, ps in g.items():
        print(n + ":")
        for p in ps:
            print("  | " + show_production(p))

# example

def example():
    g = """Rule1: 'more' Rule2 | 'end';
           Rule2: ('a' 'b')+;"""

    p = from_string(g)
    print("--- Raw:")
    print(g)
    print("--- Parsed:")
    pprint(p)
    print("--- Show grammar:")
    show_grammar(p)


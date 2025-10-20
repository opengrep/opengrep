import common
import grammar
import sys
from pprint import pprint

# self-contained

def get_referenced_rules_item(curr_rule, item):
    match item["type"]:
        case "rule_ref":
            return {(item["name"], curr_rule)}
        case "terminal":
            return set()
        case "group":
            return get_referenced_rules_prod(curr_rule, item["body"])
        case "modifier":
            return get_referenced_rules_item(curr_rule, item["base"])

def get_referenced_rules_prod(curr_rule, prod):
    res = set()
    for i in prod:
        r = get_referenced_rules_item(curr_rule, i)
        res.update(r)
    return res

def get_referenced_rules(g):
    res = set()
    for (r, ps) in g.items():
        for p in ps:
            res.update(get_referenced_rules_prod(r, p))
    return res

def get_unknown_rules(g):
    defed = g.keys()
    refed = get_referenced_rules(g)
    return [(r, from_) for r, from_ in refed if r not in defed]

# check if a rule can produce empty string

def can_empty_item(item, rules_so_far):
    match item["type"]:
        case "rule_ref":
            return item["name"] in rules_so_far
        case "terminal":
            return False
        case "group":
            return can_empty_prod(item["body"], rules_so_far)
        case "modifier":
            match item["modifier"]:
                case "?" | "*" | "+" | "@lookahead" | "@lookahead_not" :
                    return True
                case "+":
                    return can_empty_item(item["base"], rules_so_far)
                case _:
                    raise Exception("Unknown modifier")

def can_empty_prod(prod, rules_so_far):
    return all([can_empty_item(i, rules_so_far) for i in prod])

def can_empty_step(g, rules_so_far):
    new_rules = rules_so_far.copy()
    for r, ps in g.items():
        for p in ps:
            if can_empty_prod(p, rules_so_far):
                new_rules.add(r)
    return new_rules

def can_empty_fixpoint(g, rules_so_far=set()):
    new_rules = can_empty_step(g, rules_so_far)
    if new_rules == rules_so_far:
        return new_rules
    else:
        return can_empty_fixpoint(g, new_rules)

def rules_that_can_produce_empty(g):
    return can_empty_fixpoint(g)

def rules_that_can_produce_empty_example():
    raw = """digit: '0' | '1' | '2' | '3';
             number: digit*;
             char: 'a' | 'b' | 'c';
             string: char | char string |;
             floating: number '.' number;
             expr: number '+' number
                 | number '*' number
                 | string | number | floating;"""

    g = grammar.from_string(raw)["grammar"]
    print("--- Raw grammar:")
    print(raw)
    print("--- Parsed grammar:")
    print(pprint(g))
    print("--- Rules that can produce empty:")
    print(rules_that_can_produce_empty(g))

# check if grammar is left-recursive

def all_splits(lst):
    return [(lst[:i], lst[i], lst[i+1:])
            for i in range(len(lst))]

def leftmost_rule_refs_item(item, produce_empty):
    res = list()
    match item["type"]:
        case "terminal":
            pass
        case "rule_ref":
            n = item["name"]
            m = {"type": "RULE_REF",
                 "name": item["name"]}
            res.append((n,m))
        case "modifier":
            for n, j in leftmost_rule_refs_item(item["base"], produce_empty):
                m = {"type": "modifier",
                     "modifier": item["modifier"],
                     "base": j}
                res.append((n,m))
        case "group":
            res = [(n, {"type": "group", "body": p})
                    for n, p in leftmost_rule_refs_prod(item["body"], produce_empty)]
    return res

def leftmost_rule_refs_prod(prod, produce_empty):
    res = list()
    for pre, i, post in all_splits(prod):
        if not can_empty_prod(pre, produce_empty):
            break
        for n, j in leftmost_rule_refs_item(i, produce_empty):
            res.append((n, pre + [j] + post))
    return res

def leftrec(g, looking_for, rule, visited, produce_empty):
    if visited and looking_for == rule:
        return [[]]
    if rule in visited:
        return []
    prods = g[rule]
    traces = list()
    for p in prods:
        for n, pp in leftmost_rule_refs_prod(p, produce_empty):
            for trace in leftrec(g, looking_for, n, [rule] + visited, produce_empty):
                traces.append([(n, pp)] + trace)
    return traces

def check_leftrec(g):
    produce_empty = rules_that_can_produce_empty(g)
    return {r : leftrec(g, r, r, [], produce_empty) for r in g.keys()}

def is_leftrec(d): #d is result of check_leftrec
    return not all(not v for v in d.values())

gg = """digit: '0' | '1' | '2';
num: digit+;
expr: expr '+' expr | expr '*' expr | base;
base: digit | ('-')? expr;
"""

ggg = grammar.from_string(gg)

def test():
    return leftrec(ggg, "r1", "r1", list(), set())

# display left-recursive

def show_item(item):
    match item["type"]:
        case "rule_ref":
            return item["name"]
        case "RULE_REF":
            return common.color.PURPLE + item["name"] + common.color.END
        case "terminal":
            return grammar.escape(item["value"])
        case "group":
            return "(" + ' '.join(map(show_item, item["body"])) + ")"
        case "modifier":
            match item["modifier"]:
                case "@lookahead" | "@lookahead_not" :
                    return item["modifier"] + "(" + show_item(item["base"]) + ")"
                case "*" | "+" | "?":
                    return show_item(item["base"]) + item["modifier"]
                case _:
                    raise Exception("Unknown modifier when printing a grammar")

def show_production(prod):
    return ' '.join(map(show_item, prod))

def show_trace(t):
    for n, p in t:
        print("-> " + show_production(p))

def explain_leftrec(d): # d is result of check_leftrec
    for r, ts in d.items():
        for t in ts:
            print()
            print(common.color.PURPLE + r + common.color.END)
            show_trace(t)

# inspect grammar

def inspect_grammar(g):
    unknown_rules = get_unknown_rules(g)
    if unknown_rules:
        common.err("there are unknown rules in the grammar")
        print()
        for r, f in unknown_rules:
            print("- unknown rule " +
                  common.color.PURPLE + r + common.color.END +
                  " is mentioned in " +
                  common.color.PURPLE + f + common.color.END)
        print()
        return False

    leftrec = check_leftrec(g)
    if is_leftrec(leftrec):
        common.err("the grammar is left-recursive")
        explain_leftrec(leftrec)
        print()
        return False

    return True

example_ok_grammar = grammar.from_string("""
    expr : mult_expr ('+' expr)*;
    mult_expr: base ('*' mult_expr);
    base: number | '(' expr ')';
    number: digit+;
    digit: '0' | '1' | '2';
    """)

example_grammar_with_unknown_rules = grammar.from_string("""
    expr: number | string | expr '+' epxr;
    number: digit+;
    """)

example_grammar_with_left_recursion = grammar.from_string("""
    expr : add_expr;
    add_expr: expr '+' expr | base;
    base: sign? expr | '(' expr ')' | digit+;
    sign: '+' | '-';
    digit: '0' | '1' | '2';
    """)

example_grammar_with_lookahead_left_recursion = grammar.from_string("""
    expr : @lookahead('+') expr | base;
    base : @lookahead('=') expr | '0';
    """)


# Reachability

def reachable_from_item(item):
    match item["type"]:
        case "rule_ref":
            return {item["name"]}
        case "terminal":
            return {}
        case "group":
            return reachable_from_production(item["body"])
        case "modifier":
            return reachable_from_item(item["base"])

def reachable_from_production(prod):
    return set().union(*map(reachable_from_item, prod))

def reachable_from_rule(g, rule):
    return set().union(*map(reachable_from_production, g[rule]))

def reachable_from(g, rule):
    res = {rule}
    while True:
        new = set().union(*[reachable_from_rule(g, r) for r in res])
        if new <= res:
            break
        else:
            res.update(new)
    return res

def unreachable_from(g, rule):
    return g.keys() - reachable_from(g, rule)

def unreachable_in_file(filename, rule):
    u = unreachable_from(grammar.from_file(filename), rule)
    for d in sorted(u):
        print(d)

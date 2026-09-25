# MATCH:
declare -r a=b

# MATCH:
readonly a=b

declare a=b c=d

################################################################
# A declaration that declares in the current scope becomes a
# variable definition that carries the declare options given to
# it; 'readonly' carries the same option as 'declare -r'. The
# pattern matches only the definitions that carry that option.
################################################################

declare a=b

f() {
  local a=b
}

a=b

# Maybe this should match even though the definition is local to the command.
a=b command

# MATCH:
declare -r a=b c d

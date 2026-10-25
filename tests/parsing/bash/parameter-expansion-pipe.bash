#!/usr/bin/env bash

pair='key|value'
tail=${pair#*|}
tail_longest=${pair##*|}
head=${pair%|*}
head_longest=${pair%%|*}
quoted_tail="${pair#*|}"
default_value=${missing:-left|right}

# A pipe outside parameter expansion must remain a pipeline operator.
printf '%s\n' "$tail" | cat

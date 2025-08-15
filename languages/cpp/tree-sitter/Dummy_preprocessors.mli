(* NOTE:
Parsing of C/C++ happens in two stages: preprocessing and parsing. Preprocessing
does not care about syntax, so the text in between, e.g., #if and #endif does not
need to be proper C syntax. This means that in some cases we cannot simply feed
the file to the parser without first unfolding the macros, which we cannot do
because they depend on the actual context in which we compile, for example:

#ifdef DEBUG
while (i >= 0)
{
  ASSERT(j >= i);
#else
while (i >= 0)
{
#endif
  foo();
  i--;
}

The above can be unfolded to two different programs, depending on the presence of
the DEBUG flag during compilation. We cannot check every possible program that
goes out of preprocessor (combinatorial explosion, but in general there could be
infinitely many of them), and we cannot parse such a non-program before
preprocessing, we try to create *a* program by unfolding the #if, #ifdef, etc
directives.

One approach is to simply erase all conditional directives and hope the result can
still be parsed (the keep_all preprocessor).

Another solution is the "positive" preprocessor, which always picks the first
branch. This means that the program above becomes


while (i >= 0)
{
  ASSERT(j >= i);




  foo();
  i--;
}

We do lose some code, but doing this dummy unfolding is only the last resort
when other parsers end up with unrecoverable errors (partial results are better
than no results).
*)

val keep_all : string -> string

val positive : string -> string


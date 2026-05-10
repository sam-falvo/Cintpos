/*

This file defines a function match that tests whether a pattern
containing the wild cards * and ? matches a given subject string. The
asterisk can match zero or more characters of the subject string and ?
matches just one character of the subject string.

*/

GET "libhdr"

LET match(pat, str) = matchtest(pat, 1, str, 1)

AND matchtest(pat, p, str, s) = VALOF
$( LET patlen = pat%0
   AND strlen = str%0

   //IF FALSE DO
   $( // Debugging output
      writef("matchtest: pat=*""); FOR i = p TO patlen DO wrch(pat%i)
      writef("*"  str=*""); FOR i = s TO strlen DO wrch(str%i)
      writef("*"*n")
   $)

   TEST p > patlen
   THEN $( IF s > strlen RESULTIS TRUE
           RESULTIS FALSE
        $)
   ELSE $( // There is at least one pattern character left.
           LET patch = pat%p

           IF patch='**' DO
           $( IF matchtest(pat, p+1, str, s) RESULTIS TRUE
              IF s > strlen RESULTIS FALSE
              s := s+1
              LOOP
           $)

           IF s > strlen RESULTIS FALSE

           UNLESS patch='?' | patch=str%s RESULTIS FALSE
           // Optimise tail recursion.
           p, s := p+1, s+1
           LOOP
        $)
$) REPEAT

AND try(pat, str) BE
$( writef("*npat=*"%s*"  str=*"%s*"*n",  pat, str)
   TEST match(pat, str)
   THEN writes("Match*n")
   ELSE writef("Do not match*n")
$)

LET start() = VALOF
$( try("",         "")
   try("",         "ABCDEF")
   try("**",       "ABCDEF")
   try("ABCDE",    "ABCDEF")
   try("ABCDEF",   "ABCDEF")
   try("ABCDEFG",  "ABCDEF")
   try("A?CDE?",   "ABCDEF")
   try("A**CDE**", "ABCDEF")
   try("ABCD**",   "ABCDEF")
   try("**C**",    "ABCDEF")
   try("**X**",    "ABCDEF")
   try("**?**",    "ABCDEF")
   try("**?",      "ABCDEF")

   RESULTIS FALSE
$)

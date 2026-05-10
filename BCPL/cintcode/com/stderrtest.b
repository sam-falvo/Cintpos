/*
This program is the test the new features to write text
to the stderr stream.

Implemented by Martin Richards (c) 8 Mar 2025

Provided that this mechanism is found to be useful I plan to modify
programs such as the BCPL compiler to send error messages to stderr. I
am not yet convinced that this is a good idea.

History

08/03/2025
Initial implementation of this program and the new features it uses.
*/

GET "libhdr.h"

LET start() = VALOF
{ LET stdin    = cis
  LET stdout   = cos
  LET stderr   = rootnode!rtn_stderr
  LET err      = findoutput("#")
  LET prevwrch = wrch
  
  newline()
  writef("On entry*n")
  writef("cis=%n findinput(*"*****")=%n rootnode!rtn_keyboard=%n*n",
          cis, findinput("**"),   rootnode!rtn_keyboard)
  writef("cos=%n findoutput(*"*****")=%n rootnode!rtn_screen=%n*n",
          cos, findoutput("**"),   rootnode!rtn_screen)
  writef("findoutput(*"#*")=%n rootnode!rtn_stderr=%n*n",
          findoutput("#"),   rootnode!rtn_stderr)

  newline()
  selectoutput(stderr)
  writef("Written to stderr after selectoutput(rootnode!rtn_stderr)*n")

  selectoutput(findoutput("#"))
  writef("Written to stderr using selectoutput(findoutput(*"#*"))*n")


  wrch := prevwrch
  writes("Written to stdout using wrch*n")
  
  wrch := errwrch
  writes("Written to stderr using errwrch*n")

  wrch := prevwrch

  sawritef("Text written to stdout using sawritef*n")
  errwritef("Text written to stderr using errwritef*n")

  RESULTIS 0
}


  

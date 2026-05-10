GET "libhdr"

LET start() = VALOF
{ // Test the implementation of the TRn debug command.
  LET a = 123
  //sys(Sys_tracing, 20) // Trace some Cintcode instructions
                          // the next time the Cintcode interpreter
			  // is entered.
  //sys(Sys_quit, -1)       // Re-enter the interpreter.
  
  a := start  // Some of this code will be traced
  a := !(@start+500)
  a := 1_000_000
  a := -1_0000_000
  a +:= start
  writef("a=%n*n", a)
  a := 7
  
  //writef("End of test, a=%n*n", a)
  RESULTIS 0
}

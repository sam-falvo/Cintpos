// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

// Program to set the break flags of the specified task

SECTION "break"

GET "libhdr"


LET start() BE
{ LET argv = VEC 20
  LET tasknum = ?
  LET flags = 0
  LET save_res2 = ?

  UNLESS rdargs("task/a,a/s,b/s,c/s,d/s,e/s,all/s", argv, 20) DO
  { writes("Bad args to BREAK*N")
    stop(20)
  }

  tasknum := stringval(argv!0)
  IF argv!1 | argv!6 DO flags := flags | flag_a
  IF argv!2 | argv!6 DO flags := flags | flag_b
  IF argv!3 | argv!6 DO flags := flags | flag_c
  IF argv!4 | argv!6 DO flags := flags | flag_d
  IF argv!5 | argv!6 DO flags := flags | flag_e

  UNLESS flags       DO flags := flag_b // Default is B

  UNLESS setflags(tasknum, flags) DO
  { save_res2 := result2
    TEST result2 = 101
    THEN writef("Task %n does not exist*n", tasknum)
    ELSE writef("BREAK failed*n")
    result2 := save_res2
  }
}


AND stringval(s) = VALOF
{ // converts a string to a number
  LET val = 0

  FOR j = 1 TO s%0 DO
  { UNLESS '0' <= s%j <= '9' DO
    { writef("Invalid char *'%C*' in task number*N", s%j)
      stop(20)
    }
    val := val*10 + s%j - '0'
  }

  RESULTIS val
}

// (C) Copyright 1978 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

/*
15 Jan 82 BJK Modified to improve messages
 4 Dec 01 MR  Minor changes
*/

SECTION "CHANGEPRI"

GET "libhdr"

LET start() = VALOF
{ LET args          = VEC 50
  LET pri, task     = 0, taskid
  LET rdargs_string = "TASK/N/A,PRI=PRIORITY/N"

  UNLESS rdargs(rdargs_string, args, 50) DO
  { writef("Invalid args for key *"%s*"*s", rdargs_string)
    stop(20)
  }

  IF args!1=0 DO
  { args!1 := args!0
    args!0 := 0
  }

  pri := !(args!1)
  IF args!0 DO task := !(args!0)

  UNLESS changepri(task,pri) DO
  { LET res2 = result2
    writes("CHANGEPRI failed*n")
    result2 := res2
  }
  RESULTIS 0
}


SECTION "TASKID"

GET "libhdr"

LET start() BE
{ LET v = VEC 80
  LET form = "Taskid=%n"

  UNLESS rdargs("FORMAT", v, 80) DO
  { writef("Bad argument for TASKID*n")
    stop(20)
  }

  IF v!0 DO form := v!0

  writef(form, taskid)
  newline()
}



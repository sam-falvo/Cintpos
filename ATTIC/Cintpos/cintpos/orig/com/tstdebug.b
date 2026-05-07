GET "libhdr"

LET start() = VALOF
{ LET a=1234
  sys(Sys_tracing, TRUE)
  a := a/10
  a := 2345
  sys(Sys_tracing, FALSE)
  RESULTIS 0
}

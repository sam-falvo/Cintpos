/*
This library provides an approximation to the builtin BCPL Library on
the BBC Micro. This has been modified to be compiled using bbcbcpl32.

*/

let INPUT() = input()
and OUTPUT() = output()
and GETVEC(upb) = getvec(upb)
and FREEVEC(v) be freevec(v)
and STOP(n) be stop(n, 0)
and LOADSEG(name) = loadseg(name)
and GLOBIN(seg) = globin(seg)
and RDARGS(format, v, upb) = rdargs(format, v, upb)
and SELECTINPUT(name) = selectinput(name)
and SELECTOUTPUT(name) = selectoutput(name)
and ENDREAD() be endread()
and ENDWRITE() be endwrite()
and RDCH() = rdch()
and UNRDCH() = unrdch()
and WRCH(ch) be wrch(ch)
and WRITES(s) be writes(s)
and WRITEN(n) be writen(n)
and WRITEF(form, a,b,c,d,e,f,g,h,i,j,k) be writef(form, a,b,c,d,e,f,g,h,i,j,k)
and FINDINPUT(name) = findinput(name)
and FINDOUTPUT(name) = findoutput(name)
and MAXVEC() = GETVEC(100000)
and RDBIN() = binrdch()
and WRBIN(ch) = binwrch(ch)
and NEWLINE() be newline()
and CLOSESTREAM(scb) be endstream(scb)
and CAPCH(ch) = capitalch(ch)
and WRITEWORDS(v, n) BE
{ FOR i = 0 TO 2*n-1 DO WRBIN(v%i)
}
let LEVEL() = level()
let LONGJUMP(p,l) be longjump(p,l)

and GLOBUNIN(seg) BE
{ sawritef("GLOBUNIN called*n")
}

and UNLOADSEG(seg) BE
{ sawritef("UNLOADSEG called*n")
  unloadseg(seg)
}

and DELFILE(filename) BE
{ sawritef("DELFILE called*n")
}

and MOVE(f, t, n) be FOR i = 0 TO n-1 DO
                            t!i := f!i

and MOVEBYTE(f, t, n) be FOR i = 0 TO n-1 DO
                            0%(t+i) := 0%(f+i)

and BACKMOVE(f, t, n) be FOR i = n-1 TO 0 BY -1 DO
                            t!i := f!i

and BACKMVBY(f, t, n) be FOR i = n-1 TO 0 BY -1 DO
                           0%(t+i) := 0%(f+i)



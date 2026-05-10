/*
This library provides an approximation to the builtin BCPL Library on
the BBC Micro.

*/

LET INPUT() = input()
AND OUTPUT() = output()
AND GETVEC(upb) = getvec(upb)
AND FREEVEC(v) BE freevec(v)
AND STOP(n) BE stop(n, 0)
AND LOADSEG(name) = loadseg(name)
AND GLOBIN(seg) = globin(seg)
AND RDARGS(format, v, upb) = rdargs(format, v, upb)
AND SELECTINPUT(name) = selectinput(name)
AND SELECTOUTPUT(name) = selectoutput(name)
AND ENDREAD() BE endread()
AND ENDWRITE() BE endwrite()
AND RDCH() = rdch()
AND UNRDCH() = unrdch()
AND WRCH(ch) BE wrch(ch)
AND WRITES(s) BE writes(s)
AND WRITEN(n) BE writen(n)
AND WRITEF(form, a,b,c,d,e,f,g,h,i,j,k) BE writef(form, a,b,c,d,e,f,g,h,i,j,k)
AND FINDINPUT(name) = findinput(name)
AND FINDOUTPUT(name) = findoutput(name)
AND MAXVEC() = GETVEC(100000)
AND RDBIN() = binrdch()
AND WRBIN(ch) = binwrch(ch)
AND NEWLINE() BE newline()
AND CLOSESTREAM(scb) BE endstream(scb)
AND CAPCH(ch) = capitalch(ch)
AND WRITEWORDS(v, n) BE
{ FOR i = 0 TO 2*n-1 DO WRBIN(v%i)
}
LET LEVEL() = level()
LET LONGJUMP(p,l) BE longjump(p,l)

AND GLOBUNIN(seg) BE
{ //sawritef("GLOBUNIN called*n")
}

AND UNLOADSEG(seg) BE
{ //sawritef("UNLOADSEG called*n")
  unloadseg(seg)
}

AND DELFILE(filename) BE
{ sawritef("DELFILE called*n")
}

AND MOVE(f, t, n) BE FOR i = 0 TO n-1 DO
                            t!i := f!i

AND MOVEBYTE(f, t, n) BE FOR i = 0 TO n-1 DO
                            0%(t+i) := 0%(f+i)

AND BACKMOVE(f, t, n) BE FOR i = n-1 TO 0 BY -1 DO
                            t!i := f!i

AND BACKMVBY(f, t, n) BE FOR i = n-1 TO 0 BY -1 DO
                           0%(t+i) := 0%(f+i)



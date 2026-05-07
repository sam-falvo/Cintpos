SECTION "bgpm"

GET "libhdr"

GLOBAL {
s:200;     t:201;      h:202;          p:203
f:204;     c:205;      e:206;          ch:207
sysin:208; sysout:209; fromstream:210; tostream:211
base:212;  upb:213;    rec_p:214;      rec_l:215
getch:216; putch:217;  wrn:218;        error:219
exp:220;   bexp:221
wrc:222;   wrs:223;    chpos:224
}

MANIFEST {
s_eof = -1; s_eom = -2; s_def = -3; s_set = -4; s_eval = -5
s_lquote = -6; s_rquote = -7

c_call   = '['; c_apply  = ']'; c_sep = '\'; c_skip = '`' 
c_lquote = '{'; c_rquote = '}'; c_arg = '^'
}

LET start() = VALOF
{ LET argv = VEC 40

  IF rdargs("FROM,TO/K,UPB/K/N", argv, 40)=0 DO
  { writes("Bad arguments for BGPM*n"); RESULTIS 20 }

  upb := 20000
  IF argv!2 DO upb := !(argv!2)             // UPB/K/N
  IF upb<500 DO upb := 500
  base := getvec(upb)
  IF base=0 DO
  { writef("Unable to allocate work space (upb = %n)*n", upb)
    RESULTIS 20
  }

  sysin := input()
  fromstream := sysin
  UNLESS argv!0=0 DO                        // FROM
  { fromstream := findinput(argv!0)
    IF fromstream=0 DO
    { writef("Unable to read file %s*n", argv!0); RESULTIS 20 }
  }
  selectinput(fromstream)

  sysout := output()
  tostream := sysout
  UNLESS argv!1=0 DO                        // TO/K
  { tostream := findoutput(argv!1)
    IF tostream=0 DO
    { writef("Unable to write to file %s*n", argv!1)
      UNLESS fromstream=sysin DO endread()
      RESULTIS 20 }
  }
  selectoutput(tostream)

  bgpm()

  UNLESS fromstream=sysin DO endread()
  UNLESS tostream=sysout  DO endwrite()
  selectinput(sysin)
  selectoutput(sysout)
  freevec(base)
  RESULTIS 0
}

AND putch(ch) BE TEST h=0 THEN wrch(ch) ELSE push(ch)

AND push(ch) = VALOF { IF t=s DO error("Insufficient work space")
                       s := s + 1
                       !s := ch
                       RESULTIS s
                     }

AND getch() = c=0 -> rdch(), VALOF { c := c+1; RESULTIS !c }

AND arg(a, n) = VALOF { IF !a<0 DO error("Too few arguments")
                        IF n=0 RESULTIS a
                        a, n := a+!a+1, n-1
                      } REPEAT

AND lookup(a) = VALOF
{ LET q, i, len = e, 0, !a
  UNTIL q=0 | i>len TEST q!(i+2)=a!i THEN i := i+1
                                     ELSE q, i := !q, 0
  IF q=0 DO error("Macro not defined")
  RESULTIS q
}

AND define(name, code) BE
{ LET s1 = s
  push(e); push(t)
  FOR i = 0 TO name%0 DO push(name%i)
  push(1); push(code); push(s_eom)
  UNTIL s=s1 DO { !t := !s; t, s := t-1, s-1 }
  e := t+1
}

AND bgpm(v, n) BE
{ rec_p, rec_l := level(), ret

  s, t, h, p, f, e, c := base-1, base+upb, 0, 0, 0, 0, 0

  define("def",     s_def)
  define("set",     s_set)
  define("eval",    s_eval)
  define("lquote",  s_lquote)
  define("rquote",  s_rquote)
  define("eof",     s_eof)

  { ch := getch()            // Start of main scanning loop.

sw: SWITCHON ch INTO
    { DEFAULT: putch(ch); LOOP

      CASE c_lquote:
      { LET d = 1
        { ch := getch()
          IF ch<0 DO error("Non character in quoted string")
          IF ch=c_lquote DO    d := d+1
          IF ch=c_rquote DO { d := d-1; IF d=0 BREAK }
          putch(ch)
        } REPEAT
        LOOP
      }

      CASE c_call:
        f := push(f); push(h); push(?); push(?)
        h := push(?)
        LOOP

      CASE c_sep:
        IF h=0 DO { putch(ch); LOOP }
        !h := s-h
        h := push(?)
        LOOP

      CASE c_arg:
        IF p=0 DO { putch(ch); LOOP }
        ch := getch()
        { LET a = arg(p+4, rdn())
          FOR q = a+1 TO a+!a DO putch(!q)
        }
        GOTO sw

      CASE c_apply:
      { LET a = f
        IF h=0 DO { putch(ch); LOOP }
        !h := s-h
        push(s_eom)
        f, h := a!0, a!1
        a!0, a!1, a!2, a!3 := p, c, e, t
        { !t := !s; t, s := t-1, s-1 } REPEATUNTIL s<a
           p := t+1
           c := arg(lookup(p+4)+2, 1)
           LOOP
        }

      CASE c_skip:
        ch := getch() REPEATWHILE ch='*s'| ch='*t' | ch='*n'
        GOTO sw

      CASE s_lquote: putch(c_lquote); LOOP
      CASE s_rquote: putch(c_rquote); LOOP
         
      CASE s_eof: RETURN

      CASE s_eom:
ret:    IF p=0 LOOP
        c, e, t := p!1, p!2, p!3
        p       := p!0
        LOOP

      CASE s_def:
      { LET a1 = arg(p+4, 1)
        LET a2 = arg(p+4, 2)
        a2!(!a2+1) := s_eom
        e := a1 - 2
        e!0, e!1 := p!2, p!3
        c,   t   := p!1, e-1
        p        := p!0
        LOOP
      }

      CASE s_set:
      { LET name = arg(p+4, 1)
        LET val  = arg(p+4, 2)
        LET len = !val
        LET a = lookup(name)
        LET b = arg(a+2, 1)
        LET max = a!1 - b - 1  // Max length of the value.
        IF len>max DO error("New value too long")
        FOR i = 0 TO len DO b!i := val!i
        b!(len+1) := s_eom
        GOTO ret
      }

      CASE s_eval:
        c  := arg(p+4, 1)
        wrn(exp(0))
        GOTO ret
    }
  } REPEAT
}

AND rdn() = VALOF { LET val = 0
                    WHILE '0'<=ch<='9' DO { val := 10*val + ch - '0'
                                            ch := getch()
                                          }
                    RESULTIS val
                  }

AND bexp() = VALOF
{ ch := getch()

  SWITCHON ch INTO
  { DEFAULT:
      error("Bad expression")

    CASE '0': CASE '1': CASE '2': CASE '3': CASE '4':
    CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
      RESULTIS  rdn()

    CASE '+': RESULTIS  exp(2)
    CASE '-': RESULTIS -exp(2)

    CASE '(': { LET res = exp(1)
                ch := getch()
                RESULTIS res
              }
  }
}

AND exp(n) = VALOF
{ LET a = bexp()

  { SWITCHON ch INTO
    { DEFAULT:   IF n>1 | n=1 & ch=')' | n=0 & ch=s_eom RESULTIS a
                 error("Bad expression")
      CASE '**': IF n<3 DO { a := a  *  exp(3); LOOP }; RESULTIS a
      CASE '/':  IF n<3 DO { a := a  /  exp(3); LOOP }; RESULTIS a
      CASE '%':  IF n<3 DO { a := a REM exp(3); LOOP }; RESULTIS a
      CASE '+':  IF n<2 DO { a := a  +  exp(2); LOOP }; RESULTIS a
      CASE '-':  IF n<2 DO { a := a  -  exp(2); LOOP }; RESULTIS a
    }
  } REPEAT
}

AND wrn(n) BE { IF n<0 DO { putch('-'); n := -n }
                IF n>9 DO wrn(n/10)
                putch(n REM 10 + '0')
              }

AND wrc(ch) BE
{ IF ch='*n' DO { newline(); chpos := 0; RETURN }
  IF chpos>70 DO wrc('*n')
  UNLESS '*s'<=ch<127 DO ch := '?'  // Assume 7 bit ASCII.
  wrch(ch)
  chpos := chpos+1
}

AND wrs(s) BE FOR i = 1 TO s%0 DO wrc(s%i)

AND error(mess) BE
{ selectoutput(sysout)
  wrs("*nError: "); wrs(mess)
  wrs("*nIncomplete calls: ")
  TEST f=0 THEN wrs("none") ELSE prcall(20, f, h, s)
  wrs("*nActive calls:*n"); btrace(p, 20)
  wrs("Environment:*n");  wrenv(e, 4)
  wrs("End of error message*n")
  selectoutput(tostream)
  longjump(rec_p, rec_l)
}

AND prcall(n, f, h, s) BE UNLESS f=0 TEST n=0
                                     THEN wrs("...")
                                     ELSE { prcall(n-1, !f, f!1, f-1)
                                             !h := s-h
                                             wrcall(f+4, s)
                                          }

AND btrace(p, n) BE
{ IF n=0 DO wrs("...*n")
  IF p=0 | n=0 RETURN
  wrcall(p+4, p!3); wrc(c_apply); wrc('*n')
  p, n := !p, n-1
} REPEAT

AND wrcall(a, b) BE
{ LET sep = c_call
  UNTIL a>=b DO { wrc(sep); wrarg(a)
                  a := a + !a + 1
                  sep := c_sep
                }
}

AND wrarg(a) BE FOR ptr = a+1 TO a + !a DO wrc(!ptr)

AND wrenv(e, n) BE UNLESS n=0 | e=0 DO
{ wrs("Name: ");    wrarg(arg(e+2, 0))
  wrs("  Value: "); wrarg(arg(e+2, 1))
  wrc('*n')
  wrenv(!e, n-1)
}

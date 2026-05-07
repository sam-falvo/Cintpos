MODULE bgpm
 
GET "mcpl.h"
 
STATIC 
  s, t, h, p, f, c, e,
  ch,         errflag,
  base,       upb,
  chpos,
  sysin,      sysout,
  fromstream, tostream,
  workv

MANIFEST Restart=1, Default  // Exceptions
 
MANIFEST
S_eof = -1, S_eom = -2, S_def = -3, S_set = -4, S_eval = -5,
S_lquote = -6, S_rquote = -7,
 
C_call   = '[', C_apply  = ']', C_sep = '\\', C_skip = '`',
C_lquote = '{', C_rquote = '}', C_arg = '^'


FUN start : =>
  LET argv = VEC 40

  IF rdargs("FROM,TO,UPB/K", argv, 40)=0 DO
  { writes("Bad arguments for BGPM\n")
    RETURN 20
  }

  MATCH argv : [arg0, arg1, arg2] =>

    upb := 20000
    UNLESS arg2=0 DO upb := str2numb(arg2)
    IF upb<500 DO upb := 500
    workv := getvec upb
    IF workv=0 DO
    { writef("Insufficient space (%d words)\n", upb)
      RETURN 20
    }

    sysin := input()
    fromstream := sysin
    UNLESS arg0 = 0 DO
    { fromstream := findinput(arg0)
      IF fromstream=0 DO { writef("Trouble with stream \"%s\"\n", arg0)
                           RETURN 20
                         }
    }
 
    sysout := output()
    tostream := sysout
    UNLESS arg1=0 DO
    { tostream := findoutput(arg1)
      IF tostream=0 DO { writef("Trouble with stream \"%s\"\n", arg1)
                         RETURN 20
                       }
    }
 
    selectinput fromstream 
    selectoutput tostream
 
    bgpm(workv, upb)
    HANDLE : n => writef("Exception %d\n", n)
           .
 
    UNLESS fromstream=0 OR fromstream=sysin DO endread()
    UNLESS tostream=0 OR tostream=sysout    DO endwrite()
    selectinput sysin 
    selectoutput sysout
    freevec workv
    writes "\nMacrogeneration complete\n"
    RETURN 0
 
FUN putch : ch => h=0 -> wrch ch, push ch
 
FUN push  : w  =>  IF t=s DO error "Insufficient work space"
                   !+++s := w
                   s

FUN getch : => c=0 -> rdch(), !+++c

 
FUN arg
: [<0], ? => error "Too few arguments"
:    a, 0 => a
:    a, n => arg(@ a!(a!+1), n-1)

 
FUN lookup : a =>
  LET q=e, i=0, len=!a
                 
  UNTIL q=0 | i>len TEST q!(i+2)=a!i THEN i++
                                     ELSE q, i := !q, 0
  IF q=0 DO error "Macro not defined"
  q
 
FUN define
: name, code => LET s1 = s
                push e
                push t
                LET s2 = push 0
                WHILE %name DO push(%name++)
                !s2 := s-s2
                push 1
                push code
                push S_eom
                UNTIL s=s1 DO  !t--- := !s---
                e = t + 1

FUN value
: ch (>='0') (<='9') => ch-'0'
: ch                 => 10+capitalch(ch)-'A'


FUN bgpm : v, n => 
  s, t  := @ v!-1, @ v!n
  h, p, f, e, c  :=  0, 0, 0, 0, 0

  define("def",     S_def)
  define("set",     S_set)
  define("eval",    S_eval)
  define("lquote",  S_lquote)
  define("rquote",  S_rquote)
  define("eof",     S_eof)
  
  { ch := getch()      // start of main scan loop
//writef("ch = %c\n", ch)
    MATCH ch
 
    : C_lquote =>  LET depth = 1
                  { ch := getch()
                    MATCH ch
                    : <0       => error "Non character in quoted string"
                    : C_rquote => IF --depth = 0 BREAK
                    : C_lquote => depth++
                                  putch ch
                  } REPEAT
 
    : C_call  =>  f := push f
                  push(h)
                  push(0)
                  push(0)
                  h := push 0
 
    : C_sep   =>  IF h=0 GOTO Default
                  !h := s-h
                  h := push 0
 
    : C_arg   =>  IF p=0 GOTO Default
                  LET a = arg(p+4, rdn())
                  FOR q = @ a!1 TO @ a!(!a) DO putch(!q)
                  GOTO ch
 
    : C_apply =>  LET a = f
                  MATCH f : [f0, f1, f2, f3] =>
                    IF h=0 GOTO Default
                    !h := s-h
                    push S_eom
                    f, h := f0, f1
                    f0, f1, f2, f3  :=  p, c, e, t
                    !t--- := !s--- REPEATUNTIL s<a
                    p := t+1
                    c := arg(lookup(p+4), 1)
                    LOOP
                  .

    : C_skip  =>  ch := getch() REPEATWHILE ch='\n' OR ch='\s'
                  GOTO ch
 
    : S_eof |
      S_eom   =>  IF c=0 RETURN
                  UNLESS p=0 DO p, c, e, t := p!0, p!1, p!2, p!3
                  LOOP
 
    : S_def   =>  LET a1 = arg(p+4, 1)
                  LET a2 = arg(p+4, 2)
                  a2!(!a2+1) := S_eom
                  e := @ a1!(-2)
                  MATCH p : [pp, cc, ee, tt] =>
                    MATCH e : [:=ee, :=tt] => p, c := pp, cc
                                              LOOP
                    .
                  .
 
    : S_set   =>  LET name = arg(p+4, 1)
                  LET val  = arg(p+4, 2)
                  LET a    = lookup(name)
                  LET b    = arg(a, 1)
                  LET n    = a!1 - b - 1  // Max length of the value
                  IF n>!val DO error "New value too long"
                  FOR i = 0 TO n DO b!i := val!i
                  b!(n+1) := S_eom
                  GOTO S_eom
 
    : S_eval  =>  c  := arg(p+4, 1)
                  ch := getch()
                  wrn(exp 0)
                  GOTO S_eom
 
    :         =>  putch(ch)
                  LOOP
  } REPEAT  

 
FUN rdn
: => LET val = 0
     { ch := getch()
       UNLESS '0'<=ch<='9' RETURN val
       val := 10*val + ch - '0'
     } REPEAT

FUN nexp : n => ch := getch(); exp n

FUN exp
: n  => LET a = bexp()
 
        { MATCH (ch, n)
          : '\s' | '\n' => ch := getch()
          : '*', <2     => a *:= nexp 2
          : '/', <2     => a /:= nexp 2
          : '+', <1     => a +:= nexp 1
          : '-', <1     => a -:= nexp 1
          :             => RETURN a
        } REPEAT

 
FUN bexp : =>
  LET a = 0
 
  { MATCH ch
    : '\s' |
      '\n' |
       '+'  =>  ch := getch()
 
    : >='0'
      <='9' => WHILE '0'<=ch<='9' DO  a, ch := 10*a + ch - '0', getch()
               a
    : '-'   => - nexp 1
    : '('   => a  := nexp 0
               IF ch=')' DO ch := getch()
               a
    :       => 
  } REPEAT

 
 
FUN wrn
: n
  <0 => putch('-'); wrn(-n)
: n
  >9 => wrn(n/10); wrn(n MOD 10)
: n  => putch(n + '0')


FUN wrc
: '*n'    => newline()
             chpos := 0
: >='*s'
  <=127   => IF chpos>70 DO wrc('*n')
             wrch(ch)
             chpos++
:         => wrc '?'


FUN wrs : s => WHILE %s DO wrc(%s++)
 
FUN error
: n => LET mess = [ "",
                   "Out of space",
                   "Argument missing",
                   "Name not found",
                   "Control character or EOF in quoted string"
                 ] ! n

       selectoutput sysout
       wrs "\nError: "
       wrs mess
       wrs "\nIncomplete calls: "
       TEST f=0 THEN wrs "none"
                ELSE prcall(20, f, h, s)
       wrs "\nBacktrace of active calls\n"
       btrace(p, n)
       wrs "End of backtrace\n"
       selectoutput tostream
       RAISE Restart


 
FUN prcall
: ?, 0, ?, ?  => RETURN
: 0, ?, ?, ?  => wrs("...")
: n, f, h, s  => prcall(n-1, !f, f!1, f!(-1))
                 wrcall(@ f!4, h, s)

FUN btrace
: 0,           ? => RETURN
: ?,           0 => wrs("...*n")
: [p,c,e,t,a], n => wrcall(@a, t, 0)
                    wrc C_apply
                    wrc '*n'
                    btrace(p, n-1)

FUN wrcall
: x, y, z => LET sep = C_call
             UNTIL x>=y DO
             { wrc(sep)
               FOR ptr = @ x!1 TO @ x!(!x) DO wrc(!ptr)
               x = @ x!(!x + 1)
               sep = C_sep
             }
             IF z=0 RETURN
             wrc sep
             FOR ptr = @ y!1 TO z DO wrc(!ptr)

 
 
 
 
 
 


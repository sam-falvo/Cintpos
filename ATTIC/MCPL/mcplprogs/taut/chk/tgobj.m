/*
(c)  Martin Richards  30 Oct 1997

This is part of a tautology checker program.

See: chk.m, r3obj.m, r8obj.m and chk.h


This module implements the object: tgobj. It methods are:

Tg_init#(tgobj, checker)

    Initialize the term generator, telling it which the checker to
    send terms to.

Tg_close#(tgobj)

    Close down the term generator object, by freeing all its work space,
    but not its code.

Tg_gen#(tgobj, 0, str)

    Send terms for expression str to the checker.

Tg_gen#(tgobj, 1, n)

    Send terms for test n to the checker.

Tg_gen#(tgobj, 2, filename)

    Send terms derived from Dimacs format file filename to the checker.

*/

GET "chk.h"

STATIC rnobj    // The checker object

FUN start : => writes "This program should be run by: chk\n"
               RETURN 20

STATIC tgfns = [initfn, closefn, genfn]

FUN mkTgobj : => LET tg = TABLE [0, 0, 0]
                 !tg := tgfns
                 RETURN tg

FUN initfn : tg, checker => rnobj := checker

FUN closefn : tg =>

FUN genfn
: tg, 0, str      => gen str
: tg, 1,   n      => selectexp n
: tg, 2, filename => gendimacs  filename
: tg, n, a        => writef("Bad term gen arguments %d %d\n", n, a)

FUN selectexp
:  1 => writef "\nAssociative laws of & and |\n"
        gen "(P & Q) & R  =  P & (Q & R)"
:  2 => gen "(P | Q) | R  =  P | (Q | R)"

:  3 => writef "\nDistributive laws of & and |\n"
        gen "(P & Q) | R  = (P | R) & (Q | R)"
:  4 => gen "(P | Q) & R  = (P & R) | (Q & R)"

:  5 => writef "\nLaws involving implication\n"
        gen "(P|Q -> R) = (P->R) & (Q->R)"
:  6 => gen "(P & Q -> R) = (P-> (Q->R))"
:  7 => gen "(P -> Q & R) = (P->Q)  &  (P->R)"

:  8 => writef "\nClassical theorems\n"
        gen "P | Q  ->  P | ~P & Q"
:  9 => gen "(P->Q)&( ~P->R)  ->  (P&Q | R)"
: 10 => gen "P & Q | ~P & R =  (P->Q) & (~P->R)"
: 11 => gen "(P->Q) | (P->R) = (P -> Q | R)"
: 12 => gen "(P = Q) = (Q = P)"

     // Sample problems from F.J. Pelletier,
     // Seventy-Five Problems for Testing Automatic Theorem Provers,
     // J. Automated Reasoning 2 (1986), 191-216.

: 13 => writef "\nProblem 5\n"
        gen "((P|Q)->(P|R)) -> (P|(Q->R))"

: 14 => writef "\nProblem 9\n"
        gen "((P|Q) & ( ~P | Q) & (P | ~Q)) ->  ~( ~P | ~Q)"

: 15 => writef "\nProblem 12.  Dijkstra's law\n"
        gen "((P  =  Q)  =  R)  ->  (P  =  (Q  =  R))"

: 16 => writef "\nProblem 17\n"
        gen "(P & (Q->R) -> S) = (( ~P | Q | S) & ( ~P | ~R | S))"

: 17 => writef "\nFALSE GOALS\n"
        gen "(P | Q -> R) = (P -> (Q->R))"
: 18 => gen "(P->Q) = (Q ->  ~P)"
: 19 => gen " ~(P->Q) -> (Q = P)"
: 20 => gen "((P->Q) -> Q)  ->  P"
: 21 => gen "((P | Q) & (~P | Q) & (P | ~Q)) ->  ~(~P | Q)"

: 22 => writef "\nIndicates need for subsumption\n"
        gen "((P & (Q = R)) = S) = (( ~P | Q | S) & ( ~P | ~R | S))"

    // Prove that the circuit
    //      -----
    // X --| NOT |--> Y
    //      -----
    // is equivalent to:
    //      -----       -----       -----
    // A --| NOT |--B--| NOT |--C--| NOT |--> D
    //      -----       -----       -----
: 23 => writef "\nProof of the correctness of a circuit\n"
        gen "(Y=~X) & ((D=~C) & (C=~B) & (B=~A)) & (X=A)  ->  (Y=D)"

: 24 => writef "\nSuffix rule test\n"
        gen "((X=A)->B) & (~(A=X)|B)"

// Circuit 2.11 on p38 of "Reasoning in Boolean Networks"
// by Kunz,W and Stoffel,D. Kluwer Academic Press, 1997
: 25 => writef "\nA circuit with a stuck-at-1 un-testable bug\n"
        gen "\n\
            \(y1 =   x1 & k & j)         & \n\
            \(y2 = ~(l  | z3))           & \n\
            \(l  =   z3 & j)             & \n\
            \(k  =   z4 | ~h)            & \n\
            \(j  =   i  | g)             & \n\
            \(i  =   h  & z4)            & \n\
            \(h  =   e  | f)             & \n\
            \(g  =   z1 & z2)            & \n\
            \(f  =   a  | d)             & \n\
            \(e  =   c  & x1)            & \n\
            \(d  = ~(b  & z4))           & \n\
            \(c  = ~(x2 & a))            & \n\
            \(b  = ~(x6 | x7 | x8 | x9)) & \n\
            \(a  =   x3 & x4 & x5 & z3)    \n\

            \&\n\

            \(Y1 =   x1 & K & J)         & \n\
            \(Y2 = ~(L  | z3))           & \n\
            \(L  =   z3 & J)             & \n\
            \(K  =   z4 | ~H)            & \n\
            \(J  = 1)                    & \n\
            \(I  =   H  & z4)            & \n\
            \(H  =   E  | F)             & \n\
            \(G  =   z1 & z2)            & \n\
            \(F  =   A  | D)             & \n\
            \(E  =   C  & x1)            & \n\
            \(D  = ~(B  & z4))           & \n\
            \(C  = ~(x2 & A))            & \n\
            \(B  = ~(x6 | x7 | x8 | x9)) & \n\
            \(A  =   x3 & x4 & x5 & z3)    \n\

            \-> (y1=Y1) & (y2=Y2)    "

: 26 => writef "A circuit whose input P = its output Q\n"
        writef "At each level P = the majority of ai, bi and ci\n"
        writef "At each level ai, bi and ci are randomly rotated\n"
/*
        gen "\n\
            \(a0=P)          & (b0=P)          & (c0=~P)         &\n\
            \(a1=b0&A|c0&~A) & (b1=c0&A|a0&~A) & (c1=a0&A|b0&~A) &\n\
            \(a2=b1&B|c1&~B) & (b2=c1&B|a1&~B) & (c2=a1&B|b1&~B) &\n\
            \(a3=b2&C|c2&~C) & (b3=c2&C|a2&~C) & (c3=a2&C|b2&~C) &\n\
            \(Q=((a3&b3)|(b3&c3)|(c3&a3)))                        \n\
            \-> (P=Q)                                               "
*/
        gen "\n\
            \(E=F)&(F=G)&(G=H)&(H=I)     &\n\
            \(a0=P)          & (b0=P)          & (c0=~P)         &\n\
            \(a1=b0&A|c0&~A) & (b1=c0&A|a0&~A) & (c1=a0&A|b0&~A) &\n\
            \(a2=b1&B|c1&~B) & (b2=c1&B|a1&~B) & (c2=a1&B|b1&~B) &\n\
            \(a3=b2&C|c2&~C) & (b3=c2&C|a2&~C) & (c3=a2&C|b2&~C) &\n\
            \(a4=b3&D|c3&~D) & (b4=c3&D|a3&~D) & (c4=a3&D|b3&~D) &\n\
            \(a5=b4&E|c4&~E) & (b5=c4&E|a4&~E) & (c5=a4&E|b4&~E) &\n\
            \(a6=b5&F|c5&~F) & (b6=c5&F|a5&~F) & (c6=a5&F|b5&~F) &\n\
            \(a7=b6&G|c6&~G) & (b7=c6&G|a6&~G) & (c7=a6&G|b6&~G) &\n\
            \(a8=b7&H|c7&~H) & (b8=c7&H|a7&~H) & (c8=a7&H|b7&~H) &\n\
            \(a9=b8&I|c8&~I) & (b9=c8&I|a8&~I) & (c9=a8&I|b8&~I) &\n\
            \(Q=((a9&b9)|(b9&c9)|(c9&a9)))                        \n\
            \-> (P=Q)                                               "

:    => writef "Test number out of range\n"


FUN gendimacs : filename =>
  writef "\nreading Dimacs format file\n"

/* A typical file starts like this:

c FILE: aim-50-1_6-no-1.cnf
c
c SOURCE: Kazuo Iwama, Eiji Miyano (miyano@cscu.kyushu-u.ac.jp),
c          and Yuichi Asahiro
c
c DESCRIPTION: Artifical instances from generator by source.  Generators
c              and more information in sat/contributed/iwama.
c
c NOTE: Not Satisfiable
c
p cnf 50 80
16 23 42 0
-16 23 42 0
26 41 -42 0
-26 41 -42 0
*/

  LET oldin = input()
  LET data = findinput filename

  UNLESS data DO { writef("Unable to open file: %s\n", filename)
                   RETURN
                 }
  selectinput data

  // 
  LET ch = rdch()
  LET k = 0
  LET v = VEC 100
  LET lineno = 1

  nid := 3  // Generated ids must be odd.

  MATCH ch
  : ' ' | '\t'     => ch := rdch()

  : '\n' | Endstreamch
                   => IF k DO { cnfterm(v, k-1); k := 0 }
                      IF ch=Endstreamch BREAK
                      lineno++
                      ch := rdch()

  : 'c' | 'p'      => ch := rdch()
                      REPEATUNTIL ch='\n' OR ch=Endstreamch

  : '0'..'9' | '-' => LET neg = ch='-'
                      LET a = 0
                      IF neg DO ch := rdch()
                      WHILE '0'<=ch<='9' DO { a := 10*a+ch-'0'
                                              ch := rdch()
                                            }
                      v!++k := neg -> -a, a

  :                => writef("Unexpected ch: (%2x) %c on line %d\n",
                                              ch, ch, lineno)
                      ch := rdch()
  . REPEAT

  endread()
  selectinput oldin

STATIC nid

FUN cnfterm : v, n =>
  FOR i = 1 TO n DO
  { LET var = 2*ABS(v!i)+2

    IF v!i<0 DO { Rn_AddTerm3#(rnobj, NotY, nid, var, 0)
                  var := nid
                  nid +:= 2
                }
    v!i := var
  }

  LET t = n>1 -> v!1, 0
  
  
  FOR i = 2 TO n-1 DO { Rn_AddTerm3#(rnobj, Or, nid, v!i, t)
                        t := nid
                        nid +:= 2
                      }
  Rn_AddTerm3#(rnobj, Or, 1, v!n, t)



//********************* Syntax Analyser ******************

// This converts the ASCII representation of a propositional
// expression into a set of terms, each of the form
//        [op, x, y, z]
// where x, y, and z are variable ids
// It returns the id of the root of the entire expression.

// 0 .. 1   -->  0 .. 1
// A .. Z   -->  all
// a .. z   -->    distinct
// A0 .. Z9 -->      even
// a0 .. z9 -->         numbers
// ~x       -->  [NotY, t, x, 0]
// x & y    -->  [And,  t, x, y]
// x | y    -->  [Or,   t, x, y]
// x -> y   -->  [Imp,  t, x, y]
// x = y    -->  [Eqv,  t, x, y]

MANIFEST
  Id, Lparen, Rparen,Eof  // Lexical tokens

FUN relstr
: NotY => "NotY"
: And  => "And "
: Nand => "Nand"
: Or   => "Or  "
: Nor  => "Nor "
: Imp  => "Imp "
: Eqv  => "Eqv "
:      => "??? "

STATIC
  varn=0, lasteid=0

FUN mkterm : rel, y, z => varn +:= 2
                          pushterm(rel, varn, y, z)
                          RETURN varn

FUN pushterm : rel, x, y, z =>
  IF debug>0 DO writef("Term: %s %5d %5d %5d\n", relstr rel, x, y, z)
  IF rnobj DO Rn_AddTerm3#(rnobj, rel, x, y, z)

FUN parse : str, ts =>
  lex_init str
  varn  := 1       // Generated  ids are odd,  next available is 3
  lasteid := 2     // Expression ids are even, next available is 4
                   // Remember: 0 = False
                   //           1 = True
                   //           2 = Don't care, does not matter which
                   //           even>2 -- user variable
                   //           odd>1  -- system generated variable

  pushterm(Eqv, 1, nexp 0, 0) // Parse the given expression
                              // test if it is FALSE

STATIC
  strp, ch, nch, token, lexval

FUN lex_init : str => strp := str; rch(); rch()

FUN rch : => ch, nch := nch, %strp
             UNLESS nch=0 DO strp++

FUN lex : => MATCH (ch, nch)

: ' ' | '\n' => rch(); lex()

: '0'..'2'   => token, lexval := Id, ch-'0';       rch() // 0, 1 or 2

: 'A'..'Z'|                                     // Variables A0 to z9
  'a'..'z', '0'..'9' => token, lexval := Id, nch<<8 | ch
                        rch(); rch()
: 'A'..'Z'|                                     // Variables A to z
  'a'..'z'   => token, lexval := Id, ch; rch()

: '('        => token := Lparen; rch()
: ')'        => token := Rparen; rch()
: '~'        => token := NotY;   rch()
: '&'        => token := And;    rch()
: '|'        => token := Or;     rch()
: '='        => token := Eqv;    rch()

: '-', '>'   => token := Imp; rch(); rch()

: 0          => token := Eof

:            => RAISE E_Syntax

FUN prim
: => MATCH token
     : Id     => LET a = lexval
                 // Variable A to z and A0 to z9 must be even
                 IF a>2 DO a +:= a
                 IF lasteid<a DO lasteid := a
                 lex()
                 a
     : Lparen => LET a = nexp 0
                 UNLESS token=Rparen RAISE(E_Syntax, "')' missing\n")
                 lex()
                 a
     : NotY   => mkterm(NotY, nexp 4, 0)
     :  ?     => RAISE(E_Syntax, "Bad expression\n")

FUN nexp : n => lex(); exp n

FUN exp : n =>
  LET a = prim()

  MATCH (token, n)
  : And, <4 => a := mkterm(And, a, nexp 4) 
  : Or,  <3 => a := mkterm(Or , a, nexp 3) 
  : Eqv, <2 => a := mkterm(Eqv, a, nexp 2) 
  : Imp, <1 => a := mkterm(Imp, a, nexp 1) 
  :         => RETURN a
  . REPEAT

FUN gen : e => 
  { writef("\nTesting: %s\n", e)

    parse e  // Puts the terms representing expression e
             // into the given term set, and updates its
             // vmax field.

    RETURN TRUE

  } HANDLE : E_Syntax, mess, a => writef(mess, a)
           : E_Space,  mess, a => writef(mess, a)
           .
  RETURN FALSE


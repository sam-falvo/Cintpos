GET "mcpl.h"

MANIFEST

// Peg codes (cunningly chosen to ease reflection about A-E-M)

                               A=3<<18,
                          B=1,          C=2,
                 D=1<<3,       E=3<<21,      F=2<<3,
            G=1<<6,       H=1<<9,       I=2<<9,       J=2<<6, 
   K=1<<12,      L=1<<15,      M=3<<24,      N=2<<15,      O=2<<12,

All       =              A         +
                       B + C       +
                     D + E + F     +
                   G + H + I + J   +
                 K + L + M + N + O ,

Initpos   = All - A,   // Initial position

Hashtabsize = 541

STATIC
  spacev, spacep, poslist, hashtab, ways

FUN start : => 
  spacev := getvec 50000  // It uses 2012 words
  spacep := spacev
  hashtab := getvec(Hashtabsize-1)
  FOR i = 0 TO Hashtabsize-1 DO hashtab!i := 0

  poslist := 0
  addpos(Initpos, 1)

  FOR i = 1 TO 6 DO 
  { LET p = poslist
    poslist := 0
    scanlist(p, addpos)
    writef("number of positions is %d\n", len poslist)
  }

  ways := 0
  scanlist(poslist, addways)
  writef("Number of solutions = %d\n", ways)

  freevec hashtab
  freevec spacev

FUN len
: 0                     => 0
: [chain, pos, k, next] => 1 + len next

FUN scanlist : p, f =>
  WHILE p MATCH p : [chain, pos, k, next] =>
  { LET poss = pos XOR All
    WHILE poss DO // Iterate through the unoccupied positions
    { LET bit = poss & -poss
      poss -:= bit
//writef("poss=%9o  bit=%9o\n", poss, bit); abort 1234
      MATCH bit
      : =A&A<<1 => LOOP
      : =A&A>>1 => IF pos&(B+D)=(B+D) DO f(pos XOR (D+B+A), k)
             IF pos&(F+C)=(F+C) DO f(pos XOR (F+C+A), k)
      : B => IF pos&(G+D)=(G+D) DO f(pos XOR (G+D+B), k)
             IF pos&(I+E)=(I+E) DO f(pos XOR (I+E+B), k)
      : C => IF pos&(H+E)=(H+E) DO f(pos XOR (H+E+C), k)
             IF pos&(J+F)=(J+F) DO f(pos XOR (J+F+C), k)
      : D => IF pos&(F+E)=(F+E) DO f(pos XOR (F+E+D), k)
             IF pos&(A+B)=(A+B) DO f(pos XOR (A+B+D), k)
             IF pos&(K+G)=(K+G) DO f(pos XOR (K+G+D), k)
             IF pos&(M+H)=(M+H) DO f(pos XOR (M+H+D), k)
      : =E&E<<1 => LOOP
      : =E&E>>1 => IF pos&(L+H)=(L+H) DO f(pos XOR (L+H+E), k)
             IF pos&(N+I)=(N+I) DO f(pos XOR (N+I+E), k)
      : F => IF pos&(A+C)=(A+C) DO f(pos XOR (A+C+F), k)
             IF pos&(D+E)=(D+E) DO f(pos XOR (D+E+F), k)
             IF pos&(M+I)=(M+I) DO f(pos XOR (M+I+F), k)
             IF pos&(O+J)=(O+J) DO f(pos XOR (O+J+F), k)
      : G => IF pos&(I+H)=(I+H) DO f(pos XOR (I+H+G), k)
             IF pos&(B+D)=(B+D) DO f(pos XOR (B+D+G), k)
      : H => IF pos&(J+I)=(J+I) DO f(pos XOR (J+I+H), k)
             IF pos&(C+E)=(C+E) DO f(pos XOR (C+E+H), k)
      : I => IF pos&(B+E)=(B+E) DO f(pos XOR (B+E+I), k)
             IF pos&(G+H)=(G+H) DO f(pos XOR (G+H+I), k)
      : J => IF pos&(C+F)=(C+F) DO f(pos XOR (C+F+J), k)
             IF pos&(H+I)=(H+I) DO f(pos XOR (H+I+J), k)
      : K => IF pos&(M+L)=(M+L) DO f(pos XOR (M+L+K), k)
             IF pos&(D+G)=(D+G) DO f(pos XOR (D+G+K), k)
      : L => IF pos&(N+M)=(N+M) DO f(pos XOR (N+M+L), k)
             IF pos&(E+H)=(E+H) DO f(pos XOR (E+H+L), k)
      : =M&M<<1 => LOOP
      : =M&M>>1 => IF pos&(O+N)=(O+N) DO f(pos XOR (O+N+M), k)
             IF pos&(F+I)=(F+I) DO f(pos XOR (F+I+M), k)
             IF pos&(D+H)=(D+H) DO f(pos XOR (D+H+M), k)
             IF pos&(K+L)=(K+L) DO f(pos XOR (K+L+M), k)
      : N => IF pos&(E+I)=(E+I) DO f(pos XOR (E+I+N), k)
             IF pos&(L+M)=(L+M) DO f(pos XOR (L+M+N), k)
      : O => IF pos&(F+J)=(F+J) DO f(pos XOR (F+J+O), k)
             IF pos&(M+N)=(M+N) DO f(pos XOR (M+N+O), k)
    }
    p := next
  }

FUN symmetric : pos => pos = (pos<<1 | pos>>1) & All

FUN minreflect : pos => LET rpos = (pos<<1 | pos>>1) & All
                        IF pos<=rpos RETURN pos
                        RETURN rpos

FUN addpos : pos, k =>
  pos := minreflect pos
  LET hashval = pos MOD Hashtabsize
  LET p = hashtab!hashval
  WHILE p MATCH p : [chain, =pos, n, ?] => n +:= k; RETURN
                  : [chain,    ?, ?, ?] => p := chain
                  .
  p := mk4(hashtab!hashval, pos, k, poslist)
  hashtab!hashval := p
  poslist         := p

FUN lookup : pos =>
  pos := minreflect pos
  LET hashval = pos MOD Hashtabsize
  LET p = hashtab!hashval
  WHILE p MATCH p : [    ?, =pos, n, ?] => RETURN n
                  : [chain,    ?, ?, ?] => p := chain
                  .
  RETURN 0

FUN addways : pos, k => 
  TEST symmetric pos
  THEN ways +:= k * lookup(pos XOR All)
  ELSE ways +:= k * lookup(pos XOR All) / 2

FUN mk4 : a, b, c, d => LET res = spacep
                        !spacep+++ := a
                        !spacep+++ := b
                        !spacep+++ := c
                        !spacep+++ := d
                        RETURN res





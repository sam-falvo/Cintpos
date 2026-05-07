GET "mcpl.h"

MANIFEST

// Peg bits

                               A=3<<18,
                          B=1,          C=2,
                 D=1<<3,       E=3<<21,      F=2<<3,
            G=1<<6,       H=1<<9,       I=2<<9,       J=2<<6, 
   K=1<<12,      L=1<<15,      M=3<<24,      N=2<<15,      O=2<<12,



All       =              A         +
                       B + C       +
                     D + E + F     +
                   G + H + I + J   +
                 K + L + M + N + O 


STATIC
  spacev, spacep, postree, ways=0,
  triples = [[A+B,D], [B+D,G], [D+G,K], [C+E,H], [E+H,L], [F+I,M],
             [D+B,A], [G+D,B], [K+G,D], [H+E,C], [L+H,E], [M+I,F],
             [O+J,F], [J+F,C], [F+C,A], [N+I,E], [I+E,B], [M+H,D],
             [F+J,O], [C+F,J], [A+C,F], [E+I,N], [B+E,I], [D+H,M],
             [K+L,M], [L+M,N], [M+N,O], [G+H,I], [H+I,J], [D+E,F],
             [M+L,K], [N+M,L], [O+N,M], [I+H,G], [J+I,H], [F+E,D]]

FUN mk4 : a, b, c, d => LET res = spacep
                        !spacep+++ := a
                        !spacep+++ := b
                        !spacep+++ := c
                        !spacep+++ := d
                        RETURN res

FUN start : => 
  spacev := getvec 50000
  spacep := spacev

  postree := mk4(       A         +           // [ pos, k, left, right ]
                      0 + C       +
                    0 + E + F     +
                  G + H + I + J   +
                K + L + M + N + O , 1, 0, 0)

  scan(); writef("number of positions is %d\n", len postree)
  scan(); writef("number of positions is %d\n", len postree)
  scan(); writef("number of positions is %d\n", len postree)
  scan(); writef("number of positions is %d\n", len postree)
  scan(); writef("number of positions is %d\n", len postree)

  lastscan()

  writef("Number of solutions = %d\n", 2*ways)
  freevec spacev

FUN len
: 0                   => 0
: [?, ?, left, right] => 1 + len left + len right

FUN scan : =>
  LET tree = postree
  postree := 0
  scantree tree

FUN scantree
: 0 => RETURN

: [pos, k, left, right] =>
      FOR i = 0 TO 35 DO
      { MATCH triples!i : [p12, p3] =>
          IF pos & p12 = p12 AND 
             pos & p3  = 0   DO newpos(pos XOR (p12+p3), k)
      }
      scantree left
      scantree right


FUN minreflect : pos => LET rpos = (pos<<1 | pos>>1) & All
                        IF pos<=rpos RETURN pos
                        RETURN rpos

FUN newpos : pos, k => addpos(@postree, minreflect pos, k)

FUN addpos
: [tree (=0)], pos, k => tree := mk4(pos, k, 0, 0)
: [tree],      pos, k => MATCH tree 
                         : [>pos, ?, left, ?]  => addpos(@ left,  pos, k)
                         : [<pos, ?, ?, right] => addpos(@ right, pos, k)
                         : [   ?, k1, ?, ?]    => k1 +:= k

FUN lastscan : =>
  ways := 0
  lastscantree postree

FUN lastscantree
: 0 => RETURN

: [pos, k, left, right] =>
      FOR i = 0 TO 35 DO
      { MATCH triples!i : [p12, p3] =>
          IF pos & p12 = p12 AND 
             pos & p3  = 0   DO lastnewpos(pos XOR (p12+p3), k)
      }
      lastscantree left
      lastscantree right


FUN lastnewpos : pos, k =>
  ways +:= k * lookup(minreflect (pos XOR All), postree)

FUN lookup
: ?,                        0 => RETURN 0
: pos, [>pos, ?, left,     ?] => RETURN lookup(pos, left)
: pos, [<pos, ?,    ?, right] => RETURN lookup(pos, right)
: ?,   [   ?, k,    ?,     ?] => RETURN k






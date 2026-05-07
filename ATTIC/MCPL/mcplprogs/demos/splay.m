MODULE splay   // A demonstration of splay trees implemented
               // in MCPL by Martin Richards
GET "mcpl.h"

MANIFEST  Key, Val, Parent, Left, Right,   // tree node selectors
          Size                             // tree node size

GLOBAL    spacev:Ug, spacep, line


FUN prtree1 : t => line := VEC 100
                  newline()
                  prt(Left, t, 1)
.
FUN prt
: side,                                0, indent => RETURN

: side, n[key, val, parent, left, right], indent =>
    line!indent := side=Left
    prt(Right, right, indent+1)
    FOR i = 1 TO indent-1 DO writes(line!i->"| ", "  ")
    writef("*-+%c%c\n", key, val)
    line!indent := side=Right
    FOR i = 1 TO indent   DO writes(line!i->"| ", "  ")
    IF left DO writes "| "
    newline()
    prt(Left, left, indent+1)
.

FUN mktree : key, val, p, l, r =>
  spacep := @ spacep!-Size
  MATCH spacep : [:=key, :=val, :=p, :=l, :=r] => spacep
               .
.


FUN rotleft    // Promote right child    p              p
: n[key, val,                    //      |              |
    p[?,?,?,pl,pr],              //      n      =>      r
    x,                           //     / \            / \
    r[?,?,rp,y[?,?,yp,?,?],z]    //    x   r          n   z
   ] =>                          //       / \        / \
                                 //      y   z      x   y

  IF p TEST n=pl THEN pl := r    // Correct the left/right link
                 ELSE pr := r    // of the parent, if any
  IF y DO yp := n
  r, y, rp, p := y, n, p, r
.


FUN rotright   // Promote left child      p            p
: n[key, val,                    //       |            |
    p[?,?,?,pl,pr],              //       n     =>     l      
    l[?,?,lp,?,y[?,?,yp,?,?]],   //      / \          / \
    z                            //     l   z        x   n
   ] =>                          //    / \              / \
                                 //   x   y            y   z

  IF p TEST n=pl THEN pl := l    // Correct the left/right link
                 ELSE pr := l    // of the parent, if any
  IF y DO yp := n
  l, y, lp, p := y, n, p, l
.





FUN splay : x => // Promote node x to the root
  MATCH x                                                // Cases

  : [?,?,                          0,?,?] => RETURN      // no p

  : [?,?,p[?,?,             0,=x, ?],?,?] => rotright p  //   p
                                                         //  x \

  : [?,?,p[?,?,             0, ?,=x],?,?] => rotleft  p  //   p
                                                         //  / x

  : [?,?,p[?,?,g[?,?,?,=p, ?],=x, ?],?,?] => LET p'=p    //   g
                                             rotright g  //  p \
                                             rotright p' // x \

  : [?,?,p[?,?,g[?,?,?,=p, ?], ?,=x],?,?] => LET g'=g    //   g
                                             rotleft  p  //  p \
                                             rotright g' // / x

  : [?,?,p[?,?,g[?,?,?, ?,=p],=x, ?],?,?] => LET g'=g    //   g
                                             rotright p  //  / p
                                             rotleft  g' //   x \

  : [?,?,p[?,?,g[?,?,?, ?,=p], ?,=x],?,?] => LET p'=p    //   g
                                             rotleft  g  //  / p
                                             rotleft  p' //   / x
  . REPEAT
.

FUN lookup : [root], key => writef("lookup: %c\n", key)
  LET t = root

  MATCH t
  :                  0 => RETURN 0
  : [=key, ?, ?, ?, ?] => splay t
                          root := t
                          RETURN t
  : [>key, ?, ?, l, r] => t := l
  : [   ?, ?, ?, l, r] => t := r
  . REPEAT
.

FUN update : [root], key, val => writef("Update: %c=%c\n", key, val)
  LET t=root

  TEST t
  THEN MATCH t
       : [=key,:=val, p,     l,     r] => BREAK
       : [>key,    ?, p, (=0)l,     r] => t := mktree(key,val,t,0,0)
                                          l := t
                                          BREAK
       : [>key,    ?, p,     l,     r] => t := l
       : [   ?,    ?, p,     l, (=0)r] => t := mktree(key,val,t,0,0)
                                          r := t
                                          BREAK
       : [   ?,    ?, p,     l,     r] => t := r
       . REPEAT
  ELSE t := mktree(key,val,0,0,0)

  splay t
  root := t
.







FUN start : =>
  LET root=0

  spacev := getvec  10000
  spacep := @spacev!10000

  update(@root,'B', 'b')
  update(@root,'C', 'c')
  update(@root,'D', 'd')
  update(@root,'E', 'e')
  prtree root

  update(@root,'F', 'f')
  update(@root,'G', 'g')
  update(@root,'H', 'h')
  update(@root,'A', 'a')
  prtree root
 
  update(@root,'L', 'l')
  update(@root,'K', 'k')
  update(@root,'J', 'j')
  update(@root,'I', 'i')
  prtree root

  lookup(@root, 'A'); prtree root
  lookup(@root, 'H'); prtree root
  lookup(@root, 'K'); prtree root
  lookup(@root, 'A'); prtree root
  lookup(@root, 'I'); prtree root

  freevec spacev
  RETURN 0
.































FUN prtree : t =>
  line := VEC 80
  FOR i = 0 TO 80 DO line!i := 0
  line!40 := t

  { LET upb = 80
    UNTIL upb=0 OR line!upb DO upb--
    UNLESS upb RETURN
    FOR pass = 0 TO 1 DO
    { FOR i = 1 TO upb MATCH (pass, line!i)
                       : ?,             0 => wrch ' '
                       : ?,             1 => wrch '+'; line!i := 0
                       : ?,             2 => wrch '-'; line!i := 0
                       : 0,             ? => wrch '*'
                       : 1, [key,?,?,?,?] => wrch key
                       .
      newline()
    }
    FOR i = 1 TO upb DO
    { LET a=0, b=0
      MATCH line!i
      :                                     0 => LOOP
      : [?, ?, ?, l[?,?,?,?,x], r[?,?,?,y,?]] =>
        IF l DO a := size x
        IF r DO b := size y
        IF l DO { line!(i-a-2) := l                   // *
                  FOR j = i-a-1 TO i-1 DO line!j := 2 // -
                }
        line!i := 1                                   // +
        IF r DO { FOR j = i+1 TO i+b+1 DO line!j := 2 // -
                  line!(i+b+2) := r                   // *
                  i +:= b+2
                }
      .
    }
  } REPEAT
.
FUN size : 0 => 0
         : [?, ?, ?, l, r] => 2 + size l + size r
.

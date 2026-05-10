// This is a demonstration of splay trees using the new
// pattern matching features of BCPL. This is a BCPL
// translation of a program originally implemented in MCPL.

// Implemented by Martin Richards

//Still under development.

SECTION "splay"

GET "libhdr"

MANIFEST {
  Key=1; Val; Parent; Left; Right   // tree node selectors
  Size                              // tree node size
}

GLOBAL {
  spacev:ug; spacep; spacet;line
  root
  mktree
  rotleft
  rotright
  update
  splay
  prtree
}

LET dumptree() BE
{ newline()
  FOR p = spacep TO spacet-1 BY Size DO
  writef("%i5 -> [%i5 %i5 %i5 %i5 %i5]*n", p, p!0, p!1, p!2, p!3, p!4)
  writef("root=%i5*n", root)
}

LET prtree : t BE
{ LET v = VEC 100
  line := v
//dumptree()
  newline()
  prt(Left, t, 1)
  //writef("splay tree printed*n")
  //abort(2000)
}

AND prt
: side,                                0, indent BE RETURN

: side, n[key, val, parent, left, right], indent BE
  { //writef("side=%n n=%n->[%n %n %n %n %n] indent=%n*n",
    //       side, n, key, val, parent, left, right, indent)
    line!indent := side=Left
//abort(1002)
//writef("writing right branch, right=%n*n", right)
    IF right DO prt(Right, right, indent+1)
    FOR i = 1 TO indent-1 DO writes(line!i->"| ", "  ")
    writef("**-+%c%c*n", key, val)
//    abort(1003)
    line!indent := side=Right
    FOR i = 1 TO indent   DO writes(line!i->"| ", "  ")
    IF left DO writes("| ")
    newline()
//    abort(1004)
//writef("writing left branch, left=%n*n", left)
    IF left DO prt(Left, left, indent+1)
  }

AND mktree : key, val, p, l, r => VALOF
{ spacep := @ spacep!-Size
  MATCH(spacep)
  : [a, b, c, d, e] BE a, b, c, d, e := key, val, p, l, r
  //writef("mktree => %n -> [%n %n %n %n %n]*n", spacep, key, val, p, l, r)
  RESULTIS spacep
}

AND rotleft    // Promote right child      p          p
: n[key, val,                         //   |          |
    np[?,?,?,npl,npr],                //   n    =>    r
    nx,                               //  / \        / \
    nr[?,?,nrp,nry[?,?,nryp,?,?],nrz] // x   r      n   z
   ] BE                               //    / \    / \
                                      //   y   z  x   y
{ LET y = nry
  // The order of the assigments was chosen with great care.

  TEST np              // Test if n has a parent.
  THEN TEST n=npl
       THEN npl := nr  // Update the parent's left branch.
       ELSE npr := nr  // Update the parent's right branch.
  ELSE root := nr      // n has no parent, so r is the new root.
  IF nry DO nryp := n  // If y exists, its parent should be n.

  nrp := np
  nry := n
  np  := nr
  nr  := y
}

AND rotright   // Promote left child          p        p
: n[key, val,                          //     |        |
    np[?,?,?,npl,npr],                 //     n   =>   l       
    nl[?,?,nlp,nlx,nly[?,?,nlyp,?,?]], //    / \      / \
    nz                                 //   l   z    x   n
   ] BE                                //  / \          / \
                                       // x   y        y   z
{ LET y = nly
  // The order of the assigments was chosen with great care.

  TEST np              // Test if n has a parent.
  THEN TEST n=npl
       THEN npl := nl  // Update the parent's left branch.
       ELSE npr := nl  // Update the parent's right branch.
  ELSE root := nl      // n has no parent, so l is the new root.

  IF nly DO nlyp := n  // If y exists, its parent should be n.

  nlp := np
  nly := n
  np  := nl
  nl  := y
}

AND splay : x BE // Promote node x to the root
{  //writef("splay entered*n")
  { //dumptree()
    //writef("x=%n*n", x)
//abort(1005)
    MATCH(x)                                                    // Cases

    : [?,?,                          0,?,?] BE { root := x      // x is the root
                                                 RETURN   
                                               }
    : [?,?,p[?,?,             0,=x, ?],?,?] BE rotright(p)      //   p
                                                                //  x \

    : [?,?,p[?,?,             0, ?,=x],?,?] BE rotleft (p)      //   p
                                                                //  / x

    : [?,?,p[?,?,g[?,?,?,=p, ?],=x, ?],?,?] BE { LET p1 = p     //   g
                                                 rotright(g)    //  p \
                                                 rotright(p1)   // x \
                                               }
    : [?,?,p[?,?,g[?,?,?,=p, ?], ?,=x],?,?] BE { LET g1 = g     //   g
                                                 rotleft (p)    //  p \
                                                 rotright (g1)  // / x
                                               }
    : [?,?,p[?,?,g[?,?,?, ?,=p],=x, ?],?,?] BE { LET g1 = g     //   g
                                                 rotright(p)    //  / p
                                                 rotleft (g1)   //   x \
                                               }
    : [?,?,p[?,?,g[?,?,?, ?,=p], ?,=x],?,?] BE { LET p1 = p     //   g
                                                 rotleft (g)    //  / p
                                                 rotleft (p1)   //   / x
                                               }
  } REPEAT
}

AND lookup : [root], key => VALOF
{ // Return the node with the given key in the tree with the
  // given root, and promote it to the root position.
  // Return zero if no matching node can be found.
  LET t = root

  { MATCH(t)
    :                  0 BE RESULTIS 0
    : [>key, ?, ?, l, ?] BE t := l
    : [<key, ?, ?, ?, r] BE t := r
    :                  ? BE { splay(t)
                              root := t
                              RESULTIS t
	  		    }
  } REPEAT
}

AND update : ptr[root], key, val BE
{ // If a node with the given key can be found replace its value.
  // otherwise insert a new node with the given key and value.
  // In either case promote the matching node to the root position.

  LET t = root
//writef("Before inserting ptr=%n -> [%n] key=%c val=%c*n", ptr, root, key, val)
//prtree(t)
//  writef("Update: %c=%c*n", key, val)

  TEST t
  THEN { MATCH(t)
         : [>key, ?, ?, l, ?] BE TEST l
	                         THEN { t := l
			              }
		  	         ELSE { t := mktree(key,val,t,0,0)
                                        l := t
                                        BREAK
			              }

         : [<key, ?, ?, ?, r] BE TEST r
	                         THEN { t := r
			              }
			         ELSE { t := mktree(key,val,t,0,0)
                                        r := t
                                        BREAK
			              }

         : [   ?, a, ?, ?, ?] BE { a := val; BREAK }

       } REPEAT
  ELSE t := mktree(key,val,0,0,0)

//prtree(t)
//writef("update: calling splay(%n)*n", t)
  splay(t)
//writef("tree after splay(%n)*n", t)
//prtree(t)
//abort(1001)
}

LET start : => VALOF
{ root := 0

  spacev := getvec(10000)
  spacet := @spacev!10000
  spacep := spacet

  writef("Update with key B val b*n")
  update(@root, 'B', 'b')
  prtree(root)
 
  writef("Update with key C val c*n")
  update(@root, 'C', 'c')
  prtree(root)

  writef("Update with key D val d*n")
  update(@root, 'D', 'd')
  prtree(root)

  writef("Update with key E val e*n")
  update(@root, 'E', 'e')
  prtree(root)

  writef("Update with key F val f*n")
  update(@root, 'F', 'f')
  prtree(root)
  
  writef("Update with key G val g*n")
  update(@root, 'G', 'g')
  prtree(root)
  
  writef("Update with key H val h*n")
  update(@root, 'H', 'h')
  prtree(root)
  
  writef("Update with key A val a*n")
  update(@root, 'A', 'a')
  prtree(root)
 
  writef("Update with key L val l*n")
  update(@root, 'L', 'l')
  prtree(root)
  
  writef("Update with key K val k*n")
  update(@root, 'K', 'k')
  prtree(root)
  
  writef("Update with key J val j*n")
  update(@root, 'J', 'j')
  prtree(root)
  
  writef("Update with key I val i*n")
  update(@root, 'I', 'i')
  prtree(root)

  writef("Lookup key A*n")
  lookup(@root, 'A')
  prtree(root)
  
  writef("Lookup key H*n")
  lookup(@root, 'H')
  prtree(root)
  
  writef("Lookup key K*n")
  lookup(@root, 'K')
  prtree(root)
  
  writef("Lookup key A*n")
  lookup(@root, 'A')
  prtree(root)
  
  writef("Lookup key I*n")
  lookup(@root, 'I')
  prtree(root)

  freevec(spacev)
  RESULTIS 0
}

AND prtree1 : t BE
{ LET v = VEC 80
  line := v
  FOR i = 0 TO 80 DO line!i := 0
  line!40 := t

  { LET upb = 80
    UNTIL upb=0 | line!upb DO upb := upb-1
    UNLESS upb RETURN
    FOR pass = 0 TO 1 DO
    { FOR i = 1 TO upb MATCH (pass, line!i)
                       : ?,             0 BE { wrch(' ') }
                       : ?,             1 BE { wrch('+'); line!i := 0 }
                       : ?,             2 BE { wrch('-'); line!i := 0 }
                       : 0,             ? BE { wrch('**') }
                       : 1, [key,?,?,?,?] BE { wrch(key) }

      newline()
    }
    FOR i = 1 TO upb DO
    { LET a, b =0, 0
      MATCH(line!i)
      :                                     0 BE LOOP
      : [?, ?, ?, l[?,?,?,?,x], r[?,?,?,y,?]] BE
        { IF l DO a := size(x)
          IF r DO b := size(y)
          IF l DO { line!(i-a-2) := l                   // *
                    FOR j = i-a-1 TO i-1 DO line!j := 2 // -
                  }
          line!i := 1                                   // +
          IF r DO { FOR j = i+1 TO i+b+1 DO line!j := 2 // -
                    line!(i+b+2) := r                   // *
                    i +:= b+2
                  }
        }
    }
  } REPEAT
}

AND size
:               0 => 0
: [?, ?, ?, l, r] => 2 + size(l) + size(r)


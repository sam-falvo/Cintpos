/*

(c) Martin Richards   22 December 1997

*/
/************* Binary Relation Object: R2 **************

This is designed to hold known information about about
pairs of variable. There are 16 possible binary relations
over a pair of boolean variables x and y. They are encoded
as follows:

1100 x
1010 y                           --represented by varmap--
                        R2(x,y)  EqNe(x,y)   R1(x)   R1(y)
0000  Inconsistent
0001                                         x=0     y=0
0010                                         x=0     y=1
0011                                         x=0
0100                                         x=1     y=0
0101                                                 y=0
0110                               x#y
0111                    ~x->y
1000                                         x=1     y=1
1001                               x=y
1010                                                 y=1
1011                    ~x->~y
1100                                         x=1
1101                     x->y
1110                     x->~y
1111   Always satisfied

The EqNe relation and the simple simple mappings are represented
by the vector varmap.

varmap!x = -1          means variable x is known to equal 0
varmap!x =  1          means variable x is known to equal 1
varmap!x =  y    (x>y) means variables x and y have the same value
varmap!x = -y    (x>y) means variables x and y have opposite values
varmap!x =  0          means nothing known about variable x

Thus varmap contains chains with decreasing subscripts. Whenever the
mapped value of a variable x, say, is looked up (by mapof), its chain
is canonicalized, leaving one of the following conditions satisfied:

    *  varmap!x =  0                        Nothing known about x
    *  varmap!x =  y  and  varmap!y=0       x maps to y
    *  varmap!x = -y  and  varmap!y=0       x maps to ~y

The mappings in varmap are always applied to the variables of a term
before the checker attempts to apply any inference rule.

The only relations that cannot be represented by varmap are those
involving implication, namely: ~x->y, ~x->~y, x->y and x->~y.
Relations of this sort have not yet been implemented, but are likely
to be held in a DAG using an algorithm based on the paper "A
linear-time algorithm for testing the truth of certain quantified
Boolean formulas" by Aspvall, Plass and Tarjan in Information
Processing letters, Vol 8, No 3, March 1979.


An instance of an r2 object is created by the call mkR2obj() and its
methods are:

R2_Close#(r2)        Close down object r2.

R2_Put#(r2, relop, x, y)
                     Add the relation: relop(x,y) to the set, where relop
                     is the 4-bit representation of the binary relation.
                     Return TRUE, if this relation was not already known.
                     Raise E_FalseTermFound if an inconsistency is found.

R2_Eq#(r2, x, y)     Equivalent to R2_Put#(r2, #b1001, x, y)

R2_Ne#(r2, x, y)     Equivalent to R2_Put#(r2, #b0110, x, y)


R2_Simp8#(r, [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0])
                     Simplify the given 8 variable term using using
                     information currently held in r2. The term is
                     left in canonical form. Return TRUE if the term
                     is changed, and raise E_FalseTermFound if an
                     inconsistency is found.

R2_Intersect#(r2, r2')
                     Replace the information in r2 by the intersection
                     of information that is in both r2 and r2'.
                     Return TRUE if the intersection is non-empty.

R2_Unite#(r2, r2')   Add the information in r2' to r2. Return TRUE if
                     r2 is changed, and raise E_FalseTermFound if an
                     inconsistency is found.

R2_Print#(r2)        Output the information currently held in r2 in some
                     form.

********************************************************/

GET "chk.h"

MANIFEST // R2 fields
  R2Fns,
  R2Vmax,
  R2Vm,
  R2Space,
  R2Freelist,
  R2Pvd,
  R2Pvid,
  R2Nvd,
  R2Nvid,
  R2Size
  
STATIC r2fns = [ r2fnClose,
                 r2fnPut,
                 r2fnEq,
                 r2fnNe,
                 r2fnSimp8,
                 r2fnIntersect,
                 r2fnUnite,
                 r2fnPrint
               ]

FUN mkR2obj : vmax =>
  LET r = getvec R2Size
  UNLESS r RAISE (E_Space, "Can't allocate R2\n")
  r!R2Fns := r2fns
  FOR i = 1 TO R2Size-1 DO r!i := 0

  r!R2Vmax := vmax
  r!R2Space := 0
  r!R2Freelist := 0
  LET vm = getvec vmax
  r!R2Vm := vm
  LET pvd = getvec vmax
  r!R2Pvd := pvd
  LET pvid = getvec vmax
  r!R2Pvid := pvid
  LET nvd = getvec vmax
  r!R2Nvd := nvd
  LET nvid = getvec vmax
  r!R2Nvid := nvid
  UNLESS vm AND pvd AND pvid AND nvd AND nvid DO
  { r2fnClose r
    RAISE (E_Space, "Unable to allocate in R2fnInit\n")
  }
  FOR i = 0 TO vmax DO vm!i, pvd!i, pvid!i, nvd!i, nvid!i ALL:= 0

  RETURN r

FUN r2fnClose : r2 =>
  IF r2!R2Vm    DO freevec(r2!R2Vm)
  IF r2!R2Pvd   DO freevec(r2!R2Pvd)
  IF r2!R2Pvid  DO freevec(r2!R2Pvid)
  IF r2!R2Nvd   DO freevec(r2!R2Nvd)
  IF r2!R2Nvid  DO freevec(r2!R2Nvid)
  WHILE r2!R2Space DO { LET p = r2!R2Space
                        r2!R2Space := !p
                        freevec p
                      }
  freevec r2

FUN r2fnPut : r2, rel, x, y =>
  writef("R2Put: %4b %5d %5d\n", rel, x, y)
  LET vm = r2!R2Vm
  IF x<y DO { x, y := y, x
              rel := TABLE [ #b0000, #b0001, #b0100, #b0101,
                             #b0010, #b0011, #b0110, #b0111,
                             #b1000, #b1001, #b1100, #b1101,
                             #b1010, #b1011, #b1110, #b1111
                           ] ! rel
            }
  MATCH rel
  : #b0000 => RAISE E_FalseTermFound
  : #b0001 => setmap(vm,  x, -1); setmap(vm, y, -1)
  : #b0010 => setmap(vm,  x, -1); setmap(vm, y,  1)
  : #b0011 => setmap(vm,  x, -1)
  : #b0100 => setmap(vm,  x,  1); setmap(vm, y, -1)
  : #b0101 =>                     setmap(vm, y, -1)
  : #b0110 => setmap(vm,  x, -y)
  : #b0111 => setimp(r2,  x, -y)
  : #b1000 => setmap(vm,  x,  1);  setmap(vm, y, 1)
  : #b1001 => setmap(vm,  x,  y)
  : #b1010 =>                      setmap(vm, y, 1)
  : #b1011 => setimp(r2,  x,  y)
  : #b1100 => setmap(vm,  x,  1)
  : #b1101 => setimp(r2, -x, -y)
  : #b1110 => setimp(r2, -x,  y)
  : #b1111 =>
  :        => writes "Unknown relop\n" 

FUN r2fnEq : r2, x, y => r2fnPut(r2, #b1001, x, y)
FUN r2fnNe : r2, x, y => r2fnPut(r2, #b0110, x, y)

FUN r2fnIntersect : r2, r2' => bug "r2fnIntersect not implemented\n"
FUN r2fnUnite     : r2, r2' => bug "r2fnUnite not implemented\n"

FUN r2fnPrint : r2 => 
  LET k = 1            // For layout
  LET vm = r2!R2Vm
  writef "Mapping:"
  UNLESS vm DO { writef " No mapping vector\n"; RETURN }
  FOR i = 0 TO r2!R2Vmax IF vm!i DO
  { UNLESS k++ MOD 8 DO newline()
    LET y = vm!i
    IF y=-1 DO y := 0
    writef(" %d%c%d", i, (y<0->'#','='), ABS y)
  }
  newline()

STATIC varmap=0

FUN r2fnSimp8 : r, p[ r7, r6, r5, r4, r3, r2, r1, r0,
                      v7, v6, v5, v4, v3, v2, v1, v0] =>
  // On entry, x, y and z >=0
  // On return, x =0 or >2,
  //            y =0 or >2
  //        and z =0 or >2
  // It may change rel.
  // It raises no exceptions.

  varmap := r!R2Vm

  MATCH mapof v7 : t(<0) => v7 := -t; not7 p
                 : 1     => v7 :=  0; not7 p
                 : t     => v7 :=  t
                 .
  MATCH mapof v6 : t(<0) => v6 := -t; not6 p
                 : 1     => v6 :=  0; not6 p
                 : t     => v6 :=  t
                 .
  MATCH mapof v5 : t(<0) => v5 := -t; not5 p
                 : 1     => v5 :=  0; not5 p
                 : t     => v5 :=  t
                 .
  MATCH mapof v4 : t(<0) => v4 := -t; not4 p
                 : 1     => v4 :=  0; not4 p
                 : t     => v4 :=  t
                 .
  MATCH mapof v3 : t(<0) => v3 := -t; not3 p
                 : 1     => v3 :=  0; not3 p
                 : t     => v3 :=  t
                 .
  MATCH mapof v2 : t(<0) => v2 := -t; not2 p
                 : 1     => v2 :=  0; not2 p
                 : t     => v2 :=  t
                 .
  MATCH mapof v1 : t(<0) => v1 := -t; not1 p
                 : 1     => v1 :=  0; not7 1
                 : t     => v1 :=  t
                 .
  MATCH mapof v0 : t(<0) => v0 := -t; not0 p
                 : 1     => v0 :=  0; not0 p
                 : t     => v0 :=  t
                 .

FUN mapof : x =>
  UNLESS varmap RETURN x // No mapping

// varmap!x =  0        =>  nothing known about x
//          =  1        =>  x-> 1         (true)
//          = -1        =>  x-> 0         (false)
//          =  y (y>2)  =>  x-> y
//          = -y (y>2)  =>  x->~y

  MATCH varmap!x
  :    0  => RETURN x        // nothing known about x
  :   -1  => RETURN 0       // x-> 0
  :    1  => RETURN 1       // x-> 1
  :    2  => bug("dont care variable %d\n", x)  // don't-care marker
  : y(>2) => y := mapof y
             varmap!x := y=0 -> -1, y
             RETURN y
  : y     => y := -mapof(-y)
             IF y= 0 DO y := 1
             IF y=-1 DO y := 0
             varmap!x := y=0 -> -1, y
             RETURN y

FUN setmap : v, x, y =>
// map variable x to equal variable y
//
  LET neg = FALSE
  UNLESS x DO x := -1
  UNLESS y DO y := -1

  { IF x<0 DO neg, x := NOT neg, -x
    UNLESS v!x BREAK
    x := v!x
  } REPEAT

  { IF y<0 DO neg, y := NOT neg, -y
    UNLESS v!y BREAK
    y := v!y
  } REPEAT

  // x>0 and y>0  and v!x=0 and v!y=0

  IF x=y DO { IF neg DO
              { IF debug>=5 DO writef "Unsatisfiable term found\n"
                RAISE E_FalseTermFound
              }
              RETURN
            }
  TEST x>y THEN v!x := neg -> -y, y
           ELSE v!y := neg -> -x, x
  change := TRUE

/*
FUN intersect_maps : vm, vm1, n => // Intersect maps vm, vm1 -> vm
                                   // returns TRUE if non-empty.
  UNLESS vm AND vm1 DO bug "in intersect_maps\n"

  FOR i = 3 TO n DO // Conicalize the vm mapping vector.
  { LET a = vm!i
    IF a=0 LOOP
    LET absa = ABS a
    LET b = vm!absa
    IF b=0 LOOP
    vm!absa := a<0 -> -b, b
  } 

  FOR i = 3 TO n DO // Conicalize the vm1 mapping vector.
  { LET a = vm1!i
    IF a=0 LOOP
    LET absa = ABS a
    LET b = vm1!absa
    IF b=0 LOOP
    vm1!absa := a<0 -> -b, b
  } 

  FOR i = 3 TO n DO // Form mono-lists in vm1 mapping vector
  { LET a = vm1!i
    IF a=0 LOOP
    LET absa = ABS a
    LET b = vm1!absa
    LET absb = ABS b
    IF b DO vm1!i := a*b>0 -> absb, -absb
    vm1!absa := a<0->-i,i
  }
  FOR i = 0 TO n IF ABS(vm1!i)>=i DO vm1!i := 0

  //FOR i = 0 TO n DO // Debug output
  //{ LET a = vm !i
  //  LET b = vm1!i
  //  IF a OR b DO writef("%4d: %5d   %5d\n", i, a, b)
  //}

  LET nonempty = FALSE

  FOR i = n TO 3 BY -1 DO
  { LET vmi = vm!i // Find relations involving variable i common to both. 
    vm!i := 0
    IF vmi=0 LOOP
    LET p = vm1!i
    LET neg = FALSE
    WHILE p DO
    { IF p<0 DO p, neg := -p, ~neg
      LET vmp = vm!p

      //writef("considering: %3d/%3d and %3d/%3d\n", i, vmi, p, vmp)
      TEST neg THEN IF vmi=-vmp DO { IF debug>=5 DO
                                       writef("intermap: %d#%d\n", i, p)
                                     vm!i := -p
                                     nonempty := TRUE
                                   }
               ELSE IF vmi= vmp DO { IF debug>=5 DO
                                       writef("intermap: %d=%d\n", i, p)
                                     vm!i :=  p
                                     nonempty := TRUE
                                   }
      p := vm1!p
    }
  }

  //writef "After intersection\n"     // Debug output
  //FOR i = 0 TO n DO
  //{ LET a = vm !i
  //  LET b = vm1!i
  //  IF a OR b DO writef("%4d: %5d   %5d\n", i, a, b)
  //}

  RETURN nonempty // Non-empty intersection?

FUN combine_maps : vm =>
  LET varmap = curts!TsVm
  LET vmax   = curts!TsVmax
  FOR var = 3 TO curts!TsVmax IF vm!var DO
  { setmap(varmap, var, vm!var)
    writef("dilem: %d=%d\n", var, vm!var)
  }
  compact_terms()
*/

//********** Implementation of implications

FUN setimp : r2, x, y =>
  LET absx = mapof(ABS x)
  LET absy = mapof(ABS y)
  bug "setimp not implemented\n"

FUN dfs : v, i, d =>
  LET pv, nv, curid, joinring, setid
  LET dmin = d
  LET succs = v!i
  WHILE succs DO
  { LET suc = succs!1
    LET sv = pv
    IF suc<=0 DO sv, suc := nv, -suc
    LET d' = dfs(sv,  suc, d+1)
    UNLESS dmin<=d' DO dmin := d'

    TEST d'>d
    THEN setid(sv, suc, ++curid) // form a new component
    ELSE joinring(v, i, sv, suc)

    succs := !succs
  }
  RETURN dmin

FUN start : =>
  LET r2 = mkR2obj 100

  { writes "Type rel x y: "
    LET rel = readn()
    IF rel<=0 BREAK
    LET x = readn()
    LET y = readn()
    R2_Put#(r2, rel, x, y)
    R2_Print#(r2)
  } REPEAT
  HANDLE : E_FalseTermFound => writes "FalseTermFound\n"
         : E_NoTerms        => writes "NoTerms\n"
         : E_Space, a       => writef("Space %s\n", a)
         .
  R2_Close#(r2)




MODULE sortdemo

GET "mcpl.h"

GLOBAL ptr:Ug

FUN treesort : v, upb => LET tree=0, treespace=getvec(upb*3)
                         ptr := treespace
                         FOR i = 1 TO upb DO putintree(@tree, v!i)
                         ptr := @ v!1
                         flatten tree
                         freevec treespace

FUN putintree
: [ 0  (:=ptr)],        k => !ptr+++ := k; !ptr+++ := 0; !ptr+++ := 0
: [[key, l, r]],  (<key)k => putintree(@l, k)
: [[key, l, r]],        k => putintree(@r, k)
 
FUN flatten :           0 => RETURN
            : [val, l, r] => flatten l
                             !ptr+++ := val
                             flatten r

FUN shellsort : v, upb =>
  LET m = 1
  UNTIL m>upb DO m := m*3 + 1  // Find first suitable value in the
                               // series:  1, 4, 13, 40, 121, 364, ...
  { m := m/3
    FOR i = m+1 TO upb DO
    { LET vi = v!i
      LET j = i
      { LET k = j - m
        IF k<=0 OR v!k<vi BREAK
        v!j, j := v!k, k
      } REPEAT
      v!j := vi
    }
  } REPEATUNTIL m=1


FUN heapify : v, k, i, last =>
{ LET j = i+i  // If there is a son (or two), j = subscript of first.
  LET x = k    // x will hold the larger of the sons if any.

  IF j<=last DO x := v!j      // j, x = subscript and key of first son.
  IF j< last DO
  { LET y = v!(j+1)           // y = key of the other son.
    IF x<y DO x, j := y, j+1  // j, x = subscript and key of larger son.
  }

  IF k>=x DO { v!i := k; RETURN }   // Done if k >= larger son, if any.

  v!i := x                    // Promote the larger son
  i := j
} REPEAT

FUN heapsort : v, upb =>
  FOR i = upb/2 TO 1 BY -1 DO heapify(v, v!i, i, upb)

  FOR i = upb TO 2 BY -1 DO
  { LET k = v!i
    v!i := v!1
    heapify(v, k, 1, i-1)
  }




FUN quicksort : v, n => qsort(@v!1, @v!n)

FUN qsort : l, r =>
  WHILE @l!8<r DO
  { LET midpt = ((l+r)/2) & -Bpw
    // Select a good(ish) median value.
    LET val   = middle(!l, !midpt, !r)
    LET i = partition(val, l, r)
    // Only use recursion on the smaller partition.
    TEST i>midpt THEN { qsort(i,     r); r := i-Bpw }
                 ELSE { qsort(l, i-Bpw); l := i     }
  }

  FOR p = l TO r-Bpw BY Bpw DO  // Now perform insertion sort.
    FOR q = p TO l BY -Bpw TEST q!0<=q!1 THEN BREAK
                                         ELSE q!0, q!1 := q!1, q!0

FUN middle
: a, (<a)b,     c => middle(b, a, c)
: a,     b, (<b)c => middle(a, c, b)
: ?,     b,     ? => b

FUN partition : median, p, q =>
{ WHILE !p < median DO p+++
  WHILE !q > median DO q---
  IF p>=q RETURN p
  !p, !q := !q, !p
  q---
  p+++
} REPEAT

MANIFEST Upb = 10000

FUN start : =>
  LET v = getvec Upb

  try("shell", shellsort, v, Upb)
  try("heap",  heapsort,  v, Upb)
  try("tree",  treesort,  v, Upb)
  try("quick", quicksort, v, Upb)

  freevec v
  RETURN 0

FUN try : name, sortroutine, v, upb =>
  writef("\nSetting %d words of data for %s sort\n", upb, name)
  FOR i = 1 TO upb DO v!i := randno 10000
  writef("Calling %s sort\n", name)
  sortroutine(v, upb)
  TEST sorted(v, upb)
    THEN writes "The data is now sorted\n"
    ELSE writef("### ERROR: %s sort does not work\n", name)

FUN sorted : v, n =>
  FOR i = 1 TO n-1 UNLESS v!i<=v!(i+1) RETURN FALSE
  RETURN TRUE


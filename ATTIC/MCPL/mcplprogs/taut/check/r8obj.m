/*
(c) Martin Richards  10 December 1997

   This is an algorithm for tautology checking implemented in MCPL
   by M. Richards. It is related to Stalmarck's patented algorithm 
   described in the paper:

   G. Stalmarck and M. Saflund, Modelling and Verifying Systems and
   Software in Propositional Logic, IFAC, SAFECOMP90, London, UK 1990

   It also has features similar to those found in:

   Kunz,W. and Stoffel,D. Reasoning in Boolean Networks
   Kluwer Academic Publishers, 1997

The program is implemented in an object oriented style, with this
module implementing the Rn object which deals with relational terms over
n variables. This file implements the case n=8. Its methods are as follows:

Rn_Init#(r, tmax)
    Initialise the Rn object with space for tmax terms. Removing
    any previous set of terms if it existed.

Rn_Close#(r)
    Close down the Rn object, by freeing all its work space, but
    not its code.

Rn_AddTerm3#(r, rel, x, y, z)
    Add the 3 variable term [rel, x, y, z] to the current set of terms.

Rn_AddTerm4#(r, rel, v3,v2,v1,v0)
Rn_AddTerm5#(r, rel, v4,v3,v2,v1,v0)
Rn_AddTerm8#(r, r7,r6,r5,r4,r3,r2,r1,r0, v7,v6,v5,v4,v3,v2,v1,v0)
    Add terms of other sizes to the term set.

Rn_PrTerms#(r)
    Print the current set of terms.

Rn_PrMapping#(r)
    Print the current variable mapping.

Rn_Compact#(r)
    Compact the set of terms.

Rn_Unit#(r)
    Apply the unit rules to the set of terms in r.
    It returns:    0 if error occured
                   1 if the terms are satisfiable
                   2 if they are not satisfiable
                   3 if unable to decide

Rn_Pair#(r)
    Apply the pair rules to the set of terms in r.
    It returns:    0 if error occured
                   1 if the terms are satisfiable
                   2 if they are not satisfiable
                   3 if unable to decide

Rn_Check#(r)
    Apply the checking algorithm to the set of terms in r.
    It returns:    0 if error occured
                   1 if the terms are satisfiable
                   2 if they are not satisfiable
                   3 if unable to decide
*/

GET "chk.h"

STATIC rnfns = [ rnfnInit,
                 rnfnClose,
                 rnfnAddTerm3,
                 rnfnAddTerm4,
                 rnfnAddTerm5,
                 rnfnAddTerm8,
                 rnfnPrTerms,
                 rnfnPrMapping,
                 rnfnCompact,
                 rnfnUnit,
                 rnfnPair,
                 rnfnCheck
               ]

FUN mkRnobj : => LET rn = TABLE [0, 0, 0]
                 !rn := rnfns
                 RETURN rn

STATIC last_given_tmax = 100000

FUN rnfnInit : r, tmax =>
  IF tmax DO last_given_tmax := tmax
  IF curts DO rnfnClose r
  curts := alloc_termset last_given_tmax
  varmap := curts!TsVm

FUN rnfnClose : r =>
  close_termset()

FUN rnfnAddTerm3  : r, rel, v2,v1,v0 =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
  pushterm(rel, v2, v1, v0)

FUN rnfnAddTerm4  : r, rel, v3,v2,v1,v0 =>
  bug "fnAddTerm4 not implemented\n"

FUN rnfnAddTerm5  : r, rel, v4,v3,v2,v1,v0 =>
  bug "fnAddTerm5 not implemented\n"

FUN rnfnAddTerm8  : r, r7,r6,r5,r4,r3,r2,r1,r0,
                       v7,v6,v5,v4,v3,v2,v1,v0 =>
  bug "fnAddTerm8 not implemented\n"

FUN rnfnPrTerms : r =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
  prterms curts

FUN rnfnPrMapping : r =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
  prmapping curts

FUN rnfnCompact   : r =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
{ compact_vars  curts
  compact_terms curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnUnit     : rn =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
{ apply_unit_rules curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnPair     : rn =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
{ apply_pair_rules curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0

FUN rnfnCheck     : rn =>
  UNLESS curts DO { writef "No current term set\n"
                    RETURN
                  }
{ check curts

  RETURN 3
} HANDLE : E_FalseTermFound => 2
         : E_NoTerms        => 1
         : E_Space          => 0


FUN rnfnDebug     : rn, n => debug := n



MANIFEST 

Id, Lparen, Rparen,Eof  // Lexical tokens

// There are 2**256 relations over 8 boolean variable. Eight 32 bit words
// are use to represent any particular relation, and eight words are used
// to identify the variables to which the relation applies.

//  [r7 r6 r5 r4 r3 r2 r1 r0 v7 v6 v5 v4 v3 v2 v1 v0]

//  v7  1       1       1       1       0       0       0       0
//  v6  1       1       0       0       1       1       0       0
//  v5  1       0       1       0       1       0       1       0

//      r7      r6      r5      r4      r3      r2      r1      r0

// Each ri is a 32 bit pattern with bits defined as follows:

//  v4  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
//  v3  1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0
//  v2  1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0
//  v1  1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0
//  v0  1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0

//      -----------------a 32 bit pattern: ri -------------------------

//      ^ ^             ^
//      | |             |
//      | |             |         etc
//      | |             |
//      | |             *=1 <=> 10111 in [ri, v4, v3, v2, v1, v0]
//      | |  ...
//      | *=1 <=> 11110 in [ri, v4, v3, v2, v1, v0]
//      *=1 <=> 11111 in [ri, v4, v3, v2, v1, v0]



// A given propositional expression is represented as a set of terms
// of the form: [r7, r6, r5, r4, r3, r2, r1, r0,
//               v7, v6, v5, v4, v3, v2, v1, v0]
// where the 256 bit in r7..r0 identify which patterns of 0s and 1s
// the variables v7..v0 can be set to.

// v7..v0 are are variable ids, some occurring in the original expression
// and some are ids given to the subexpressions. The ids are represented
// by integers: 0, 1, 2,...
// Id 0 is always false
// Id 1 is always true
// Id 2 is used to mark a variable that only occurs once in the set
//      of terms and so can be treated as a don't-care value.

// and the other ids are free to hold either true or false

FUN not7 // Change relation for v7->~v7
: [r7,r6,r5,r4,r3,r2,r1,r0] =>
   r7,r6,r5,r4,r3,r2,r1,r0 := r3,r2,r1,r0,r7,r6,r5,r4

FUN not6 // Change relation for v6->~v6
: [r7,r6,r5,r4,r3,r2,r1,r0] =>
   r7,r6,r5,r4,r3,r2,r1,r0 := r5,r4,r7,r6,r1,r0,r3,r2

FUN not5 // Change relation for v5->~v5
: [r7,r6,r5,r4,r3,r2,r1,r0] =>
   r7,r6,r5,r4,r3,r2,r1,r0 := r6,r7,r3,r4,r2,r3,r0,r1

FUN not4 // Change relation for v4->~v4
: [r7,r6,r5,r4,r3,r2,r1,r0] =>
   r7 := (r7>>16)&#x0000FFFF | (r7<<16)&#xFFFF0000
   r6 := (r6>>16)&#x0000FFFF | (r6<<16)&#xFFFF0000
   r5 := (r5>>16)&#x0000FFFF | (r5<<16)&#xFFFF0000
   r4 := (r4>>16)&#x0000FFFF | (r4<<16)&#xFFFF0000
   r3 := (r3>>16)&#x0000FFFF | (r3<<16)&#xFFFF0000
   r2 := (r2>>16)&#x0000FFFF | (r2<<16)&#xFFFF0000
   r1 := (r1>>16)&#x0000FFFF | (r1<<16)&#xFFFF0000
   r0 := (r0>>16)&#x0000FFFF | (r0<<16)&#xFFFF0000

FUN not3 // Change relation for v3->~v3
: [r7,r6,r5,r4,r3,r2,r1,r0] =>
   r7 := (r7>>8)&#x00FF00FF | (r7<<8)&#xFF00FF00
   r6 := (r6>>8)&#x00FF00FF | (r6<<8)&#xFF00FF00
   r5 := (r5>>8)&#x00FF00FF | (r5<<8)&#xFF00FF00
   r4 := (r4>>8)&#x00FF00FF | (r4<<8)&#xFF00FF00
   r3 := (r3>>8)&#x00FF00FF | (r3<<8)&#xFF00FF00
   r2 := (r2>>8)&#x00FF00FF | (r2<<8)&#xFF00FF00
   r1 := (r1>>8)&#x00FF00FF | (r1<<8)&#xFF00FF00
   r0 := (r0>>8)&#x00FF00FF | (r0<<8)&#xFF00FF00

FUN not2 // Change relation for v2->~v2
: [r7,r6,r5,r4,r3,r2,r1,r0] =>
   r7 := (r7>>4)&#x0F0F0F0F | (r7<<4)&#xF0F0F0F0
   r6 := (r6>>4)&#x0F0F0F0F | (r6<<4)&#xF0F0F0F0
   r5 := (r5>>4)&#x0F0F0F0F | (r5<<4)&#xF0F0F0F0
   r4 := (r4>>4)&#x0F0F0F0F | (r4<<4)&#xF0F0F0F0
   r3 := (r3>>4)&#x0F0F0F0F | (r3<<4)&#xF0F0F0F0
   r2 := (r2>>4)&#x0F0F0F0F | (r2<<4)&#xF0F0F0F0
   r1 := (r1>>4)&#x0F0F0F0F | (r1<<4)&#xF0F0F0F0
   r0 := (r0>>4)&#x0F0F0F0F | (r0<<4)&#xF0F0F0F0

FUN not1 // Change relation for v1->~v1
: [r7,r6,r5,r4,r3,r2,r1,r0] =>
   r7 := (r7>>2)&#x33333333 | (r7<<2)&#xCCCCCCCC
   r6 := (r6>>2)&#x33333333 | (r6<<2)&#xCCCCCCCC
   r5 := (r5>>2)&#x33333333 | (r5<<2)&#xCCCCCCCC
   r4 := (r4>>2)&#x33333333 | (r4<<2)&#xCCCCCCCC
   r3 := (r3>>2)&#x33333333 | (r3<<2)&#xCCCCCCCC
   r2 := (r2>>2)&#x33333333 | (r2<<2)&#xCCCCCCCC
   r1 := (r1>>2)&#x33333333 | (r1<<2)&#xCCCCCCCC
   r0 := (r0>>2)&#x33333333 | (r0<<2)&#xCCCCCCCC

FUN not0 // Change relation for v0->~v0
: [r7,r6,r5,r4,r3,r2,r1,r0] =>
   r7 := (r7>>1)&#x55555555 | (r7<<1)&#xAAAAAAAA
   r6 := (r6>>1)&#x55555555 | (r6<<1)&#xAAAAAAAA
   r5 := (r5>>1)&#x55555555 | (r5<<1)&#xAAAAAAAA
   r4 := (r4>>1)&#x55555555 | (r4<<1)&#xAAAAAAAA
   r3 := (r3>>1)&#x55555555 | (r3<<1)&#xAAAAAAAA
   r2 := (r2>>1)&#x55555555 | (r2<<1)&#xAAAAAAAA
   r1 := (r1>>1)&#x55555555 | (r1<<1)&#xAAAAAAAA
   r0 := (r0>>1)&#x55555555 | (r0<<1)&#xAAAAAAAA

FUN sw76 // Swap variables v7<->v6
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v7,v6 := v6,v7
   r5,r4,r3,r2 := r3,r2,r5,r4

FUN sw75 // Swap variables v7<->v5
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v7,v5 := v5,v7
   r6,r4,r3,r1 := r3,r1,r6,r4

FUN sw74 // Swap variables v7<->v4
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v7,v4 := v4,v7
   r7, r3 := r7&#xFFFF0000 | (r3>>16)&#x0000FFFF,
             r3&#x0000FFFF | (r7<<16)&#xFFFF0000
   r6, r2 := r6&#xFFFF0000 | (r2>>16)&#x0000FFFF,
             r2&#x0000FFFF | (r6<<16)&#xFFFF0000
   r5, r1 := r5&#xFFFF0000 | (r1>>16)&#x0000FFFF,
             r1&#x0000FFFF | (r5<<16)&#xFFFF0000
   r4, r0 := r4&#xFFFF0000 | (r0>>16)&#x0000FFFF,
             r0&#x0000FFFF | (r4<<16)&#xFFFF0000

FUN sw73 // Swap variables v7<->v3
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v7,v3 := v3,v7
   r7, r3 := r7&#xFF00FF00 | (r3>>8)&#x00FF00FF,
             r3&#x00FF00FF | (r7<<8)&#xFF00FF00
   r6, r2 := r6&#xFF00FF00 | (r2>>8)&#x00FF00FF,
             r2&#x00FF00FF | (r6<<8)&#xFF00FF00
   r5, r1 := r5&#xFF00FF00 | (r1>>8)&#x00FF00FF,
             r1&#x00FF00FF | (r5<<8)&#xFF00FF00
   r4, r0 := r4&#xFF00FF00 | (r0>>8)&#x00FF00FF,
             r0&#x00FF00FF | (r4<<8)&#xFF00FF00

FUN sw72 // Swap variables v7<->v2
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v7,v2 := v2,v7
   r7, r3 := r7&#xF0F0F0F0 | (r3>>4)&#x0F0F0F0F,
             r3&#x0F0F0F0F | (r7<<4)&#xF0F0F0F0
   r6, r2 := r6&#xF0F0F0F0 | (r2>>4)&#x0F0F0F0F,
             r2&#x0F0F0F0F | (r6<<4)&#xF0F0F0F0
   r5, r1 := r5&#xF0F0F0F0 | (r1>>4)&#x0F0F0F0F,
             r1&#x0F0F0F0F | (r5<<4)&#xF0F0F0F0
   r4, r0 := r4&#xF0F0F0F0 | (r0>>4)&#x0F0F0F0F,
             r0&#x0F0F0F0F | (r4<<4)&#xF0F0F0F0

FUN sw71 // Swap variables v7<->v1
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v7,v1 := v1,v7
   r7, r3 := r7&#xCCCCCCCC | (r3>>2)&#x33333333,
             r3&#x33333333 | (r7<<2)&#xCCCCCCCC
   r6, r2 := r6&#xCCCCCCCC | (r2>>2)&#x33333333,
             r2&#x33333333 | (r6<<2)&#xCCCCCCCC
   r5, r1 := r5&#xCCCCCCCC | (r1>>2)&#x33333333,
             r1&#x33333333 | (r5<<2)&#xCCCCCCCC
   r4, r0 := r4&#xCCCCCCCC | (r0>>2)&#x33333333,
             r0&#x33333333 | (r4<<2)&#xCCCCCCCC

FUN sw70 // Swap variables v7<->v0
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v7,v0 := v0,v7
   r7, r3 := r7&#xAAAAAAAA | (r3>>1)&#x55555555,
             r3&#x55555555 | (r7<<1)&#xAAAAAAAA
   r6, r2 := r6&#xAAAAAAAA | (r2>>1)&#x55555555,
             r2&#x55555555 | (r6<<1)&#xAAAAAAAA
   r5, r1 := r5&#xAAAAAAAA | (r1>>1)&#x55555555,
             r1&#x55555555 | (r5<<1)&#xAAAAAAAA
   r4, r0 := r4&#xAAAAAAAA | (r0>>1)&#x55555555,
             r0&#x55555555 | (r4<<1)&#xAAAAAAAA

FUN sw65 // Swap variables v6<->v5
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v6,v5 := v5,v6
   r6,r5,r2,r1 := r5,r6,r1,r2

FUN sw64 // Swap variables v6<->v4
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v6,v4 := v4,v6
   r7, r5 := r7&#xFFFF0000 | (r5>>16)&#x0000FFFF,
             r5&#x0000FFFF | (r7<<16)&#xFFFF0000
   r6, r4 := r6&#xFFFF0000 | (r4>>16)&#x0000FFFF,
             r4&#x0000FFFF | (r6<<16)&#xFFFF0000
   r3, r1 := r3&#xFFFF0000 | (r1>>16)&#x0000FFFF,
             r1&#x0000FFFF | (r3<<16)&#xFFFF0000
   r2, r0 := r2&#xFFFF0000 | (r0>>16)&#x0000FFFF,
             r0&#x0000FFFF | (r2<<16)&#xFFFF0000

FUN sw63 // Swap variables v6<->v3
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v6,v3 := v3,v6
   r7, r5 := r7&#xFF00FF00 | (r5>>8)&#x00FF00FF,
             r5&#x00FF00FF | (r7<<8)&#xFF00FF00
   r6, r4 := r6&#xFF00FF00 | (r4>>8)&#x00FF00FF,
             r4&#x00FF00FF | (r6<<8)&#xFF00FF00
   r3, r1 := r3&#xFF00FF00 | (r1>>8)&#x00FF00FF,
             r1&#x00FF00FF | (r3<<8)&#xFF00FF00
   r2, r0 := r2&#xFF00FF00 | (r0>>8)&#x00FF00FF,
             r0&#x00FF00FF | (r2<<8)&#xFF00FF00

FUN sw62 // Swap variables v6<->v2
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v6,v2 := v2,v6
   r7, r5 := r7&#xF0F0F0F0 | (r5>>4)&#x0F0F0F0F,
             r5&#x0F0F0F0F | (r7<<4)&#xF0F0F0F0
   r6, r4 := r6&#xF0F0F0F0 | (r4>>4)&#x0F0F0F0F,
             r4&#x0F0F0F0F | (r6<<4)&#xF0F0F0F0
   r3, r1 := r3&#xF0F0F0F0 | (r1>>4)&#x0F0F0F0F,
             r1&#x0F0F0F0F | (r3<<4)&#xF0F0F0F0
   r2, r0 := r2&#xF0F0F0F0 | (r0>>4)&#x0F0F0F0F,
             r0&#x0F0F0F0F | (r2<<4)&#xF0F0F0F0

FUN sw61 // Swap variables v6<->v1
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v6,v1 := v1,v6
   r7, r5 := r7&#xCCCCCCCC | (r5>>2)&#x33333333,
             r5&#x33333333 | (r7<<2)&#xCCCCCCCC
   r6, r4 := r6&#xCCCCCCCC | (r4>>2)&#x33333333,
             r4&#x33333333 | (r6<<2)&#xCCCCCCCC
   r3, r1 := r3&#xCCCCCCCC | (r1>>2)&#x33333333,
             r1&#x33333333 | (r3<<2)&#xCCCCCCCC
   r2, r0 := r2&#xCCCCCCCC | (r0>>2)&#x33333333,
             r0&#x33333333 | (r2<<2)&#xCCCCCCCC

FUN sw60 // Swap variables v6<->v0
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v6,v0 := v0,v6
   r7, r5 := r7&#xAAAAAAAA | (r5>>1)&#x55555555,
             r5&#x55555555 | (r7<<1)&#xAAAAAAAA
   r6, r4 := r6&#xAAAAAAAA | (r4>>1)&#x55555555,
             r4&#x55555555 | (r6<<1)&#xAAAAAAAA
   r3, r1 := r3&#xAAAAAAAA | (r1>>1)&#x55555555,
             r1&#x55555555 | (r3<<1)&#xAAAAAAAA
   r2, r0 := r2&#xAAAAAAAA | (r0>>1)&#x55555555,
             r0&#x55555555 | (r2<<1)&#xAAAAAAAA

FUN sw54 // Swap variables v5<->v4
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v5,v4 := v4,v5
   r7, r6 := r7&#xFFFF0000 | (r6>>16)&#x0000FFFF,
             r6&#x0000FFFF | (r7<<16)&#xFFFF0000
   r5, r4 := r5&#xFFFF0000 | (r4>>16)&#x0000FFFF,
             r4&#x0000FFFF | (r5<<16)&#xFFFF0000
   r3, r2 := r3&#xFFFF0000 | (r2>>16)&#x0000FFFF,
             r2&#x0000FFFF | (r3<<16)&#xFFFF0000
   r1, r0 := r1&#xFFFF0000 | (r0>>16)&#x0000FFFF,
             r0&#x0000FFFF | (r1<<16)&#xFFFF0000

FUN sw53 // Swap variables v5<->v3
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v5,v3 := v3,v5
   r7, r6 := r7&#xFF00FF00 | (r6>>8)&#x00FF00FF,
             r6&#x00FF00FF | (r7<<8)&#xFF00FF00
   r5, r4 := r5&#xFF00FF00 | (r4>>8)&#x00FF00FF,
             r4&#x00FF00FF | (r5<<8)&#xFF00FF00
   r3, r2 := r3&#xFF00FF00 | (r2>>8)&#x00FF00FF,
             r2&#x00FF00FF | (r3<<8)&#xFF00FF00
   r1, r0 := r1&#xFF00FF00 | (r0>>8)&#x00FF00FF,
             r0&#x00FF00FF | (r1<<8)&#xFF00FF00

FUN sw52 // Swap variables v5<->v2
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v5,v2 := v2,v5
   r7, r6 := r7&#xF0F0F0F0 | (r6>>4)&#x0F0F0F0F,
             r6&#x0F0F0F0F | (r7<<4)&#xF0F0F0F0
   r5, r4 := r5&#xF0F0F0F0 | (r4>>4)&#x0F0F0F0F,
             r4&#x0F0F0F0F | (r5<<4)&#xF0F0F0F0
   r3, r2 := r3&#xF0F0F0F0 | (r2>>4)&#x0F0F0F0F,
             r2&#x0F0F0F0F | (r3<<4)&#xF0F0F0F0
   r1, r0 := r1&#xF0F0F0F0 | (r0>>4)&#x0F0F0F0F,
             r0&#x0F0F0F0F | (r1<<4)&#xF0F0F0F0

FUN sw51 // Swap variables v5<->v1
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v5,v1 := v1,v5
   r7, r6 := r7&#xCCCCCCCC | (r6>>2)&#x33333333,
             r6&#x33333333 | (r7<<2)&#xCCCCCCCC
   r5, r4 := r5&#xCCCCCCCC | (r4>>2)&#x33333333,
             r4&#x33333333 | (r5<<2)&#xCCCCCCCC
   r3, r2 := r3&#xCCCCCCCC | (r2>>2)&#x33333333,
             r2&#x33333333 | (r3<<2)&#xCCCCCCCC
   r1, r0 := r1&#xCCCCCCCC | (r0>>2)&#x33333333,
             r0&#x33333333 | (r1<<2)&#xCCCCCCCC

FUN sw50 // Swap variables v5<->v0
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v5,v0 := v0,v5
   r7, r6 := r7&#xAAAAAAAA | (r6>>1)&#x55555555,
             r6&#x55555555 | (r7<<1)&#xAAAAAAAA
   r5, r4 := r5&#xAAAAAAAA | (r4>>1)&#x55555555,
             r4&#x55555555 | (r5<<1)&#xAAAAAAAA
   r3, r2 := r3&#xAAAAAAAA | (r2>>1)&#x55555555,
             r2&#x55555555 | (r3<<1)&#xAAAAAAAA
   r1, r0 := r1&#xAAAAAAAA | (r0>>1)&#x55555555,
             r0&#x55555555 | (r1<<1)&#xAAAAAAAA

FUN sw43 // Swap variables v4<->v3
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v4,v3 := v3,v4
   r7 := r7&#xFF0000FF | (r7<<8)&#x00FF0000 | (r7>>8)&#x00000FF00
   r6 := r6&#xFF0000FF | (r6<<8)&#x00FF0000 | (r6>>8)&#x00000FF00
   r5 := r5&#xFF0000FF | (r5<<8)&#x00FF0000 | (r5>>8)&#x00000FF00
   r4 := r4&#xFF0000FF | (r4<<8)&#x00FF0000 | (r4>>8)&#x00000FF00
   r3 := r3&#xFF0000FF | (r3<<8)&#x00FF0000 | (r3>>8)&#x00000FF00
   r2 := r2&#xFF0000FF | (r2<<8)&#x00FF0000 | (r2>>8)&#x00000FF00
   r1 := r1&#xFF0000FF | (r1<<8)&#x00FF0000 | (r1>>8)&#x00000FF00
   r0 := r0&#xFF0000FF | (r0<<8)&#x00FF0000 | (r0>>8)&#x00000FF00

FUN sw42 // Swap variables v4<->v2
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v4,v2 := v2,v4
   r7 := r7&#xF0F00F0F | (r7<<12)&#x0F0F0000 | (r7>>12)&#x0000F0F0
   r6 := r6&#xF0F00F0F | (r6<<12)&#x0F0F0000 | (r6>>12)&#x0000F0F0
   r5 := r5&#xF0F00F0F | (r5<<12)&#x0F0F0000 | (r5>>12)&#x0000F0F0
   r4 := r4&#xF0F00F0F | (r4<<12)&#x0F0F0000 | (r4>>12)&#x0000F0F0
   r3 := r3&#xF0F00F0F | (r3<<12)&#x0F0F0000 | (r3>>12)&#x0000F0F0
   r2 := r2&#xF0F00F0F | (r2<<12)&#x0F0F0000 | (r2>>12)&#x0000F0F0
   r1 := r1&#xF0F00F0F | (r1<<12)&#x0F0F0000 | (r1>>12)&#x0000F0F0
   r0 := r0&#xF0F00F0F | (r0<<12)&#x0F0F0000 | (r0>>12)&#x0000F0F0

FUN sw41 // Swap variables v4<->v1
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v4,v1 := v1,v4
   r7 := r7&#xCCCC3333 | (r7<<14)&#x33330000 | (r7>>14)&#x0000CCCC
   r6 := r6&#xCCCC3333 | (r6<<14)&#x33330000 | (r6>>14)&#x0000CCCC
   r5 := r5&#xCCCC3333 | (r5<<14)&#x33330000 | (r5>>14)&#x0000CCCC
   r4 := r4&#xCCCC3333 | (r4<<14)&#x33330000 | (r4>>14)&#x0000CCCC
   r3 := r3&#xCCCC3333 | (r3<<14)&#x33330000 | (r3>>14)&#x0000CCCC
   r2 := r2&#xCCCC3333 | (r2<<14)&#x33330000 | (r2>>14)&#x0000CCCC
   r1 := r1&#xCCCC3333 | (r1<<14)&#x33330000 | (r1>>14)&#x0000CCCC
   r0 := r0&#xCCCC3333 | (r0<<14)&#x33330000 | (r0>>14)&#x0000CCCC

FUN sw40 // Swap variables v4<->v0
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v4,v0 := v0,v4
   r7 := r7&#xAAAA5555 | (r7<<15)&#x55550000 | (r7>>15)&#x0000AAAA
   r6 := r6&#xAAAA5555 | (r6<<15)&#x55550000 | (r6>>15)&#x0000AAAA
   r5 := r5&#xAAAA5555 | (r5<<15)&#x55550000 | (r5>>15)&#x0000AAAA
   r4 := r4&#xAAAA5555 | (r4<<15)&#x55550000 | (r4>>15)&#x0000AAAA
   r3 := r3&#xAAAA5555 | (r3<<15)&#x55550000 | (r3>>15)&#x0000AAAA
   r2 := r2&#xAAAA5555 | (r2<<15)&#x55550000 | (r2>>15)&#x0000AAAA
   r1 := r1&#xAAAA5555 | (r1<<15)&#x55550000 | (r1>>15)&#x0000AAAA
   r0 := r0&#xAAAA5555 | (r0<<15)&#x55550000 | (r0>>15)&#x0000AAAA

FUN sw32 // Swap variables v3<->v2
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v3,v2 := v2,v3
   r7 := r7&#xF00FF00F | (r7<<4)&#x0F000F00 | (r7>>4)&#x00F000F0
   r6 := r6&#xF00FF00F | (r6<<4)&#x0F000F00 | (r6>>4)&#x00F000F0
   r5 := r5&#xF00FF00F | (r5<<4)&#x0F000F00 | (r5>>4)&#x00F000F0
   r4 := r4&#xF00FF00F | (r4<<4)&#x0F000F00 | (r4>>4)&#x00F000F0
   r3 := r3&#xF00FF00F | (r3<<4)&#x0F000F00 | (r3>>4)&#x00F000F0
   r2 := r2&#xF00FF00F | (r2<<4)&#x0F000F00 | (r2>>4)&#x00F000F0
   r1 := r1&#xF00FF00F | (r1<<4)&#x0F000F00 | (r1>>4)&#x00F000F0
   r0 := r0&#xF00FF00F | (r0<<4)&#x0F000F00 | (r0>>4)&#x00F000F0

FUN sw31 // Swap variables v3<->v1
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v3,v1 := v1,v3
   r7 := r7&#xCC33CC33 | (r7<<6)&#x33003300 | (r7>>6)&#x00CC00CC
   r6 := r6&#xCC33CC33 | (r6<<6)&#x33003300 | (r6>>6)&#x00CC00CC
   r5 := r5&#xCC33CC33 | (r5<<6)&#x33003300 | (r5>>6)&#x00CC00CC
   r4 := r4&#xCC33CC33 | (r4<<6)&#x33003300 | (r4>>6)&#x00CC00CC
   r3 := r3&#xCC33CC33 | (r3<<6)&#x33003300 | (r3>>6)&#x00CC00CC
   r2 := r2&#xCC33CC33 | (r2<<6)&#x33003300 | (r2>>6)&#x00CC00CC
   r1 := r1&#xCC33CC33 | (r1<<6)&#x33003300 | (r1>>6)&#x00CC00CC
   r0 := r0&#xCC33CC33 | (r0<<6)&#x33003300 | (r0>>6)&#x00CC00CC

FUN sw30 // Swap variables v3<->v0
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v3,v0 := v0,v3
   r7 := r7&#xAA55AA55 | (r7<<7)&#x55005500 | (r7>>7)&#x00AA00AA
   r6 := r6&#xAA55AA55 | (r6<<7)&#x55005500 | (r6>>7)&#x00AA00AA
   r5 := r5&#xAA55AA55 | (r5<<7)&#x55005500 | (r5>>7)&#x00AA00AA
   r4 := r4&#xAA55AA55 | (r4<<7)&#x55005500 | (r4>>7)&#x00AA00AA
   r3 := r3&#xAA55AA55 | (r3<<7)&#x55005500 | (r3>>7)&#x00AA00AA
   r2 := r2&#xAA55AA55 | (r2<<7)&#x55005500 | (r2>>7)&#x00AA00AA
   r1 := r1&#xAA55AA55 | (r1<<7)&#x55005500 | (r1>>7)&#x00AA00AA
   r0 := r0&#xAA55AA55 | (r0<<7)&#x55005500 | (r0>>7)&#x00AA00AA

FUN sw21 // Swap variables v2<->v1
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v2,v1 := v1,v2
   r7 := r7&#xC3C3C3C3 | (r7<<2)&#x30303030 | (r7>>2)&#x0C0C0C0C
   r6 := r6&#xC3C3C3C3 | (r6<<2)&#x30303030 | (r6>>2)&#x0C0C0C0C
   r5 := r5&#xC3C3C3C3 | (r5<<2)&#x30303030 | (r5>>2)&#x0C0C0C0C
   r4 := r4&#xC3C3C3C3 | (r4<<2)&#x30303030 | (r4>>2)&#x0C0C0C0C
   r3 := r3&#xC3C3C3C3 | (r3<<2)&#x30303030 | (r3>>2)&#x0C0C0C0C
   r2 := r2&#xC3C3C3C3 | (r2<<2)&#x30303030 | (r2>>2)&#x0C0C0C0C
   r1 := r1&#xC3C3C3C3 | (r1<<2)&#x30303030 | (r1>>2)&#x0C0C0C0C
   r0 := r0&#xC3C3C3C3 | (r0<<2)&#x30303030 | (r0>>2)&#x0C0C0C0C

FUN sw20 // Swap variables v2<->v0
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v2,v0 := v0,v2
   r7 := r7&#xA5A5A5A5 | (r7<<3)&#x50505050 | (r7>>3)&#x0A0A0A0A
   r6 := r6&#xA5A5A5A5 | (r6<<3)&#x50505050 | (r6>>3)&#x0A0A0A0A
   r5 := r5&#xA5A5A5A5 | (r5<<3)&#x50505050 | (r5>>3)&#x0A0A0A0A
   r4 := r4&#xA5A5A5A5 | (r4<<3)&#x50505050 | (r4>>3)&#x0A0A0A0A
   r3 := r3&#xA5A5A5A5 | (r3<<3)&#x50505050 | (r3>>3)&#x0A0A0A0A
   r2 := r2&#xA5A5A5A5 | (r2<<3)&#x50505050 | (r2>>3)&#x0A0A0A0A
   r1 := r1&#xA5A5A5A5 | (r1<<3)&#x50505050 | (r1>>3)&#x0A0A0A0A
   r0 := r0&#xA5A5A5A5 | (r0<<3)&#x50505050 | (r0>>3)&#x0A0A0A0A

FUN sw10 // Swap variables v1<->v0
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v1,v0 := v0,v1
   r7 := r7&#x99999999 | (r7<<1)&#x44444444 | (r7>>1)&#x22222222
   r6 := r6&#x99999999 | (r6<<1)&#x44444444 | (r6>>1)&#x22222222
   r5 := r5&#x99999999 | (r5<<1)&#x44444444 | (r5>>1)&#x22222222
   r4 := r4&#x99999999 | (r4<<1)&#x44444444 | (r4>>1)&#x22222222
   r3 := r3&#x99999999 | (r3<<1)&#x44444444 | (r3>>1)&#x22222222
   r2 := r2&#x99999999 | (r2<<1)&#x44444444 | (r2>>1)&#x22222222
   r1 := r1&#x99999999 | (r1<<1)&#x44444444 | (r1>>1)&#x22222222
   r0 := r0&#x99999999 | (r0<<1)&#x44444444 | (r0>>1)&#x22222222


FUN test_swaps : =>
  LET a7 = 541 * randno 1234567
  LET a6 = 541 * randno 1234567
  LET a5 = 541 * randno 1234567
  LET a4 = 541 * randno 1234567
  LET a3 = 541 * randno 1234567
  LET a2 = 541 * randno 1234567
  LET a1 = 541 * randno 1234567
  LET a0 = 541 * randno 1234567

  LET p = [a7,a6,a5,a4,a3,a2,a1,a0,7,6,5,4,3,2,1,0]
  LET q = [a7,a6,a5,a4,a3,a2,a1,a0,7,6,5,4,3,2,1,0]

  // Each line should leave p unchanged
  sw76 p; sw65 p; sw75 p; sw65 p; testeq(p, q, 65)
  sw76 p; sw64 p; sw74 p; sw64 p; testeq(p, q, 64)
  sw76 p; sw63 p; sw73 p; sw63 p; testeq(p, q, 63)
  sw76 p; sw62 p; sw72 p; sw62 p; testeq(p, q, 62)
  sw76 p; sw61 p; sw71 p; sw61 p; testeq(p, q, 61)
  sw76 p; sw60 p; sw70 p; sw60 p; testeq(p, q, 60)
  sw75 p; sw54 p; sw74 p; sw54 p; testeq(p, q, 54)
  sw75 p; sw53 p; sw73 p; sw53 p; testeq(p, q, 53)
  sw75 p; sw52 p; sw72 p; sw52 p; testeq(p, q, 52)
  sw75 p; sw51 p; sw71 p; sw51 p; testeq(p, q, 51)
  sw75 p; sw50 p; sw70 p; sw50 p; testeq(p, q, 50)
  sw74 p; sw43 p; sw73 p; sw43 p; testeq(p, q, 43)
  sw74 p; sw42 p; sw72 p; sw42 p; testeq(p, q, 42)
  sw74 p; sw41 p; sw71 p; sw41 p; testeq(p, q, 41)
  sw74 p; sw40 p; sw70 p; sw40 p; testeq(p, q, 40)
  sw73 p; sw32 p; sw72 p; sw32 p; testeq(p, q, 32)
  sw73 p; sw31 p; sw71 p; sw31 p; testeq(p, q, 31)
  sw73 p; sw30 p; sw70 p; sw30 p; testeq(p, q, 30)
  sw72 p; sw21 p; sw71 p; sw21 p; testeq(p, q, 21)
  sw72 p; sw20 p; sw70 p; sw20 p; testeq(p, q, 20)
  sw71 p; sw10 p; sw70 p; sw10 p; testeq(p, q, 10)

  writef "End of swaps test\n"

FUN testeq : [a7,a6,a5,a4,a3,a2,a1,a0,v7,v6,v5,v4,v3,v2,v1,v0],
             [b7,b6,b5,b4,b3,b2,b1,b0,w7,w6,w5,w4,w3,w2,w1,w0], n =>
  UNLESS a7=b7 AND a6=b6 AND a5=b5 AND a4=b4 AND
         a3=b3 AND a2=b2 AND a1=b1 AND a0=b0 AND
         v7=w7 AND v6=w6 AND v5=w5 AND v4=w4 AND
         v3=w3 AND v2=w2 AND v1=w1 AND v0=w0 DO abort(9000+n)

FUN dcv7 // Update term for don't care v7
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v7 := 0
   r7,r6,r5,r4,r3,r2,r1,r0 := 0, 0, 0, 0, r7|r3, r6|r2, r5|r1, r4|r0

FUN dcv6 // Update term for don't care v6
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v6 := 0
   r7,r6,r3,r2,r5,r4,r1,r0 := 0, 0, 0, 0, r7|r5, r6|r4, r3|r1, r2|r0

FUN dcv5 // Update term for don't care v5
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v5 := 0
   r7,r5,r3,r1,r6,r4,r2,r0 := 0, 0, 0, 0, r7|r6, r5|r4, r3|r2, r1|r0

FUN dcv4 // Update term for don't care v4
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v4 := 0
   r7 := (r7 | r7>>16) & #x0000FFFF 
   r6 := (r6 | r6>>16) & #x0000FFFF 
   r5 := (r5 | r5>>16) & #x0000FFFF 
   r4 := (r4 | r4>>16) & #x0000FFFF 
   r3 := (r3 | r3>>16) & #x0000FFFF 
   r2 := (r2 | r2>>16) & #x0000FFFF 
   r1 := (r1 | r1>>16) & #x0000FFFF 
   r0 := (r0 | r0>>16) & #x0000FFFF 

FUN dcv3 // Update term for don't care v3
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v3 := 0
   r7 := (r7 | r7>>8) & #x00FF00FF 
   r6 := (r6 | r6>>8) & #x00FF00FF 
   r5 := (r5 | r5>>8) & #x00FF00FF 
   r4 := (r4 | r4>>8) & #x00FF00FF 
   r3 := (r3 | r3>>8) & #x00FF00FF 
   r2 := (r2 | r2>>8) & #x00FF00FF 
   r1 := (r1 | r1>>8) & #x00FF00FF 
   r0 := (r0 | r0>>8) & #x00FF00FF 

FUN dcv2 // Update term for don't care v2
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v2 := 0
   r7 := (r7 | r7>>4) & #x0F0F0F0F 
   r6 := (r6 | r6>>4) & #x0F0F0F0F 
   r5 := (r5 | r5>>4) & #x0F0F0F0F 
   r4 := (r4 | r4>>4) & #x0F0F0F0F 
   r3 := (r3 | r3>>4) & #x0F0F0F0F 
   r2 := (r2 | r2>>4) & #x0F0F0F0F 
   r1 := (r1 | r1>>4) & #x0F0F0F0F 
   r0 := (r0 | r0>>4) & #x0F0F0F0F 

FUN dcv1 // Update term for don't care v1
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v1 := 0
   r7 := (r7 | r7>>2) & #x33333333 
   r6 := (r6 | r6>>2) & #x33333333 
   r5 := (r5 | r5>>2) & #x33333333 
   r4 := (r4 | r4>>2) & #x33333333 
   r3 := (r3 | r3>>2) & #x33333333 
   r2 := (r2 | r2>>2) & #x33333333 
   r1 := (r1 | r1>>2) & #x33333333 
   r0 := (r0 | r0>>2) & #x33333333 

FUN dcv0 // Update term for don't care v0
: [r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
   v0 := 0
   r7 := (r7 | r7>>1) & #x55555555 
   r6 := (r6 | r6>>1) & #x55555555 
   r5 := (r5 | r5>>1) & #x55555555 
   r4 := (r4 | r4>>1) & #x55555555 
   r3 := (r3 | r3>>1) & #x55555555 
   r2 := (r2 | r2>>1) & #x55555555 
   r1 := (r1 | r1>>1) & #x55555555 
   r0 := (r0 | r0>>1) & #x55555555 


// A term is represented by a 16-tuple: [r7..r0, v7..v0]

// The 16-tuples are held in a vector of 16 word cells.

MANIFEST // Term fields
         Tr7,      // An 32-bit relation on v4..v0 for case v7v6v5=111
         Tr6,      // An 32-bit relation on v4..v0 for case v7v6v5=110
         Tr5,      // An 32-bit relation on v4..v0 for case v7v6v5=101
         Tr4,      // An 32-bit relation on v4..v0 for case v7v6v5=100
         Tr3,      // An 32-bit relation on v4..v0 for case v7v6v5=011
         Tr2,      // An 32-bit relation on v4..v0 for case v7v6v5=010
         Tr1,      // An 32-bit relation on v4..v0 for case v7v6v5=001
         Tr0,      // An 32-bit relation on v4..v0 for case v7v6v5=000

         Tv7,      // The integer identifiers for each argument
         Tv6,
         Tv5,
         Tv4,
         Tv3,
         Tv2,
         Tv1,
         Tv0,

         Tsize     // The size of a term

STATIC   curts=0,  // The current termset
         varmap=0, // The current variable mapping
         sortvec=0 // For registering the sort vector

                   // Termset fields
MANIFEST TsVm,     // varmap vector, if allocated
         TsVmax,   // largest variable id in use
         TsTerm1,  // Pointer to the first term (on a 16 byte boundary)
         TsTermp,  // Pointer to the next free term position
         TsTermt,  // Pointer to the last possible term position
         TsTn,     // Actual number of terms in the set
         TsSysSize

FUN alloc_termset : tmax => // Allocate a termset large enough to
                            // hold tmax terms.

  LET tsupb = TsSysSize+15+Tsize*(tmax+1) // Leave room alignment space.
  LET ts    = getvec tsupb

  IF ts=0 RAISE (E_Space, "no space for termset vector\n")

  LET t = @ts!TsSysSize
  WHILE t&63 DO t++  // Round up to next 64-byte (16-word) boundary.
  
  ts!TsTerm1 := t
  ts!TsTermp := t
  ts!TsTermt := @t!(Tsize*tmax)
  IF @ts!tsupb < @t!(Tsize*(tmax+1)) DO // Check that there is room
     bug "alloc_termset error"          // for tmax terms.

  ts!TsVm    := 0  // No varmap
  ts!TsVmax  := 0  // Max variable number, when known
  ts!TsTn    := 0  // No terms in the set yet

  RETURN ts

FUN select_ts : ts => curts := ts
                      varmap := ts!TsVm

FUN mkcopy_termset : =>
  UNLESS curts DO bug "mkcopy_termset\n"
  LET n = curts!TsTn
  LET nts = alloc_termset n // Allocate a termset of the right size
  UNLESS nts RAISE (E_Space, "in mkcopy_termset\n")
  nts!TsTn := n

  LET p =  curts!TsTerm1
  LET q = nts!TsTerm1
  FOR i = 1 TO n MATCH (p, q)     // Copy the terms
  : [  r7, r6, r5, r4, r3, r2, r1, r0,
       v7, v6, v5, v4, v3, v2, v1, v0, np],
    [:=r7,:=r6,:=r5,:=r4,:=r3,:=r2,:=r1,:=r0,
     :=v7,:=v6,:=v5,:=v4,:=v3,:=v2,:=v1,:=v0,nq]  => p, q := @np, @nq
  .

  alloc_vm(nts, curts!TsVmax)

  RETURN nts

FUN alloc_vm : ts, vmax =>
  UNLESS vmax DO { ts!TsVm, ts!TsVmax := 0, vmax; RETURN 0 } 
  LET vm = getvec vmax
  UNLESS vm RAISE (E_Space, "Unable to alloc varmap of size %d\n", vmax)
  ts!TsVm, ts!TsVmax := vm, vmax
  FOR i = 0 TO vmax DO vm!i := 0
  RETURN vm

FUN close_termset : =>
  UNLESS curts DO bug "close_termset\n"
  IF curts!TsVm DO freevec(curts!TsVm)
  IF sortvec DO { freevec sortvec  // Free the sort vector, if allocated
                  sortvec := 0
                }
  freevec curts
  curts := 0
  varmap := 0


FUN pushterm : rel, x, y, z => 
  UNLESS curts DO bug "pushterm\n"

  MATCH curts : [vm, vmax, term1, termp,termt,n] =>

  IF termp>termt RAISE (E_Space, "Too many terms\n")

  UNLESS TsVmax=1 DO abort 999
  EVERY curts : [?, <x :=x] =>  // Update TsVmax field
              : [?, <y :=y] =>  // if a larger variable
              : [?, <z :=z] =>  // id is encountered.
              .

  MATCH termp : [:=0,:=0,:=0,:=0,:=0,:=0,:=0,:=rel,
                 :=0,:=0,:=0,:=0,:=0,:=x,:=y,:=z, np] => n++
                                                         termp := @np


// Variables such as v7..v0 occuring in terms are identified
// by positive integers. Variables may get renamed by such mapping 
// actions as: v5=v7 or v3=~v1. The accumulated collection
// of mappings is held in the current mapping vector varmap.

// varmap!x =  0        =>  nothing known about x
//          =  1        =>  x-> 1         (true)
//          = -1        =>  x-> 0         (false)
//          =  2        =>  marker saying x is only used in one term
//                          and so can freely be set to either value.
//          =  y (y>2)  =>  x-> y
//          = -y (y>2)  =>  x->~y



FUN compact_vars : =>
  UNLESS curts DO bug "compact_vars\n"

  IF curts!TsVm DO{ freevec(curts!TsVm)  // Free possible old vector
                    curts!TsVm := 0
                    varmap := 0
                  }

  LET vmax = curts!TsVmax

  UNLESS vmax RETURN

  LET hv = getvec vmax              // Vector for histogram.
  UNLESS hv RAISE(E_Space, "compact_vars\n")

  LET n = curts!TsTn
  LET p = curts!TsTerm1

  FOR i = 0 TO vmax DO hv!i := 0    // Form histogram of variable uses.
  FOR i = 1 TO n MATCH p
  : [r7,r6,r5,r4,r3,r2,r1,r0,
     v7,v6,v5,v4,v3,v2,v1,v0, np] => (hv!v7)++
                                     (hv!v6)++
                                     (hv!v5)++
                                     (hv!v4)++
                                     (hv!v3)++
                                     (hv!v2)++
                                     (hv!v1)++
                                     (hv!v0)++
                                      p := @np
  .

  LET newvmax = 2                   // Construct the renaming mapping.

  hv!0 := 0  // Do not
  hv!1 := 1  //        rename 
  hv!2 := 2  //               these variables

  FOR i = 3 TO vmax MATCH hv!i      // Form the rename mapping.
  : 0 =>                    // Never used
  : 1 => hv!i := 2          // used once -- place don't care marker
  :   => hv!i := ++newvmax  // otherwise give it the next id.
  .
  p := curts!TsTerm1

  FOR i = 1 TO n MATCH p            // Apply the rename mapping.
  : [r7,r6,r5,r4,r3,r2,r1,r0,
     v7:=hv!v7,v6:=hv!v6,v5:=hv!v5,v4:=hv!v4,
     v3:=hv!v3,v2:=hv!v2,v1:=hv!v1,v0:=hv!v0, np] =>
       IF v7=2 DO dcv7 p
       IF v6=2 DO dcv6 p
       IF v5=2 DO dcv5 p
       IF v4=2 DO dcv4 p
       IF v3=2 DO dcv3 p
       IF v2=2 DO dcv2 p
       IF v1=2 DO dcv1 p
       IF v0=2 DO dcv0 p
       p := @np
  .

  freevec hv             // Free the rename mapping vector

  varmap := alloc_vm(curts, newvmax)


FUN compact_terms : => // Remove deleted or duplicate terms.
  UNLESS curts DO bug "compact_terms\n"

  UNLESS varmap DO bug "varmap=0 in compact_terms\n"

  mark_duplicates()       // Canonicalize terms and 
                          // mark duplicates as deleted.

  LET n = curts!TsTn      // Number of terms.
  LET p = curts!TsTerm1   // Next term to look at.
  LET k = 0               // Count of non-deleted terms.
  LET q = p               // Next free term position.

  FOR i = 1 TO n MATCH (p, q)
  : [r7,    r6,r5,r4,r3,r2,r1,r0,
     v7(<0),v6,v5,v4,v3,v2,v1,v0, np], ? => // A deleted term.
         IF debug>=8 DO { writes "cmpct:\n"; prterm p }
         p := @np // Skip deleted term.

  : [r7, r6, r5, r4, r3, r2, r1, r0,
     v7, v6, v5, v4, v3, v2, v1, v0, np],
    [r7',r6',r5',r4',r3',r2',r1',r0',
     v7',v6',v5',v4',v3',v2',v1',v0', nq] =>
         k++      // Preserve non-deleted term.
         UNLESS p=q DO { r7',r6',r5',r4',r3',r2',r1',r0' :=
                         r7, r6, r5, r4, r3, r2, r1, r0
                         v7',v6',v5',v4',v3',v2',v1',v0' :=
                         v7, v6, v5, v4, v3, v2, v1, v0
                       }
         p, q := @np, @nq
  .
  curts!TsTn := k         // New number of terms.
  IF debug>=8 AND k<n DO { writef("Compacted to %d terms\n", k)
                           prterms()
                         }
  UNLESS k DO { writef "No Terms left\n"
                RAISE E_NoTerms
              }


FUN mark_duplicates :  =>          // Mark duplicates as deleted.
  UNLESS curts DO bug "mark_duplicates\n"

  IF sortvec DO { freevec sortvec  // Free sort vector, if allocated
                  sortvec := 0
                }
  LET n = curts!TsTn
  LET k = 0                        // Count of non-deleted terms
  LET v = getvec n                 // Allocate sort vector
  IF v=0 RAISE (E_Space, "in mark_duplicates\n")
  sortvec := v                     // Register sort vector

  LET p = curts!TsTerm1

  FOR i = 1 TO n DO                // Put pointers to
  { canonterm p                    // canonicalized non-deleted terms
    UNLESS p!Tv7<0 DO v!++k := p   // into the sort vector.
    p := @p!Tsize
  }

  UNLESS k DO { writef "No Terms left\n"
                RAISE E_NoTerms
              }

  //prterms()
  //writef("Terms for sorting\n")
  //FOR i = 1 TO k DO prterm(v!i)
  //newline()

  sort(v, k, cmpfull)  // Sort the canonical term pointers (v!1...v!k).

  FOR i = 1 TO k-1 DO
  { LET t  = v!i
    LET t' = v!(i+1)
    EVERY (t, t')      // Search for duplicate pairs.
    : [ r7, r6, r5, r4, r3, r2, r1, r0,
        v7, v6, v5, v4, v3, v2, v1, v0],
      [=r7,=r6,=r5,=r4,=r3,=r2,=r1,=r0,
       =v7,=v6,=v5,=v4,=v3,=v2,=v1,=v0]  =>
          IF debug>=8 DO { writes "dupl:\n"; prterm p }
                           v7 := -1   // Mark duplicate term.
  }

  //writef("Terms after marking duplicates\n")
  //FOR i = 1 TO k DO prterm(v!i)

  freevec sortvec            // Free the sort vector
  sortvec := 0

FUN prterm
: p[ r7, r6, r5, r4, r3, r2, r1, r0,
     v7, v6, v5, v4, v3, v2, v1, v0] =>
  UNLESS curts DO bug "prterm\n"

  writef("%5d:", (p-curts!TsTerm1)/(4*Tsize))
  FOR i = 0 TO 7 DO writef(" %8d", (@v7)!i)
  writes "\n      "
  FOR i = 0 TO 7 DO writef(" %8x", (@r7)!i)
  newline()

FUN prterms :  =>
  UNLESS curts DO bug "prterms\n"

  LET n = curts!TsTn      // The number of terms
  LET p = curts!TsTerm1

  writef("Terms:\n")

  FOR i = 1 TO n DO { IF p!Tv7>=0 OR TRUE DO prterm p
                      p := @p!Tsize
                    }
  prmapping()

FUN prmapping : =>
  UNLESS curts DO bug "prmapping\n"

  LET k = 1            // For layout
  writef "Mapping:"
  UNLESS varmap DO { writef " No mapping vector\n"; RETURN }
  FOR i = 0 TO curts!TsVmax IF varmap!i DO
  { UNLESS k++ MOD 8 DO newline()
    LET y = varmap!i
    IF y=-1 DO y := 0
    writef(" %d%c%d", i, (y<0->'#','='), ABS y)
  }
  newline()


FUN check :  =>   // It may raise E_FalseTermFound
                  //              E_NoTerms
                  //              E_Space
  UNLESS curts DO bug "check\n"
//test_swaps()
  compact_vars()
  IF debug>=6 DO prterms()
  apply_simple_rules()
  //apply_dilemma_rules()


FUN intersect_maps : vm, vm1, n => // Intersect maps vm, vm1 -> vm
                                   // returs TRUE if non-empty.
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

FUN apply_dilemma_rules :  =>
  seed := 0  // To make runs repeatable.
  // Depths of more than about 5 are more or less hopeless
  FOR depth = 1 TO 5 DO dilemma depth

FUN dilemma : depth =>
  compact_terms()
  compact_vars()

  LET vmax = curts!TsVmax

  UNLESS varmap DO bug "varmap not allocated\n"

  IF debug>=8 DO { writes("\nApply dilemma rule, depth %d, to:\n", depth)
                   prterms()
                 }

  LET used = getvec vmax
  UNLESS used RAISE (E_Space, "In delemma\n")

  FOR v0 = 3 TO vmax DO
  { FOR v = 0 TO vmax DO used!v := 0

    { LET p = curts!TsTerm1     // Form usage histogram
      FOR i = 1 TO curts!TsTn EVERY p
      : [     r7,r6,r5,r4,r3,r2,r1,r0,
         (>=0)v7,v6,v5,v4,v3,v2,v1,v0, np] => (used!v7)++
                                              (used!v6)++
                                              (used!v5)++
                                              (used!v4)++
                                              (used!v3)++
                                              (used!v2)++
                                              (used!v1)++
                                              (used!v0)++
      : [     r7,r6,r5,r4,r3,r2,r1,r0,
              v7,v6,v5,v4,v3,v2,v1,v0, np] => p := @np
    }
    UNTIL used!v0 OR v0>vmax DO v0++
    IF v0>vmax BREAK

    LET m0 = mapof v0
    IF m0=0 OR m0=1 LOOP // Is v0 already specified?
    IF depth=1 DO { tryall(1, v0)
                    LOOP
                  }

    FOR v1 = v0+1 TO vmax IF used!v1 DO
    { LET m1 = mapof v1
      IF m1=0 OR m1=1 LOOP    // Is v1 already specified?
      IF ABS m1 = ABS m0 LOOP // Are v1 and v0 related?
      IF m0=0 OR m0=1 BREAK   // Has v0 become specified?

      // v0 and v1 are independent unspecified variables.
      IF depth=2 DO { tryall(2, v0,v1)
                      m0 := mapof v0
                      LOOP
                    }

      FOR v2 = v1+1 TO vmax IF used!v2 DO
      { LET m2 = mapof v2
        IF m2=0 OR m2=1 LOOP    // Is v2 already specified?
        IF ABS m2 = ABS m1 LOOP // Are v2 and v1 related?
        IF ABS m2 = ABS m0 LOOP // Are v2 and v0 related?
        IF m1=0 OR m1=1 BREAK   // Has v1 become specified?
        IF m0=0 OR m0=1 BREAK   // Has v0 become specified?

        // v0, v1 and v2 are independent unspecified variables.
        IF depth=3 DO { tryall(3, v0,v1,v2)
                        m1 := mapof v1
                        m0 := mapof v0
                        LOOP
                      }

        FOR v3 = v2+1 TO vmax IF used!v3 DO
        { LET m3 = mapof v3
          IF m3=0 OR m3=1 LOOP    // Is v3 already specified?
          IF ABS m3 = ABS m2 LOOP // Are v3 and v2 related?
          IF ABS m3 = ABS m1 LOOP // Are v3 and v1 related?
          IF ABS m3 = ABS m0 LOOP // Are v3 and v0 related?
          IF m2=0 OR m2=1 BREAK   // Has v2 become specified?
          IF m1=0 OR m1=1 BREAK   // Has v1 become specified?
          IF m0=0 OR m0=1 BREAK   // Has v0 become specified?

          // v0, v1, v2 and v3 are independent unspecified variables.
          IF depth=4 DO { tryall(4, v0,v1,v2,v3)
                          m2 := mapof v2
                          m1 := mapof v1
                          m0 := mapof v0
                          LOOP
                        }

          FOR v4 = v3+1 TO vmax IF used!v4 DO
          { LET m4 = mapof v4
            IF m4=0 OR m4=1 LOOP    // Is v4 already specified?
            IF ABS m4 = ABS m3 LOOP // Are v4 and v3 related?
            IF ABS m4 = ABS m2 LOOP // Are v4 and v2 related?
            IF ABS m4 = ABS m1 LOOP // Are v4 and v1 related?
            IF ABS m4 = ABS m0 LOOP // Are v4 and v0 related?
            IF m3=0 OR m3=1 BREAK   // Has v3 become specified?
            IF m2=0 OR m2=1 BREAK   // Has v2 become specified?
            IF m1=0 OR m1=1 BREAK   // Has v1 become specified?
            IF m0=0 OR m0=1 BREAK   // Has v0 become specified?

            // v0, v1, v2, v3 and v4 are independent unspecified variables.
            IF depth=5 DO { tryall(5, v0,v1,v2,v3,v4)
                            m3 := mapof v3
                            m2 := mapof v2
                            m1 := mapof v1
                            m0 := mapof v0
                            LOOP
                          }
            writef("Unable to apply dilemma rule to depth %d\n", depth)
            RETURN
          }
        }
      }
    }
  }
  freevec used

STATIC seed=0

FUN tryall : depth, v0,v1,v2,v3,v4 =>
  LET oldts = curts
  LET vmax = curts!TsVmax
                             // depth= 1,  2,  3,  4,  5, ...
  LET mask = 1<<depth - 1    //        1,  3,  7, 15, 31, ...
  LET inc  = 2<<(depth/2)-1  //        1,  3,  3,  7,  7, ...
  seed := (seed+1) & mask    // Choose a different starting point each time
  LET bits = seed

  IF debug>=6 DO
  { writef "tryall: "
    FOR i = 0 TO depth-1 DO writef(" %3d", (@v0)!i)
    newline()
  }

  IF debug>=10 DO writef("depth=%d inc=%5b  mask=%5b seed=%5b\n",
                          depth,   inc,     mask,    seed)
  LET interv = 0

  { bits := (bits+inc) & mask
    LET nts = mkcopy_termset()
    select_ts nts
    UNLESS varmap DO bug "varmap should be allocated\n"

    // Set the selected variables.
    { LET vp = @v0
      LET settings = bits
      FOR i = 1 TO depth DO
      { LET var = !vp+++
        TEST settings&1 THEN setmap(varmap, var,  1)
                        ELSE setmap(varmap, var, -1)
        settings >>:= 1
      }
    }

    apply_simple_rules()
    HANDLE : E_FalseTermFound =>
                  select_ts nts
                  close_termset()
                  select_ts oldts
                  IF debug>=5 DO 
                    writef("####### setting inconsistent\n")
                  LOOP
           : E_NoTerms =>           // All terms have been satisfied
                  select_ts nts
                  close_termset()
                  select_ts oldts
                  RAISE E_NoTerms
           .

    // Simple rules returned normally (failed to find a solution yet).
    TEST interv
    THEN UNLESS intersect_maps(interv, varmap, vmax) BREAK // If empty.
    ELSE { interv := varmap
           varmap := 0
           nts!TsVm := 0  // So that the varmap is not deallocated.
         }
    select_ts nts
    close_termset()
    select_ts oldts
  } REPEATUNTIL bits=seed

  IF interv=0 DO // All settings lead to inconsistencies.
  { IF debug>=5 DO writef "All settings inconsistent\n"
    RAISE E_FalseTermFound
  }

  select_ts oldts
  combine_maps interv // Add in newly found mappings.
  freevec interv

  apply_simple_rules()

  IF debug>=4 DO
  { LET k = 0
    LET p = curts!TsTerm1
    FOR i = 1 TO curts!TsTn EVERY p
    : [r7,r6,r5,r4,r3,r2,r1,r0,>=0,v6,v5,v4,v3,v2,v1,v0,np] => k++
    : [r7,r6,r5,r4,r3,r2,r1,r0, v7,v6,v5,v4,v3,v2,v1,v0,np] => p := @np
    .
    //prterms()
    writef("Non deleted terms %3d after dilemma rule", k)
    FOR i = 0 TO depth-1 DO writef(" %3d", (@v0)!i)
    newline()
  }

//********************** Apply Simple Rules **********************

STATIC
  change  // Set to TRUE whenever any change is made.

FUN apply_simple_rules : =>
             // It may raise E_FalseTermFound
             //              E_NoTerms
             //              E_Space

  IF debug>=8 DO writef "\nApplying simple rules\n"

  UNLESS varmap DO bug "Found in apply_simple_rules\n"

  IF debug>=7 DO { writef "Terms before applying simple rules:\n"
                   prterms()
                 }

  change := FALSE

  { apply_unit_rules()

    IF debug>=8 DO prterms()

    apply_pair_rules()

    compact_terms()
    IF curts!TsTn=0 RAISE E_NoTerms
    IF debug>=8 DO prterms()

  } REPEATWHILE change

FUN mapterm : p[ r7, r6, r5, r4, r3, r2, r1, r0,
                 v7, v6, v5, v4, v3, v2, v1, v0] =>
  // On entry, x, y and z >=0
  // On return, x =0 or >2,
  //            y =0 or >2
  //        and z =0 or >2
  // It may change rel.
  // It raises no exceptions.

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


FUN canonterm              // This canonicalizes a given term. It is
                           // guaranteed not to raise an exception.

:p[ r7,   r6, r5, r4, r3, r2, r1, r0,
    (<0), v6, v5, v4, v3, v2, v1, v0] => RETURN // A void term

:p[ r7, r6, r5, r4, r3, r2, r1, r0,
    v7, v6, v5, v4, v3, v2, v1, v0] =>

  IF debug>=12 DO { writes "Canon:\n"; prterm p }
  mapterm p
  IF debug>=12 DO { writes "Mapped:\n"; prterm p }
  // x =0 or >2, y =0 or >2  and z =0 or >2

  // If v7 ignored, set v7=0 and change the relation
  IF r7=r3 AND r6=r2 AND r5=r1 AND r4=r0 DO dcv7 p
writes "Canon v7:\n"; prterm p
 
  // If v6 ignored, set v6=0 and change the relation
  IF r7=r5 AND r6=r4 AND r3=r1 AND r2=r0 DO dcv6 p
writes "Canon v6:\n"; prterm p

  // If v5 ignored, set v5=0 and change the relation
  IF r7=r6 AND r5=r4 AND r3=r2 AND r1=r0 DO dcv5 p
writes "Canon v5:\n"; prterm p

  // If v4 ignored, set v4=0 and change the relation
  UNLESS (r7>>16 XOR r7) & #x0000FFFF OR
         (r6>>16 XOR r6) & #x0000FFFF OR
         (r5>>16 XOR r5) & #x0000FFFF OR
         (r4>>16 XOR r4) & #x0000FFFF OR
         (r3>>16 XOR r3) & #x0000FFFF OR
         (r2>>16 XOR r2) & #x0000FFFF OR
         (r1>>16 XOR r1) & #x0000FFFF OR
         (r0>>16 XOR r0) & #x0000FFFF DO dcv4 p
writes "Canon v4:\n"; prterm p

  // If v3 ignored, set v3=0 and change the relation
  UNLESS (r7>>8 XOR r7) & #x00FF00FF OR
         (r6>>8 XOR r6) & #x00FF00FF OR
         (r5>>8 XOR r5) & #x00FF00FF OR
         (r4>>8 XOR r4) & #x00FF00FF OR
         (r3>>8 XOR r3) & #x00FF00FF OR
         (r2>>8 XOR r2) & #x00FF00FF OR
         (r1>>8 XOR r1) & #x00FF00FF OR
         (r0>>8 XOR r0) & #x00FF00FF DO dcv3 p
writes "Canon v3:\n"; prterm p

  // If v2 ignored, set v2=0 and change the relation
  UNLESS (r7>>4 XOR r7) & #x0F0F0F0F OR
         (r6>>4 XOR r6) & #x0F0F0F0F OR
         (r5>>4 XOR r5) & #x0F0F0F0F OR
         (r4>>4 XOR r4) & #x0F0F0F0F OR
         (r3>>4 XOR r3) & #x0F0F0F0F OR
         (r2>>4 XOR r2) & #x0F0F0F0F OR
         (r1>>4 XOR r1) & #x0F0F0F0F OR
         (r0>>4 XOR r0) & #x0F0F0F0F DO dcv2 p
writes "Canon v2:\n"; prterm p

  // If v1 ignored, set v1=0 and change the relation
  UNLESS (r7>>2 XOR r7) & #x33333333 OR
         (r6>>2 XOR r6) & #x33333333 OR
         (r5>>2 XOR r5) & #x33333333 OR
         (r4>>2 XOR r4) & #x33333333 OR
         (r3>>2 XOR r3) & #x33333333 OR
         (r2>>2 XOR r2) & #x33333333 OR
         (r1>>2 XOR r1) & #x33333333 OR
         (r0>>2 XOR r0) & #x33333333 DO dcv1 p
writes "Canon v1:\n"; prterm p

  // If v0 ignored, set v0=0 and change the relation
  UNLESS (r7>>1 XOR r7) & #x55555555 OR
         (r6>>1 XOR r6) & #x55555555 OR
         (r5>>1 XOR r5) & #x55555555 OR
         (r4>>1 XOR r4) & #x55555555 OR
         (r3>>1 XOR r3) & #x55555555 OR
         (r2>>1 XOR r2) & #x55555555 OR
         (r1>>1 XOR r1) & #x55555555 OR
         (r0>>1 XOR r0) & #x55555555 DO dcv0 p
writes "Canon v0:\n"; prterm p

  IF v7=1 DO { not7 p; v7 := 0 } // rel ( 1,v6,v5,v4,v3,v2,v1,v0) =>
                                 // rel'( 0,v6,v5,v4,v3,v2,v1,v0)
writes "Canon nv7:\n"; prterm p

  IF v6=1 DO { not6 p; v6 := 0 } // rel (v7, 1,v5,v4,v3,v2,v1,v0) =>
                                 // rel'(v7, 0,v5,v4,v3,v2,v1,v0)
writes "Canon nv6:\n"; prterm p

  IF v5=1 DO { not5 p; v5 := 0 } // rel (v7,v6, 1,v4,v3,v2,v1,v0) =>
                                 // rel'(v7,v6, 0,v4,v3,v2,v1,v0)
writes "Canon nv5:\n"; prterm p

  IF v4=1 DO { not4 p; v4 := 0 } // rel (v7,v6,v5, 1,v3,v2,v1,v0) =>
                                 // rel'(v7,v6,v5, 0,v3,v2,v1,v0)
writes "Canon nv4:\n"; prterm p

  IF v3=1 DO { not3 p; v3 := 0 } // rel (v7,v6,v5,v4, 1,v2,v1,v0) =>
                                 // rel'(v7,v6,v5,v4, 0,v2,v1,v0)
writes "Canon nv3:\n"; prterm p

  IF v2=1 DO { not2 p; v2 := 0 } // rel (v7,v6,v5,v4,v3, 1,v1,v0) =>
                                 // rel'(v7,v6,v5,v4,v3, 0,v1,v0)
writes "Canon nv2:\n"; prterm p

  IF v1=1 DO { not1 p; v1 := 0 } // rel (v7,v6,v5,v4,v3,v2, 1,v0) =>
                                 // rel'(v7,v6,v5,v4,v3,v2, 0,v0)
writes "Canon nv1:\n"; prterm p

  IF v0=1 DO { not0 p; v0 := 0 } // rel (v7,v6,v5,v4,v3,v2,v1, 1) =>
                                 // rel'(v7,v6,v5,v4,v3,v2,v1, 0)
writes "Canon nv0:\n"; prterm p


  // Sort the relation arguments v7 v6 v5 v4 v3 v2 v1 v0
  // minimising swaps, and getting rid of duplicate variables.
  LET mv = v0
  LET swfn = 0
  IF mv<v1 DO mv, swfn := v1, sw10
  IF mv<v2 DO mv, swfn := v2, sw20
  IF mv<v3 DO mv, swfn := v3, sw30
  IF mv<v4 DO mv, swfn := v4, sw40
  IF mv<v5 DO mv, swfn := v5, sw50
  IF mv<v6 DO mv, swfn := v6, sw60
  IF mv<v7 DO mv, swfn := v7, sw70
  IF swfn DO swfn p

  WHILE v0 DO
  { mv  := v1
    swfn := 0
    IF mv<v2 DO mv, swfn := v2, sw21
    IF mv<v3 DO mv, swfn := v3, sw31
    IF mv<v4 DO mv, swfn := v4, sw41
    IF mv<v5 DO mv, swfn := v5, sw51
    IF mv<v6 DO mv, swfn := v6, sw61
    IF mv<v7 DO mv, swfn := v7, sw71
    IF swfn DO swfn p
    UNLESS v1=v0 BREAK
    // v1=v0>0
    v1 := 0                                  // Get rid of v1.
    r7 := r7>>2 & #x22222222 | r7&#x11111111  
    r6 := r6>>2 & #x22222222 | r6&#x11111111  
    r5 := r5>>2 & #x22222222 | r5&#x11111111  
    r4 := r4>>2 & #x22222222 | r4&#x11111111  
    r3 := r3>>2 & #x22222222 | r3&#x11111111  
    r2 := r2>>2 & #x22222222 | r2&#x11111111  
    r1 := r1>>2 & #x22222222 | r1&#x11111111  
    r0 := r0>>2 & #x22222222 | r0&#x11111111  
  }

  WHILE v1 DO
  { mv  := v2
    swfn := 0
    IF mv<v3 DO mv, swfn := v3, sw32
    IF mv<v4 DO mv, swfn := v4, sw42
    IF mv<v5 DO mv, swfn := v5, sw52
    IF mv<v6 DO mv, swfn := v6, sw62
    IF mv<v7 DO mv, swfn := v7, sw72
    IF swfn DO swfn p
    UNLESS v2=v1 BREAK
    // v2=v1>0
    v2 := 0                                  // Get rid of v2.
    r7 := r7>>4 & #x0C0C0C0C | r7&#x03030303  
    r6 := r6>>4 & #x0C0C0C0C | r6&#x03030303  
    r5 := r5>>4 & #x0C0C0C0C | r5&#x03030303  
    r4 := r4>>4 & #x0C0C0C0C | r4&#x03030303  
    r3 := r3>>4 & #x0C0C0C0C | r3&#x03030303  
    r2 := r2>>4 & #x0C0C0C0C | r2&#x03030303  
    r1 := r1>>4 & #x0C0C0C0C | r1&#x03030303  
    r0 := r0>>4 & #x0C0C0C0C | r0&#x03030303  
  }

  WHILE v2 DO
  { mv  := v3
    swfn := 0
    IF mv<v4 DO mv, swfn := v4, sw43
    IF mv<v5 DO mv, swfn := v5, sw53
    IF mv<v6 DO mv, swfn := v6, sw63
    IF mv<v7 DO mv, swfn := v7, sw73
    IF swfn DO swfn p
    UNLESS v3=v2 BREAK
    // v3=v2>0
    v3 := 0                                  // Get rid of v3.
    r7 := r7>>8 & #x00F000F0 | r7&#x000F000F  
    r6 := r6>>8 & #x00F000F0 | r6&#x000F000F  
    r5 := r5>>8 & #x00F000F0 | r5&#x000F000F  
    r4 := r4>>8 & #x00F000F0 | r4&#x000F000F  
    r3 := r3>>8 & #x00F000F0 | r3&#x000F000F  
    r2 := r2>>8 & #x00F000F0 | r2&#x000F000F  
    r1 := r1>>8 & #x00F000F0 | r1&#x000F000F  
    r0 := r0>>8 & #x00F000F0 | r0&#x000F000F  
  }

  WHILE v3 DO
  { mv  := v4
    swfn := 0
    IF mv<v5 DO mv, swfn := v5, sw54
    IF mv<v6 DO mv, swfn := v6, sw64
    IF mv<v7 DO mv, swfn := v7, sw74
    IF swfn DO swfn p
    UNLESS v4=v3 BREAK
    // v4=v3>0
    v4 := 0                                  // Get rid of v4.
    r7 := r7>>16 & #x0000FF00 | r7&#x000000FF  
    r6 := r6>>16 & #x0000FF00 | r6&#x000000FF  
    r5 := r5>>16 & #x0000FF00 | r5&#x000000FF  
    r4 := r4>>16 & #x0000FF00 | r4&#x000000FF  
    r3 := r3>>16 & #x0000FF00 | r3&#x000000FF  
    r2 := r2>>16 & #x0000FF00 | r2&#x000000FF  
    r1 := r1>>16 & #x0000FF00 | r1&#x000000FF  
    r0 := r0>>16 & #x0000FF00 | r0&#x000000FF  
  }

  WHILE v4 DO
  { mv  := v5
    swfn := 0
    IF mv<v6 DO mv, swfn := v6, sw65
    IF mv<v7 DO mv, swfn := v7, sw75
    IF swfn DO swfn p
    UNLESS v5=v4 BREAK
    // v5=v4>0
    v5 := 0                                  // Get rid of v5.
    r7, r6 := 0, r7&#xFFFF0000 | r6&#x0000FFFF  
    r5, r4 := 0, r5&#xFFFF0000 | r4&#x0000FFFF  
    r3, r2 := 0, r3&#xFFFF0000 | r2&#x0000FFFF  
    r1, r0 := 0, r1&#xFFFF0000 | r0&#x0000FFFF  
  }

  WHILE v5 DO
  { mv  := v6
    swfn := 0
    IF mv<v7 DO mv, swfn := v7, sw76
    IF swfn DO swfn p
    UNLESS v6=v5 BREAK
    // v6=v5>0
    v6 := 0                                  // Get rid of v6.
    r3, r2, r1 := 0, 0, r3
    r7, r6, r5 := 0, 0, r7
  }

  IF v6 AND v7=v6 DO
  { // v7=v6>0
    v7 := 0                                  // Get rid of v7.
    r3, r2 := r7, r6
    r7, r6, r5, r4 := 0, 0, 0, 0
  }
writes "Canon rid:\n"; prterm p


  IF v7=0 DO r7, r6, r5, r4 := 0, 0, 0, 0
  IF v6=0 DO r7, r6, r3, r2 := 0, 0, 0, 0
  IF v5=0 DO r7, r5, r3, r1 := 0, 0, 0, 0
  IF v4=0 DO { r7 &:= #x0000FFFF
               r6 &:= #x0000FFFF
               r5 &:= #x0000FFFF
               r4 &:= #x0000FFFF
               r3 &:= #x0000FFFF
               r2 &:= #x0000FFFF
               r1 &:= #x0000FFFF
               r0 &:= #x0000FFFF
             }
  IF v3=0 DO { r7 &:= #x00FF00FF
               r6 &:= #x00FF00FF
               r5 &:= #x00FF00FF
               r4 &:= #x00FF00FF
               r3 &:= #x00FF00FF
               r2 &:= #x00FF00FF
               r1 &:= #x00FF00FF
               r0 &:= #x00FF00FF
             }
  IF v2=0 DO { r7 &:= #x0F0F0F0F
               r6 &:= #x0F0F0F0F
               r5 &:= #x0F0F0F0F
               r4 &:= #x0F0F0F0F
               r3 &:= #x0F0F0F0F
               r2 &:= #x0F0F0F0F
               r1 &:= #x0F0F0F0F
               r0 &:= #x0F0F0F0F
             }
  IF v1=0 DO { r7 &:= #x33333333
               r6 &:= #x33333333
               r5 &:= #x33333333
               r4 &:= #x33333333
               r3 &:= #x33333333
               r2 &:= #x33333333
               r1 &:= #x33333333
               r0 &:= #x33333333
             }
  IF v0=0 DO { r7 &:= #x55555555
               r6 &:= #x55555555
               r5 &:= #x55555555
               r4 &:= #x55555555
               r3 &:= #x55555555
               r2 &:= #x55555555
               r1 &:= #x55555555
               r0 &:= #x55555555
             }
writes "Canon done:\n"; prterm p


  IF ( r7=#xFFFFFFFF AND r6=#xFFFFFFFF AND
       r5=#xFFFFFFFF AND r4=#xFFFFFFFF AND
       r3=#xFFFFFFFF AND r2=#xFFFFFFFF AND
       r1=#xFFFFFFFF AND r0=#xFFFFFFFF ) OR
     v7=0 AND
     ( r3=#xFFFFFFFF AND r2=#xFFFFFFFF AND
       r1=#xFFFFFFFF AND r0=#xFFFFFFFF OR
       v6=0 AND
       ( r1=#xFFFFFFFF AND r0=#xFFFFFFFF OR
         v5=0 AND
         ( r0=#xFFFFFFFF OR
           v4=0 AND
           ( r0=#x0000FFFF OR
             v3=0 AND
             ( r0=#x000000FF OR
               v2=0 AND
               ( r0=#x0000000F OR
                 v1=0 AND
                 ( r0=#x00000003 OR
                   v0=0 AND r0=#x00000001
                 )
               )
             )
           )
         )
       )
     ) DO v7, change := -1, TRUE

  IF debug>=12 DO { writes "=====>\n"; prterm p }


FUN apply_unit_rules : =>
  // This function applies the mapping (in
  // vector curts!TsVm) to the terms in curts, and
  // applies the unit rules until convergence,
  // leaving the final mapping in the varmap.

  { change := FALSE // changes to TRUE if the variable mapping changes

    IF debug>=7 DO writef("\nApplying unit rules\n")

    IF debug>=7 DO prterms()

    LET n = curts!TsTn
    LET p = curts!TsTerm1
    LET k = 0 // Count of non-deleted terms
    FOR i = 1 TO n DO { UNLESS !p<0 DO { k++; apply_unit_rule p }
                        p := @p!Tsize
                      }
    IF k=0 DO { IF debug>=5 DO writef "No non-deleted terms\n"
                RAISE E_NoTerms
              }
  } REPEATWHILE change // repeat if the mapping has changed

FUN apply_unit_rule
:  [r7,r6,r5,r4,r3,r2,r1,r0,<0,v6,v5,v4,v3,v2,v1,v0] => // A deleted term 

: p[r7,r6,r5,r4,r3,r2,r1,r0,v7,v6,v5,v4,v3,v2,v1,v0] =>
  { canonterm p

    IF v7<0 DO { IF debug>=5 DO { writef "void:\n"; prterm p } 
                 RETURN
               }

    UNLESS r7|r6|r5|r4|r3|r2|r1|r0 DO
    { IF debug>=5 DO { writef "unsat:\n"; prterm p }
      RAISE E_FalseTermFound
    }

    UNLESS v7=0 DO
    { UNLESS r7|r6|r5|r4 DO    // Test for action v7=0
      { IF debug>=5 DO { prterm p; writef("Action: %5d=0\n", v7) }
         setmap(varmap, v7, -1)
         LOOP
      }
      UNLESS r3|r2|r1|r0 DO    // Test for action v7=1
      { IF debug>=5 DO { prterm p; writef("Action: %5d=1\n", v7) }
         setmap(varmap, v7, 1)
         LOOP
      }
    }

    UNLESS v6=0 DO
    { UNLESS r7|r6|r3|r2 DO    // Test for action v6=0
      { IF debug>=5 DO { prterm p; writef("Action: %5d=0\n", v6) }
         setmap(varmap, v6, -1)
         LOOP
      }
      UNLESS r5|r4|r1|r0 DO    // Test for action v6=1
      { IF debug>=5 DO { prterm p; writef("Action: %5d=1\n", v6) }
         setmap(varmap, v6, 1)
         LOOP
      }
    }

    UNLESS v5=0 DO
    { UNLESS r7|r5|r3|r1 DO    // Test for action v5=0
      { IF debug>=5 DO { prterm p; writef("Action: %5d=0\n", v5) }
         setmap(varmap, v5, -1)
         LOOP
      }
      UNLESS r6|r4|r2|r0 DO    // Test for action v5=1
      { IF debug>=5 DO { prterm p; writef("Action: %5d=1\n", v5) }
         setmap(varmap, v5, 1)
         LOOP
      }
    }

    LET all = r7|r6|r5|r4|r3|r2|r1|r0

    UNLESS v4=0 DO
    { UNLESS all & #xFFFF0000 DO    // Test for action v4=0
      { IF debug>=5 DO { prterm p; writef("Action: %5d=0\n", v4) }
         setmap(varmap, v4, -1)
         LOOP
      }
      UNLESS all & #x0000FFFF DO    // Test for action v4=1
      { IF debug>=5 DO { prterm p; writef("Action: %5d=1\n", v4) }
         setmap(varmap, v4, 1)
         LOOP
      }
    }

    UNLESS v3=0 DO
    { UNLESS all & #xFF00FF00 DO    // Test for action v3=0
      { IF debug>=5 DO { prterm p; writef("Action: %5d=0\n", v3) }
         setmap(varmap, v3, -1)
         LOOP
      }
      UNLESS all & #x00FF00FF DO    // Test for action v3=1
      { IF debug>=5 DO { prterm p; writef("Action: %5d=1\n", v3) }
         setmap(varmap, v3, 1)
         LOOP
      }
    }

    UNLESS v2=0 DO
    { UNLESS all & #xF0F0F0F0 DO    // Test for action v2=0
      { IF debug>=5 DO { prterm p; writef("Action: %5d=0\n", v2) }
         setmap(varmap, v2, -1)
         LOOP
      }
      UNLESS all & #x0F0F0F0F DO    // Test for action v2=1
      { IF debug>=5 DO { prterm p; writef("Action: %5d=1\n", v2) }
         setmap(varmap, v2, 1)
         LOOP
      }
    }

    UNLESS v1=0 DO
    { UNLESS all & #xCCCCCCCC DO    // Test for action v1=0
      { IF debug>=5 DO { prterm p; writef("Action: %5d=0\n", v1) }
         setmap(varmap, v1, -1)
         LOOP
      }
      UNLESS all & #x33333333 DO    // Test for action v1=1
      { IF debug>=5 DO { prterm p; writef("Action: %5d=1\n", v1) }
         setmap(varmap, v1, 1)
         LOOP
      }
    }

    UNLESS v0=0 DO
    { UNLESS all & #xAAAAAAAA DO    // Test for action v0=0
      { IF debug>=5 DO { prterm p; writef("Action: %5d=0\n", v0) }
         setmap(varmap, v0, -1)
         LOOP
      }
      UNLESS all & #x55555555 DO    // Test for action v0=1
      { IF debug>=5 DO { prterm p; writef("Action: %d=1\n", v0) }
         setmap(varmap, v0, 1)
         LOOP
      }
    }


    UNLESS v7&v6=0 DO
    { UNLESS r5|r4|r3|r2 DO    // Test for action v6=v7
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v6, v7) }
         setmap(varmap, v6, v7)
         LOOP
      }
      UNLESS r7|r6|r1|r0 DO    // Test for action v6#v7
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v6, v7) }
         setmap(varmap, v6, -v7)
         LOOP
      }
    }

    UNLESS v7&v5=0 DO
    { UNLESS r6|r4|r3|r1 DO    // Test for action v5=v7
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v5, v7) }
         setmap(varmap, v5, v7)
         LOOP
      }
      UNLESS r7|r5|r2|r0 DO    // Test for action v6#v7
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v5, v7) }
         setmap(varmap, v5, -v7)
         LOOP
      }
    }

    LET r7654 = r7|r6|r5|r4
    LET r3210 = r3|r2|r1|r0

    UNLESS v7&v4=0 DO
    { UNLESS r7654&#x0000FFFF OR r3210&#xFFFF0000 DO    // Test for v4=v7
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v4, v7) }
         setmap(varmap, v4, v7)
         LOOP
      }
      UNLESS r7654&#xFFFF0000 OR r3210&#x0000FFFF DO    // Test for v4#v7
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v4, v7) }
         setmap(varmap, v4, -v7)
         LOOP
      }
    }

    UNLESS v7&v3=0 DO
    { UNLESS r7654&#x00FF00FF OR r3210&#xFF00FF00 DO    // Test for v3=v7
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v3, v7) }
         setmap(varmap, v3, v7)
         LOOP
      }
      UNLESS r7654&#xFF00FF00 OR r3210&#x00FF00FF DO    // Test for v3#v7
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v3, v7) }
         setmap(varmap, v3, -v7)
         LOOP
      }
    }

    UNLESS v7&v2=0 DO
    { UNLESS r7654&#x0F0F0F0F OR r3210&#xF0F0F0F0 DO    // Test for v2=v7
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v2, v7) }
         setmap(varmap, v2, v7)
         LOOP
      }
      UNLESS r7654&#xF0F0F0F0 OR r3210&#x0F0F0F0F DO    // Test for v2#v7
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v2, v7) }
         setmap(varmap, v2, -v7)
         LOOP
      }
    }

    UNLESS v7&v1=0 DO
    { UNLESS r7654&#x33333333 OR r3210&#xCCCCCCCC DO    // Test for v1=v7
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v1, v7) }
         setmap(varmap, v1, v7)
         LOOP
      }
      UNLESS r7654&#xCCCCCCCC OR r3210&#x33333333 DO    // Test for v1#v7
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v1, v7) }
         setmap(varmap, v1, -v7)
         LOOP
      }
    }

    UNLESS v7&v0=0 DO
    { UNLESS r7654&#x55555555 OR r3210&#xAAAAAAAA DO    // Test for v0=v7
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v0, v7) }
         setmap(varmap, v0, v7)
         LOOP
      }
      UNLESS r7654&#xAAAAAAAA OR r3210&#x55555555 DO    // Test for v0#v7
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v0, v7) }
         setmap(varmap, v0, -v7)
         LOOP
      }
    }

    UNLESS v6&v5=0 DO
    { UNLESS r6|r5|r2|r1 DO    // Test for action v5=v6
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v5, v6) }
         setmap(varmap, v5, v6)
         LOOP
      }
      UNLESS r7|r4|r3|r0 DO    // Test for action v5#v6
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v5, v6) }
         setmap(varmap, v5, -v6)
         LOOP
      }
    }

    LET r7632 = r7|r6|r3|r2
    LET r5410 = r5|r4|r1|r0

    UNLESS v6&v4=0 DO
    { UNLESS r7632&#x0000FFFF OR r5410&#xFFFF0000 DO    // Test for v4=v6
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v4, v6) }
         setmap(varmap, v4, v6)
         LOOP
      }
      UNLESS r7632&#xFFFF0000 OR r5410&#x0000FFFF DO    // Test for v4#v6
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v4, v6) }
         setmap(varmap, v4, -v6)
         LOOP
      }
    }

    UNLESS v6&v3=0 DO
    { UNLESS r7632&#x00FF00FF OR r5410&#xFF00FF00 DO    // Test for v3=v6
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v3, v6) }
         setmap(varmap, v3, v6)
         LOOP
      }
      UNLESS r7632&#xFF00FF00 OR r5410&#x00FF00FF DO    // Test for v3#v6
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v3, v6) }
         setmap(varmap, v3, -v6)
         LOOP
      }
    }

    UNLESS v6&v2=0 DO
    { UNLESS r7632&#x0F0F0F0F OR r5410&#xF0F0F0F0 DO    // Test for v2=v6
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v2, v6) }
         setmap(varmap, v2, v6)
         LOOP
      }
      UNLESS r7632&#xF0F0F0F0 OR r5410&#x0F0F0F0F DO    // Test for v2#v6
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v2, v6) }
         setmap(varmap, v2, -v6)
         LOOP
      }
    }

    UNLESS v6&v1=0 DO
    { UNLESS r7632&#x33333333 OR r5410&#xCCCCCCCC DO    // Test for v1=v6
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v1, v6) }
         setmap(varmap, v1, v6)
         LOOP
      }
      UNLESS r7632&#xCCCCCCCC OR r5410&#x33333333 DO    // Test for v1#v6
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v1, v6) }
         setmap(varmap, v1, -v6)
         LOOP
      }
    }

    UNLESS v6&v0=0 DO
    { UNLESS r7632&#x55555555 OR r5410&#xAAAAAAAA DO    // Test for v0=v6
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v0, v6) }
         setmap(varmap, v0, v6)
         LOOP
      }
      UNLESS r7632&#xAAAAAAAA OR r5410&#x55555555 DO    // Test for v0#v6
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v0, v6) }
         setmap(varmap, v0, -v6)
         LOOP
      }
    }

    LET r7531 = r7|r5|r3|r1
    LET r6420 = r6|r4|r2|r0

    UNLESS v5&v4=0 DO
    { UNLESS r7531&#x0000FFFF OR r6420&#xFFFF0000 DO    // Test for v4=v5
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v4, v5) }
         setmap(varmap, v4, v5)
         LOOP
      }
      UNLESS r7531&#xFFFF0000 OR r6420&#x0000FFFF DO    // Test for v4#v5
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v4, v5) }
         setmap(varmap, v4, -v5)
         LOOP
      }
    }

    UNLESS v5&v3=0 DO
    { UNLESS r7531&#x00FF00FF OR r6420&#xFF00FF00 DO    // Test for v3=v5
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v3, v5) }
         setmap(varmap, v3, v5)
         LOOP
      }
      UNLESS r7531&#xFF00FF00 OR r6420&#x00FF00FF DO    // Test for v3#v5
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v3, v5) }
         setmap(varmap, v3, -v5)
         LOOP
      }
    }

    UNLESS v5&v2=0 DO
    { UNLESS r7531&#x0F0F0F0F OR r6420&#xF0F0F0F0 DO    // Test for v2=v5
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v2, v5) }
         setmap(varmap, v2, v5)
         LOOP
      }
      UNLESS r7531&#xF0F0F0F0 OR r6420&#x0F0F0F0F DO    // Test for v2#v5
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v2, v5) }
         setmap(varmap, v2, -v5)
         LOOP
      }
    }

    UNLESS v5&v1=0 DO
    { UNLESS r7531&#x33333333 OR r6420&#xCCCCCCCC DO    // Test for v1=v5
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v1, v5) }
         setmap(varmap, v1, v5)
         LOOP
      }
      UNLESS r7531&#xCCCCCCCC OR r6420&#x33333333 DO    // Test for v1#v5
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v1, v5) }
         setmap(varmap, v1, -v5)
         LOOP
      }
    }

    UNLESS v5&v0=0 DO
    { UNLESS r7531&#x55555555 OR r6420&#xAAAAAAAA DO    // Test for v0=v5
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v0, v5) }
         setmap(varmap, v0, v5)
         LOOP
      }
      UNLESS r7531&#xAAAAAAAA OR r6420&#x55555555 DO    // Test for v0#v5
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v0, v5) }
         setmap(varmap, v0, -v5)
         LOOP
      }
    }

    UNLESS v4&v3=0 DO
    { UNLESS all&#x00FFFF00 DO    // Test for v3=v4
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v3, v4) }
         setmap(varmap, v3, v4)
         LOOP
      }
      UNLESS all&#xFF0000FF DO    // Test for v3#v4
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v3, v4) }
         setmap(varmap, v3, -v4)
         LOOP
      }
    }

    UNLESS v4&v2=0 DO
    { UNLESS all&#x0F0FF0F0 DO    // Test for v2=v4
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v2, v4) }
         setmap(varmap, v2, v4)
         LOOP
      }
      UNLESS all&#xF0F00F0F DO    // Test for v2#v4
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v2, v4) }
         setmap(varmap, v2, -v4)
         LOOP
      }
    }

    UNLESS v4&v1=0 DO
    { UNLESS all&#x3333CCCC DO    // Test for v1=v4
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v1, v4) }
         setmap(varmap, v1, v4)
         LOOP
      }
      UNLESS all&#xCCCC3333 DO    // Test for v1#v4
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v1, v4) }
         setmap(varmap, v1, -v4)
         LOOP
      }
    }

    UNLESS v4&v0=0 DO
    { UNLESS all&#x5555AAAA DO    // Test for v0=v4
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v0, v4) }
         setmap(varmap, v0, v4)
         LOOP
      }
      UNLESS all&#xAAAA5555 DO    // Test for v0#v4
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v0, v4) }
         setmap(varmap, v0, -v4)
         LOOP
      }
    }

    UNLESS v3&v2=0 DO
    { UNLESS all&#x0FF00FF0 DO    // Test for v2=v3
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v2, v3) }
         setmap(varmap, v2, v3)
         LOOP
      }
      UNLESS all&#xF00FF00F DO    // Test for v2#v3
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v2, v3) }
         setmap(varmap, v2, -v3)
         LOOP
      }
    }

    UNLESS v3&v1=0 DO
    { UNLESS all&#x33CC33CC DO    // Test for v1=v3
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v1, v3) }
         setmap(varmap, v1, v3)
         LOOP
      }
      UNLESS all&#xCC33CC33 DO    // Test for v1#v3
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v1, v3) }
         setmap(varmap, v1, -v3)
         LOOP
      }
    }

    UNLESS v3&v0=0 DO
    { UNLESS all&#x55AA55AA DO    // Test for v0=v3
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v0, v3) }
         setmap(varmap, v0, v3)
         LOOP
      }
      UNLESS all&#xAA55AA55 DO    // Test for v0#v3
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v0, v3) }
         setmap(varmap, v0, -v3)
         LOOP
      }
    }

    UNLESS v2&v1=0 DO
    { UNLESS all&#x3C3C3C3C DO    // Test for v1=v2
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v1, v2) }
         setmap(varmap, v1, v2)
         LOOP
      }
      UNLESS all&#xC3C3C3C3 DO    // Test for v1#v2
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v1, v2) }
         setmap(varmap, v1, -v2)
         LOOP
      }
    }

    UNLESS v2&v0=0 DO
    { UNLESS all&#x5A5A5A5A DO    // Test for v0=v2
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v0, v2) }
         setmap(varmap, v0, v2)
         LOOP
      }
      UNLESS all&#xA5A5A5A5 DO    // Test for v0#v2
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v0, v2) }
         setmap(varmap, v0, -v2)
         LOOP
      }
    }

    UNLESS v1&v0=0 DO
    { UNLESS all&#x66666666 DO    // Test for v0=v1
      { IF debug>=5 DO { prterm p; writef("Action: %d=%d\n", v0, v1) }
         setmap(varmap, v0, v1)
         LOOP
      }
      UNLESS all&#x99999999 DO    // Test for v0#v1
      { IF debug>=5 DO { prterm p; writef("Action: %d#%d\n", v0, v1) }
         setmap(varmap, v0, -v1)
         LOOP
      }
    }

    RETURN
  } REPEAT  // To canonicalize and look for other mappings.


FUN apply_pair_rules : =>

  IF debug>=8 DO writes "\nApplying pair rules\n"

  // Look for pairs of the form: rel  (v7 ..v0 )
  //                             rel' (v7'..v0')
  // with at least v0=v0' and deduce whatever is possible.

  UNLESS varmap DO { varmap := alloc_vm(curts, curts!TsVmax)
                     abort 712
                   }

  LET n = curts!TsTn
  LET k = 0

  LET vupb = 8*n+curts!TsVmax // Up to 8 variables per term + 
                              // up to one marker per variable.
  LET v = getvec vupb
  UNLESS v RAISE (E_Space, "while allocating sort vector\n")

  sortvec := v              // Register the sort vector

  LET p = curts!TsTerm1 
  IF p & 63 DO bug "Alignment error\n" // Check p is suitably aligned.

  FOR i = 1 TO n DO
  { canonterm p
    UNLESS !p<0 DO { IF p!Tv7 DO v!++k := @p!Tv7
                     IF p!Tv6 DO v!++k := @p!Tv6
                     IF p!Tv5 DO v!++k := @p!Tv5
                     IF p!Tv4 DO v!++k := @p!Tv4
                     IF p!Tv3 DO v!++k := @p!Tv3
                     IF p!Tv2 DO v!++k := @p!Tv2
                     IF p!Tv1 DO v!++k := @p!Tv1
                     IF p!Tv0 DO v!++k := @p!Tv0
                   }
    p := @p!Tsize
  }

  UNLESS k DO
  { IF debug>=5 DO writef "There are no terms with non-zero variables\n"
    freevec sortvec
    sortvec := 0
    RETURN
  }

  sort(v, k, cmpval)

  // v!1 ... v!k are pointers to non-zero variables in non-deleted terms
  // The pointers are sorted so that !(v!i) <= !(v!(i+1)), i=1,..,k-1
  // (v!i)&-16 points to the start of the term that v!i refers to.
 
  // Separate in groups for each variable with a zero marker separating
  // each group, and deal with variables that are only used once.

  LET t = vupb
  v!t := 0 
  LET ap=v!k
  LET a=!ap, b=0, c=0
  FOR i = k-1 TO 0 BY -1 DO
  { LET term = ap&-64   // Round down to start of term.
    a, b, c := 0, a, b
    IF i>0 DO { ap := v!i; a := !ap }
    UNLESS a=b OR b=c DO
    { // Variable b is a singleton so get rid of it.
      IF term!Tv7=b DO dcv7 term
      IF term!Tv6=b DO dcv6 term
      IF term!Tv5=b DO dcv5 term
      IF term!Tv4=b DO dcv4 term
      IF term!Tv3=b DO dcv3 term
      IF term!Tv2=b DO dcv2 term
      IF term!Tv1=b DO dcv1 term
      IF term!Tv0=b DO dcv0 term
      change := TRUE
      IF debug>=5 DO { writef("el%4d:\n", b); prterm term }
      LOOP
    }
    UNLESS b=c DO v!--t := 0 // Place separator mark.
    v!--t := term
  }

  IF debug>=12 DO { writef "Terms for pair search\n"
                    FOR i = t TO vupb TEST v!i
                                      THEN prterm(v!i)
                                      ELSE newline()
                  }

  // Match up term pairs, taking those with most variables
  // in common first. All pair rules must leave the terms
  // in canonical form.
  FOR incommon = 8 TO 1 BY -1 FOR i = t TO vupb DO
  { LET p = v!i
    IF p=0 LOOP
    LET j = i
    UNTIL p!Tv7<0 DO { LET q = v!++j
                       IF q=0 BREAK
                       IF q!Tv7<0 LOOP
                       LET k = vars_in_common(p, q)
                       UNLESS k=incommon LOOP
                       LET kp = var_count p
                       LET kq = var_count q
                       IF kp+kq <= 8+k DO { comb_pair(p, q, k, kp, kq)
                                            LOOP
                                          }
                       MATCH k // Else deal with harder pairs
                       : 1 => pair_rule1(p, q)
                       : 2 => pair_rule2(p, q)
                       : 3 => pair_rule3(p, q)
                       : 4 => pair_rule4(p, q)
                       : 5 => pair_rule5(p, q)
                       : 6 => pair_rule6(p, q)
                       : 7 => pair_rule7(p, q)
                       : 8 => pair_rule8(p, q)
                       :   =>
                     }
  }
  
  freevec sortvec
  sortvec := 0

FUN vars_in_common : p, q =>
  LET count = 0
  FOR i = 0 TO 7 DO
  { LET pvi = p!(Tv7+i)
    IF pvi FOR j = 0 TO 7 IF pvi=q!(Tv7+j) DO count++
  }
  RETURN count

FUN var_count : p =>
  LET count = 0
  FOR i = 0 TO 7 IF p!(Tv7+i) DO count++
  RETURN count

FUN comb_pair // The total number of different variables is less than 8
              // so the two terms can be combined into one.
: p[r7, r6, r5, r4, r3, r2, r1, r0, v7, v6, v5, v4, v3, v2, v1, v0],
  q[r7',r6',r5',r4',r3',r2',r1',r0',v7',v6',v5',v4',v3',v2',v1',v0'],
  k, kp, kq =>
    writef("comb_pair %d %d %d\n", k, kp, kq)
    prterm p
    prterm q
    // First put the common variables into the same positions.
    IF k>=1 UNTIL v0=v0' DO                // make v0=v0'
    { IF v1'=v0 DO { sw10 q; BREAK }
      IF v2'=v0 DO { sw20 q; BREAK }
      IF v3'=v0 DO { sw30 q; BREAK }
      IF v4'=v0 DO { sw40 q; BREAK }
      IF v5'=v0 DO { sw50 q; BREAK }
      IF v6'=v0 DO { sw60 q; BREAK }
      IF v7'=v0 DO { sw70 q; BREAK }
      IF v0'=v1 DO { sw10 p; BREAK }
      IF v1'=v1 DO { sw10 p; sw10 q; BREAK }
      IF v2'=v1 DO { sw10 p; sw20 q; BREAK }
      IF v3'=v1 DO { sw10 p; sw30 q; BREAK }
      IF v4'=v1 DO { sw10 p; sw40 q; BREAK }
      IF v5'=v1 DO { sw10 p; sw50 q; BREAK }
      IF v6'=v1 DO { sw10 p; sw60 q; BREAK }
      IF v7'=v1 DO { sw10 p; sw70 q; BREAK }
      IF v0'=v2 DO { sw20 p; BREAK }
      IF v1'=v2 DO { sw20 p; sw10 q; BREAK }
      IF v2'=v2 DO { sw20 p; sw20 q; BREAK }
      IF v3'=v2 DO { sw20 p; sw30 q; BREAK }
      IF v4'=v2 DO { sw20 p; sw40 q; BREAK }
      IF v5'=v2 DO { sw20 p; sw50 q; BREAK }
      IF v6'=v2 DO { sw20 p; sw60 q; BREAK }
      IF v7'=v2 DO { sw20 p; sw70 q; BREAK }
      IF v0'=v3 DO { sw30 p; BREAK }
      IF v1'=v3 DO { sw30 p; sw10 q; BREAK }
      IF v2'=v3 DO { sw30 p; sw20 q; BREAK }
      IF v3'=v3 DO { sw30 p; sw30 q; BREAK }
      IF v4'=v3 DO { sw30 p; sw40 q; BREAK }
      IF v5'=v3 DO { sw30 p; sw50 q; BREAK }
      IF v6'=v3 DO { sw30 p; sw60 q; BREAK }
      IF v7'=v3 DO { sw30 p; sw70 q; BREAK }
      IF v0'=v4 DO { sw40 p; BREAK }
      IF v1'=v4 DO { sw40 p; sw10 q; BREAK }
      IF v2'=v4 DO { sw40 p; sw20 q; BREAK }
      IF v3'=v4 DO { sw40 p; sw30 q; BREAK }
      IF v4'=v4 DO { sw40 p; sw40 q; BREAK }
      IF v5'=v4 DO { sw40 p; sw50 q; BREAK }
      IF v6'=v4 DO { sw40 p; sw60 q; BREAK }
      IF v7'=v4 DO { sw40 p; sw70 q; BREAK }
      IF v0'=v5 DO { sw50 p; BREAK }
      IF v1'=v5 DO { sw50 p; sw10 q; BREAK }
      IF v2'=v5 DO { sw50 p; sw20 q; BREAK }
      IF v3'=v5 DO { sw50 p; sw30 q; BREAK }
      IF v4'=v5 DO { sw50 p; sw40 q; BREAK }
      IF v5'=v5 DO { sw50 p; sw50 q; BREAK }
      IF v6'=v5 DO { sw50 p; sw60 q; BREAK }
      IF v7'=v5 DO { sw50 p; sw70 q; BREAK }
      IF v0'=v6 DO { sw60 p; BREAK }
      IF v1'=v6 DO { sw60 p; sw10 q; BREAK }
      IF v2'=v6 DO { sw60 p; sw20 q; BREAK }
      IF v3'=v6 DO { sw60 p; sw30 q; BREAK }
      IF v4'=v6 DO { sw60 p; sw40 q; BREAK }
      IF v5'=v6 DO { sw60 p; sw50 q; BREAK }
      IF v6'=v6 DO { sw60 p; sw60 q; BREAK }
      IF v7'=v6 DO { sw60 p; sw70 q; BREAK }
      IF v0'=v7 DO { sw70 p; BREAK }
      IF v1'=v7 DO { sw70 p; sw10 q; BREAK }
      IF v2'=v7 DO { sw70 p; sw20 q; BREAK }
      IF v3'=v7 DO { sw70 p; sw30 q; BREAK }
      IF v4'=v7 DO { sw70 p; sw40 q; BREAK }
      IF v5'=v7 DO { sw70 p; sw50 q; BREAK }
      IF v6'=v7 DO { sw70 p; sw60 q; BREAK }
      IF v7'=v7 DO { sw70 p; sw70 q; BREAK }
    }

    IF k>=2 UNTIL v1=v1' DO                // make v1=v1'
    { IF v2'=v1 DO { sw21 q; BREAK }
      IF v3'=v1 DO { sw31 q; BREAK }
      IF v4'=v1 DO { sw41 q; BREAK }
      IF v5'=v1 DO { sw51 q; BREAK }
      IF v6'=v1 DO { sw61 q; BREAK }
      IF v7'=v1 DO { sw71 q; BREAK }
      IF v1'=v2 DO { sw21 p; BREAK }
      IF v2'=v2 DO { sw21 p; sw21 q; BREAK }
      IF v3'=v2 DO { sw21 p; sw31 q; BREAK }
      IF v4'=v2 DO { sw21 p; sw41 q; BREAK }
      IF v5'=v2 DO { sw21 p; sw51 q; BREAK }
      IF v6'=v2 DO { sw21 p; sw61 q; BREAK }
      IF v7'=v2 DO { sw21 p; sw71 q; BREAK }
      IF v1'=v3 DO { sw31 p; BREAK }
      IF v2'=v3 DO { sw31 p; sw21 q; BREAK }
      IF v3'=v3 DO { sw31 p; sw31 q; BREAK }
      IF v4'=v3 DO { sw31 p; sw41 q; BREAK }
      IF v5'=v3 DO { sw31 p; sw51 q; BREAK }
      IF v6'=v3 DO { sw31 p; sw61 q; BREAK }
      IF v7'=v3 DO { sw31 p; sw71 q; BREAK }
      IF v1'=v4 DO { sw41 p; BREAK }
      IF v2'=v4 DO { sw41 p; sw21 q; BREAK }
      IF v3'=v4 DO { sw41 p; sw31 q; BREAK }
      IF v4'=v4 DO { sw41 p; sw41 q; BREAK }
      IF v5'=v4 DO { sw41 p; sw51 q; BREAK }
      IF v6'=v4 DO { sw41 p; sw61 q; BREAK }
      IF v7'=v4 DO { sw41 p; sw71 q; BREAK }
      IF v1'=v5 DO { sw51 p; BREAK }
      IF v2'=v5 DO { sw51 p; sw21 q; BREAK }
      IF v3'=v5 DO { sw51 p; sw31 q; BREAK }
      IF v4'=v5 DO { sw51 p; sw41 q; BREAK }
      IF v5'=v5 DO { sw51 p; sw51 q; BREAK }
      IF v6'=v5 DO { sw51 p; sw61 q; BREAK }
      IF v7'=v5 DO { sw51 p; sw71 q; BREAK }
      IF v1'=v6 DO { sw61 p; BREAK }
      IF v2'=v6 DO { sw61 p; sw21 q; BREAK }
      IF v3'=v6 DO { sw61 p; sw31 q; BREAK }
      IF v4'=v6 DO { sw61 p; sw41 q; BREAK }
      IF v5'=v6 DO { sw61 p; sw51 q; BREAK }
      IF v6'=v6 DO { sw61 p; sw61 q; BREAK }
      IF v7'=v6 DO { sw61 p; sw71 q; BREAK }
      IF v1'=v7 DO { sw71 p; BREAK }
      IF v2'=v7 DO { sw71 p; sw21 q; BREAK }
      IF v3'=v7 DO { sw71 p; sw31 q; BREAK }
      IF v4'=v7 DO { sw71 p; sw41 q; BREAK }
      IF v5'=v7 DO { sw71 p; sw51 q; BREAK }
      IF v6'=v7 DO { sw71 p; sw61 q; BREAK }
      IF v7'=v7 DO { sw71 p; sw71 q; BREAK }
    }

    IF k>=3 UNTIL v2=v2' DO                // make v2=v2'
    { IF v3'=v2 DO { sw32 q; BREAK }
      IF v4'=v2 DO { sw42 q; BREAK }
      IF v5'=v2 DO { sw52 q; BREAK }
      IF v6'=v2 DO { sw62 q; BREAK }
      IF v7'=v2 DO { sw72 q; BREAK }
      IF v2'=v3 DO { sw32 p; BREAK }
      IF v3'=v3 DO { sw32 p; sw32 q; BREAK }
      IF v4'=v3 DO { sw32 p; sw42 q; BREAK }
      IF v5'=v3 DO { sw32 p; sw52 q; BREAK }
      IF v6'=v3 DO { sw32 p; sw62 q; BREAK }
      IF v7'=v3 DO { sw32 p; sw72 q; BREAK }
      IF v2'=v4 DO { sw42 p; BREAK }
      IF v3'=v4 DO { sw42 p; sw32 q; BREAK }
      IF v4'=v4 DO { sw42 p; sw42 q; BREAK }
      IF v5'=v4 DO { sw42 p; sw52 q; BREAK }
      IF v6'=v4 DO { sw42 p; sw62 q; BREAK }
      IF v7'=v4 DO { sw42 p; sw72 q; BREAK }
      IF v2'=v5 DO { sw52 p; BREAK }
      IF v3'=v5 DO { sw52 p; sw32 q; BREAK }
      IF v4'=v5 DO { sw52 p; sw42 q; BREAK }
      IF v5'=v5 DO { sw52 p; sw52 q; BREAK }
      IF v6'=v5 DO { sw52 p; sw62 q; BREAK }
      IF v7'=v5 DO { sw52 p; sw72 q; BREAK }
      IF v2'=v6 DO { sw62 p; BREAK }
      IF v3'=v6 DO { sw62 p; sw32 q; BREAK }
      IF v4'=v6 DO { sw62 p; sw42 q; BREAK }
      IF v5'=v6 DO { sw62 p; sw52 q; BREAK }
      IF v6'=v6 DO { sw62 p; sw62 q; BREAK }
      IF v7'=v6 DO { sw62 p; sw72 q; BREAK }
      IF v2'=v7 DO { sw72 p; BREAK }
      IF v3'=v7 DO { sw72 p; sw32 q; BREAK }
      IF v4'=v7 DO { sw72 p; sw42 q; BREAK }
      IF v5'=v7 DO { sw72 p; sw52 q; BREAK }
      IF v6'=v7 DO { sw72 p; sw62 q; BREAK }
      IF v7'=v7 DO { sw72 p; sw72 q; BREAK }
    }

    IF k>=4 UNTIL v3=v3' DO                // make v3=v3'
    { IF v4'=v3 DO { sw43 q; BREAK }
      IF v5'=v3 DO { sw53 q; BREAK }
      IF v6'=v3 DO { sw63 q; BREAK }
      IF v7'=v3 DO { sw73 q; BREAK }
      IF v3'=v4 DO { sw43 p; BREAK }
      IF v4'=v4 DO { sw43 p; sw43 q; BREAK }
      IF v5'=v4 DO { sw43 p; sw53 q; BREAK }
      IF v6'=v4 DO { sw43 p; sw63 q; BREAK }
      IF v7'=v4 DO { sw43 p; sw73 q; BREAK }
      IF v3'=v5 DO { sw53 p; BREAK }
      IF v4'=v5 DO { sw53 p; sw43 q; BREAK }
      IF v5'=v5 DO { sw53 p; sw53 q; BREAK }
      IF v6'=v5 DO { sw53 p; sw63 q; BREAK }
      IF v7'=v5 DO { sw53 p; sw73 q; BREAK }
      IF v3'=v6 DO { sw63 p; BREAK }
      IF v4'=v6 DO { sw63 p; sw43 q; BREAK }
      IF v5'=v6 DO { sw63 p; sw53 q; BREAK }
      IF v6'=v6 DO { sw63 p; sw63 q; BREAK }
      IF v7'=v6 DO { sw63 p; sw73 q; BREAK }
      IF v3'=v7 DO { sw73 p; BREAK }
      IF v4'=v7 DO { sw73 p; sw43 q; BREAK }
      IF v5'=v7 DO { sw73 p; sw53 q; BREAK }
      IF v6'=v7 DO { sw73 p; sw63 q; BREAK }
      IF v7'=v7 DO { sw73 p; sw73 q; BREAK }
    }

    IF k>=5 UNTIL v4=v4' DO                // make v4=v4'
    { IF v5'=v4 DO { sw54 q; BREAK }
      IF v6'=v4 DO { sw64 q; BREAK }
      IF v7'=v4 DO { sw74 q; BREAK }
      IF v4'=v5 DO { sw54 p; BREAK }
      IF v5'=v5 DO { sw54 p; sw54 q; BREAK }
      IF v6'=v5 DO { sw54 p; sw64 q; BREAK }
      IF v7'=v5 DO { sw54 p; sw74 q; BREAK }
      IF v4'=v6 DO { sw64 p; BREAK }
      IF v5'=v6 DO { sw64 p; sw54 q; BREAK }
      IF v6'=v6 DO { sw64 p; sw64 q; BREAK }
      IF v7'=v6 DO { sw64 p; sw74 q; BREAK }
      IF v4'=v7 DO { sw74 p; BREAK }
      IF v5'=v7 DO { sw74 p; sw54 q; BREAK }
      IF v6'=v7 DO { sw74 p; sw64 q; BREAK }
      IF v7'=v7 DO { sw74 p; sw74 q; BREAK }
    }

    IF k>=6 UNTIL v5=v5' DO                // make v5=v5'
    { IF v6'=v5 DO { sw65 q; BREAK }
      IF v7'=v5 DO { sw75 q; BREAK }
      IF v5'=v6 DO { sw65 p; BREAK }
      IF v6'=v6 DO { sw65 p; sw65 q; BREAK }
      IF v7'=v6 DO { sw65 p; sw75 q; BREAK }
      IF v5'=v7 DO { sw75 p; BREAK }
      IF v6'=v7 DO { sw75 p; sw65 q; BREAK }
      IF v7'=v7 DO { sw75 p; sw75 q; BREAK }
    }

    IF k>=7 UNTIL v6=v6' DO                // make v6=v6'
    { IF v7'=v6 DO { sw76 q; BREAK }
      IF v6'=v7 DO { sw76 p; BREAK }
      IF v7'=v7 DO { sw76 p; sw76 q; BREAK }
    }

    // Now generalise both terms with don't care bits
    IF kp<=0 DO r0 *:= #x00000003
    IF kp<=1 DO r0 *:= #x00000005
    IF kp<=2 DO r0 *:= #x00000011
    IF kp<=3 DO r0 *:= #x00000101
    IF kp<=4 DO r0 *:= #x00010001
    IF kp<=5 DO r1 := r0
    IF kp<=6 DO r3, r2 := r1, r0
    IF kp<=7 DO r7, r6, r5, r4 := r3, r2, r1, r0
    	
    IF kq<=0 DO r0' *:= #x00000003
    IF kq<=1 DO r0' *:= #x00000005
    IF kq<=2 DO r0' *:= #x00000011
    IF kq<=3 DO r0' *:= #x00000101
    IF kq<=4 DO r0' *:= #x00010001
    IF kq<=5 DO r1' := r0'
    IF kq<=6 DO r3', r2' := r1', r0'
    IF kq<=7 DO r7', r6', r5', r4' := r3', r2', r1', r0'

    // Now move non shared vars in p to left end
    IF kp-k>=1 WHILE v7=0 DO // Move a variable to v7
    { IF v6 DO { sw76 p; BREAK }
      IF v5 DO { sw75 p; BREAK }
      IF v4 DO { sw74 p; BREAK }
      IF v3 DO { sw73 p; BREAK }
      IF v2 DO { sw72 p; BREAK }
      IF v1 DO { sw71 p; BREAK }
    }
    	
    IF kp-k>=2 WHILE v6=0 DO // Move a variable to v6
    { IF v5 DO { sw65 p; BREAK }
      IF v4 DO { sw64 p; BREAK }
      IF v3 DO { sw63 p; BREAK }
      IF v2 DO { sw62 p; BREAK }
      IF v1 DO { sw61 p; BREAK }
    }
    	
    IF kp-k>=3 WHILE v5=0 DO // Move a variable to v5
    { IF v4 DO { sw54 p; BREAK }
      IF v3 DO { sw53 p; BREAK }
      IF v2 DO { sw52 p; BREAK }
      IF v1 DO { sw51 p; BREAK }
    }
    	
    IF kp-k>=4 WHILE v4=0 DO // Move a variable to v4
    { IF v3 DO { sw43 p; BREAK }
      IF v2 DO { sw42 p; BREAK }
      IF v1 DO { sw41 p; BREAK }
    }
    IF kp-k>=5 WHILE v3=0 DO // Move a variable to v3
    { IF v2 DO { sw32 p; BREAK }
      IF v1 DO { sw31 p; BREAK }
    }
    IF kp-k>=6 WHILE v2=0 DO // Move a variable to v2
    { IF v1 DO { sw21 p; BREAK }
    }
    	
prterm p;prterm q
abort 2345
    IF kp+kq <= k+8 DO // No more than 8 variables in the combined term.
    { FOR i = 8-kq TO 7-k DO p!(Tv7+i) := q!(Tv7+i) // Copy some vars
      FOR i = 0 TO 7 DO p!(Tr7+i) &:= q!(Tr7+i)
      q!Tv7 := -1
      IF debug>=6 DO { writef "====>\n"; prterm p }
prterm p;prterm q
abort 2346
      canonterm p
      canonterm q
prterm p;prterm q
abort 2347
      change := TRUE
      RETURN
    }

    abort 3458

FUN pair_rule1 : p, q => writef "Pair1\n"; prterm p; prterm q
FUN pair_rule2 : p, q => writef "Pair2\n"; prterm p; prterm q
FUN pair_rule3 : p, q => writef "Pair3\n"; prterm p; prterm q
FUN pair_rule4 : p, q => writef "Pair4\n"; prterm p; prterm q
FUN pair_rule5 : p, q => writef "Pair5\n"; prterm p; prterm q
FUN pair_rule6 : p, q => writef "Pair6\n"; prterm p; prterm q
FUN pair_rule7 : p, q => writef "Pair7\n"; prterm p; prterm q
FUN pair_rule8 : p, q => writef "Pair8\n"; prterm p; prterm q

/*
FUN pair_rule2 : p[rel,  x,  y,  z ],
                 q[rel', x', y', z'] =>

    IF rel<=0 OR rel'<=0  RETURN

    IF  x=x'  OR   y~=y'  OR  z~=z' DO bug "Bug found in pair_rule2\n"


    IF debug>=12 DO { writef("pair2: "); prterm p
                      writef(" with: "); prterm q
                      newline()
                    }

    IF y=0 DO
    { IF debug>=5 DO { writef("comb:  "); prterm p
                       writef("with:  "); prterm q
                     }
      y := x'                                        // [rel,   x,0,z]
      rel := (rel * #b101) & (swapxy%rel' * #b10001) // [rel',  y,0,z]
      rel', change := -1, TRUE                       // ==>
      IF debug>=5 DO { writef("   ==> "); prterm p   // [rel'', x,y,z]
                       writef("  and: "); prterm q   // [void,  y,0,z]
                     }
      RETURN
    }

    LET a = rel  >> 4
    LET b = rel  &  #b1111
    LET c = rel' >> 4
    LET d = rel' &  #b1111
    a, b, c, d := a&c, b&c, a&d, b&d
    LET nrel  = (a|c)<<4 | (b|d)
    LET nrel' = (a|b)<<4 | (c|d)

    IF debug>=8 DO writef("crel: %4b %4b %4b %4b\n", a, b, c, d)

    // All the information in the terms: [rel,  x,  y, z]
    //                                   [rel', x', y, z]
    // is now contained in:           [crel, x, x', y, z]

    // Let's see what this tells us...

    UNLESS rel=nrel AND rel'=nrel' DO
    { IF debug>=5 DO { writef("simp:  "); prterm p
                       writef("with:  "); prterm q
                     }
      IF rel~=nrel DO   { rel := nrel
                          IF debug>=5 DO { writef "=====> "; prterm p }
                        }

      IF rel'~=nrel' DO { rel' := nrel'
                          IF debug>=5 DO { writef "=====> "; prterm q }
                        }
      UNLESS rel DO
      { IF debug>=5 DO { writef("unsat: "); prterm p }
        RAISE E_FalseTermFound
      }
      UNLESS rel' DO
      { IF debug>=5 DO { writef("unsat: "); prterm q }
        RAISE E_FalseTermFound
      }
    }

    // a|b|c|d non zero

    UNLESS b|c DO // Must x=x' ?
    { IF debug>=5 DO { writef("pgen=: "); prterm p
                       writef(" with: "); prterm q
                       writef("    => Axpx'   ie %d=%d\n", x, x')
                     }
      IF x<3 AND x'<3 DO bug "both<3 in pair3\n"
      setmap(varmap, x, x')
      mapterm p  // Must not permute the arguments
      mapterm q  // or raise an exception.
      rel, rel' := -1, rel&rel'  // Three arguments are now the same
      change := TRUE
      IF debug>=5 DO { writef("gives: "); prterm p
                       writef("  and: "); prterm q
                     }
    }

    UNLESS a|d DO // Must x=~x' ?
    { IF debug>=5 DO { writef("pgen#: "); prterm p
                       writef(" with: "); prterm q
                       writef("    => Axnx'   ie %d#%d\n", x, x')
                     }
      IF x<3 AND x'<3 DO bug "both<3 in pair3\n"
      setmap(varmap, x, x'=0->1,-x')
      mapterm p  // Must not permute the arguments
      mapterm q  // or raise an exception.
      rel, rel' := -1, rel&rel'  // Three arguments are now the same
      change := TRUE
      IF debug>=5 DO { writef("gives: "); prterm p
                       writef("  and: "); prterm q
                     }
    }

    apply_unit_rule p
    apply_unit_rule q
*/

/*
FUN pair_rule3 : p[rel,  x,  y,  z ],
                 q[rel', x', y', z'] =>

    IF rel<=0 OR rel'<=0 DO bug "Bug1 found in pair_rule3\n"


    UNLESS x=x' AND y=y' AND z=z' DO
    { prterm p
      prterm q
      bug "Bug found in pair_rule3\n"
    }

    LET nrel = rel&rel'

    IF debug>=5 AND rel' ~= nrel DO
    { writef("pair3: "); prterm p
      writef(" with: "); prterm q
    }

    rel, rel' := -1, nrel
    change := TRUE

    IF debug>=5 AND rel= -1 DO
    { writef("gives: "); prterm p
      writef("  and: "); prterm q
    }

    apply_unit_rule q
*/

//********************* Sort Function ******************

STATIC cmpfn  // cmpfn(p, q)=TRUE <=> term p < term q

FUN cmpval : [<z], [z] => TRUE
           :           => FALSE

FUN cmpfull
: [r7, r6, r5, r4, r3, r2, r1, r0, v7, v6, v5, v4, v3, v2, v1, v0 ] ,
  [r7',r6',r5',r4',r3',r2',r1',r0',v7',v6',v5',v4',v3',v2',v1',v0'] =>

  IF v0<v0' RETURN TRUE
  IF v0>v0' RETURN FALSE
  IF v1<v1' RETURN TRUE
  IF v1>v1' RETURN FALSE
  IF v2<v2' RETURN TRUE
  IF v2>v2' RETURN FALSE
  IF v3<v3' RETURN TRUE
  IF v3>v3' RETURN FALSE
  IF v4<v4' RETURN TRUE
  IF v4>v4' RETURN FALSE
  IF v5<v5' RETURN TRUE
  IF v5>v5' RETURN FALSE
  IF v6<v6' RETURN TRUE
  IF v6>v6' RETURN FALSE
  IF v7<v7' RETURN TRUE
  IF v7>v7' RETURN FALSE

  IF r0<r0' RETURN TRUE
  IF r0>r0' RETURN FALSE
  IF r1<r1' RETURN TRUE
  IF r1>r1' RETURN FALSE
  IF r2<r2' RETURN TRUE
  IF r2>r2' RETURN FALSE
  IF r3<r3' RETURN TRUE
  IF r3>r3' RETURN FALSE
  IF r4<r4' RETURN TRUE
  IF r4>r4' RETURN FALSE
  IF r5<r5' RETURN TRUE
  IF r5>r5' RETURN FALSE
  IF r6<r6' RETURN TRUE
  IF r6>r6' RETURN FALSE
  IF r7<r7' RETURN TRUE
  IF r7>r7' RETURN FALSE

  RETURN FALSE         // This is important -- sort may loop otherwise.

FUN sort : v, n, f => cmpfn := f
                      qsort(@v!1, @v!n)

FUN qsort : l, r =>
  WHILE @l!8<r DO
  { LET midpt = ((l+r)/2) & -Bpw
    //writef("qsort: %5d  %5d\n", (l-sortvec)/Bpw, (r-sortvec)/Bpw)
    // Select a good(ish) median value.
    LET val   = middle(!l, !midpt, !r)
    LET p = partition(val, l, r)
    // Only use recursion on the smaller partition.
    TEST p>midpt THEN { qsort(p, r);     r := @p!-1 }
                 ELSE { qsort(l, @p!-1); l := p     }
   }
   //writef("isort: %5d  %5d\n", (l-sortvec)/Bpw, (r-sortvec)/Bpw)
   FOR p = @l!1 TO r BY Bpw DO  // Now perform insertion sort.
     FOR q = @p!-1 TO l BY -Bpw DO
         TEST cmpfn(q!0,q!1) THEN BREAK
                             ELSE q!0, q!1 := q!1, q!0


FUN middle : a, b, c => cmpfn(a,b) -> cmpfn(b,c) -> b,
                                      cmpfn(a,c) -> c,
                                                    a,
                        cmpfn(b,c) -> cmpfn(a,c) -> a,
                                                    c,
                        b

FUN partition : median, p, q =>
{  WHILE cmpfn(!p, median) DO p+++
   WHILE cmpfn(median, !q) DO q---
   IF p>=q RETURN p
   !p, !q := !q, !p
   p+++
   q---
} REPEAT


FUN start : => writes "This program should be called from chk\n"
               RETURN 20



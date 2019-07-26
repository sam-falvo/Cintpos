GET "mcpl.h"

/* This is a program to show that there are 120549 elements in
   the domain D3 as described on pages 113-115 of "Denotational
   Semantics" by J.E.Stoy.

   The domain D2 has 10 elements (A-J) arranged in a lattice as 
   follows:

             J
             |
             I
            / \
           H   G
            \ / \
             F   E
            / \ /
           C   D
            \ /
             B
             |
             A

   This program computes the number of monotonic functions in

                          D3 = D2->D2
*/

MANIFEST  A=1,     B=1<<1, C=1<<2, D=1<<3, E=1<<4,
          F=1<<5,  G=1<<6, H=1<<7, I=1<<8, J=1<<9

STATIC    tab = VEC J,
          count

FUN start : =>

   tab!J := A+B+C+D+E+F+G+H+I+J
   tab!I := A+B+C+D+E+F+G+H+I
   tab!H := A+B+C+D  +F  +H
   tab!G := A+B+C+D+E+F+G
   tab!F := A+B+C+D  +F
   tab!E := A+B  +D+E
   tab!D := A+B  +D
   tab!C := A+B+C
   tab!B := A+B
   tab!A := A

// tab!e = the set of elements <= e in the lattice D2

   count := 0
   try(p10, A+B+C+D+E+F+G+H+I+J)
   writef("Number of elements in D3 = %d\n", count)
   RETURN 0
.
FUN try : p, a, b => UNTIL a=0 DO { LET x = a & -a
                                    a -:= x
                                    p(tab!x, b)
                                  }
.
                                   //        J
FUN p10 : a     => try(p9, a)      //        |a
.                                  //        I
FUN p9  : a     => try(p8, a, a)   //        |a
.                                  //       / \
                                   //      H   |
FUN p8  : a, b  => try(p7, b, a)   //     a|   |b
.                                  //      |   G
FUN p7  : a, b  => try(p6, a&b, a) //     b|   |a
.                                  //       \ / \
                                   //        F   |
FUN p6  : a, b  => try(p5, b, a)   //       a|   |b
.                                  //        |   E
FUN p5  : a, b  => try(p4, a&b, b) //       b|   |a
.                                  //       / \ /
                                   //      |   D
FUN p4  : a, b  => try(p3, b, a)   //     b|   |a
.                                  //      C   |
FUN p3  : a, b  => try(p2, a&b)    //     a|   |b
.                                  //       \ /
                                   //        B
FUN p2  : a     => try(p1, a)      //        |a
.                                  //        A
FUN p1  :       => count++         //        |
.


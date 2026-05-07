GET "mcpl.h"

// Main program ********************************************

FUN start : =>
  writef "Turing entered\n\n"

  { { mk_init 200000
      turing(s0, "A11", 'B', "111A")
    } HANDLE : E_mk, mess => writef("Error: %s\n",  mess)
  }

  mk_close()


// Space Allocation ****************************************

STATIC spacev=0, spacep=0

MANIFEST E_mk

FUN mk_init : upb =>
  spacev := getvec upb
  IF spacev=0 RAISE (E_mk, "Unable to allocate workspace")
  spacep := @ spacev!upb

FUN mk_close : => freevec spacev

FUN mk2 : x, y => LET p = @ spacep!-2
                  IF p<spacev RAISE (E_mk, "Out of space")
                  p!0, p!1, spacep := x, y, p
                  RETURN p


// Turing Machine Implementation ***************************

STATIC ltape, ch, rtape

FUN turing : init_state, lstr, char, rstr =>
  ltape, ch, rtape := 0, char, 0

  LET i = 0
  WHILE rstr%i DO i++
  WHILE i      DO rtape := mk2(rtape, rstr%--i)
  WHILE lstr%i DO ltape := mk2(ltape, lstr%i++)

  pr()
  init_state ch

FUN right : c => 
  IF rtape=0 DO rtape := mk2(0, ' ')

  MATCH rtape
  : [link, k] =>  link, ltape, rtape, ch, k :=
                 ltape, rtape,  link,  k, c
                 pr()
                 RETURN ch

FUN left : c => 
  IF ltape=0 DO ltape := mk2(0, ' ')

  MATCH ltape
  : [link, k] =>  link, rtape, ltape, ch, k :=
                 rtape, ltape,  link,  k, c
                 pr()
                 RETURN ch

FUN halt : c => ch := c; pr()


// Output Routines *****************************************

FUN pr : =>
  prb ltape               // Print the tape on the left
  writef("[%c]", ch)      // Print the current character
  prf rtape               // Print the tape on the right
  newline()

FUN prb                   // Print list of chars backwards
:         0 => RETURN
: [chs, ch] => prb chs; wrch ' '; wrch ch

FUN prf                   // Print list of chars forwards
:         0 => RETURN
: [chs, ch] => wrch ch; wrch ' '; prf chs



// Turing Machine Definition *******************************


FUN s0 : '1' => s1 (right '0')
       : 'A' => s2 (right ' ')
       :  c  => s0 (left   c )

FUN s1 : 'A' => s3 (left  'A')
       : 'X' => s1 (right '1')
       :  c  => s1 (right  c )

FUN s2 : 'A' => s5 (right 'A')
       :  ?  => s2 (right ' ') 

FUN s3 : 'B' => s0 (left  'B')
       : '1' => s4 (right 'X')
       :  c  => s3 (left   c )

FUN s4 : ' ' => s3 (left  'X')
       :  c  => s4 (right  c )

FUN s5 : ' ' =>      halt 'A'
       : 'X' => s5 (right '1')
       :  c  => s5 (right  c )


/*
   The above Turing Machine will multiply two numbers 
given in unary.  It is exercised by the call:

   turing(s0, "A11", 'B', "111A")

which will trace the execution of the machine as follows:
 
 A 1 1[B]1 1 1 A 
 A 1[1]B 1 1 1 A 
 A 1 0[B]1 1 1 A 
 A 1 0 B[1]1 1 A 
 A 1 0 B 1[1]1 A 
 A 1 0 B 1 1[1]A 
 A 1 0 B 1 1 1[A]
 A 1 0 B 1 1[1]A 
 A 1 0 B 1 1 X[A]
 A 1 0 B 1 1 X A[ ]
 A 1 0 B 1 1 X[A]X 
 A 1 0 B 1 1[X]A X 

... lots of lines until ...

 A 0 0[B]X X X A X X X X X X 
 A 0[0]B X X X A X X X X X X 
 A[0]0 B X X X A X X X X X X 
[A]0 0 B X X X A X X X X X X 
  [0]0 B X X X A X X X X X X 
    [0]B X X X A X X X X X X 
      [B]X X X A X X X X X X 
        [X]X X A X X X X X X 
          [X]X A X X X X X X 
            [X]A X X X X X X 
              [A]X X X X X X 
               A[X]X X X X X 
               A 1[X]X X X X 
               A 1 1[X]X X X 
               A 1 1 1[X]X X 
               A 1 1 1 1[X]X 
               A 1 1 1 1 1[X]
               A 1 1 1 1 1 1[ ]
               A 1 1 1 1 1 1[A]

i.e. 2 times 3 equals 6    
*/

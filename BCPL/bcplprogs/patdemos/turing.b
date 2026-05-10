GET "libhdr"

GLOBAL {
  spacev:ug; spacep
  ltape; ch; rtape
}

// Main program ********************************************

LET start : => VALOF
{  writef("Turing entered*n*n")

  mk_init(200000)
  turing(s0, "A11", 'B', "111A")

  mk_close()
}


// Space Allocation ****************************************

AND mk_init : upb BE
{ spacev := getvec(upb)
  UNLESS spacev DO
  { writef("Unable to allocate workspace*n")
    abort(999)
  }
  spacep := @ spacev!upb
}

AND mk_close : BE freevec(spacev)

AND mk2 : x, y => VALOF
{ LET p = spacep-2
  IF p<spacev DO
  { writef("Out of space*n")
    abort(999)
  }
  p!0, p!1, spacep := x, y, p
  //writef("mk2: Allocating %i5 -> [%n %n]*n", p, x, y)
//abort(1234)
  RESULTIS p
}

// Turing Machine Implementation ***************************

//  turing(s0, "A11", 'B', "111A")

AND turing : init_state, lstr, char, rstr BE
{ ltape, ch, rtape := 0, char, 0

  FOR i = rstr%0 TO 1 BY -1 DO rtape := mk2(rtape, rstr%i)
  FOR i = 1 TO lstr%0 BY +1 DO ltape := mk2(ltape, lstr%i)

  pr()
  init_state(ch)
}

AND right : c => VALOF
{ UNLESS rtape DO rtape := mk2(0, ' ')

  MATCH(rtape)
  : [link, k] BE { LET a1, a2, a3, a4, a5 = ltape, rtape, link, k, c
                   link, ltape, rtape, ch, k := a1, a2, a3, a4, a5
                   pr()
		 }
  RESULTIS ch
}

AND left : c => VALOF
{ UNLESS ltape DO ltape := mk2(0, ' ')

  MATCH(ltape)
  : [link, k] BE { LET a1, a2, a3, a4, a5 = rtape, ltape, link, k, c
                   link, rtape, ltape, ch, k := a1, a2, a3, a4, a5
                   pr()
		 }
  RESULTIS ch
}

AND halt : c => VALOF { ch := c; pr(); RESULTIS '*n' }


// Output Routines *****************************************

AND pr : BE
{ prb(ltape)              // Print the tape on the left
  writef("[%c]", ch)      // Print the current character
  prf(rtape)              // Print the tape on the right
  newline()
}

AND prb                   // Print list of chars backwards
:         0 BE RETURN
: [chs, ch] BE { prb(chs); wrch(' '); wrch(ch) }

AND prf                   // Print list of chars forwards
:         0 BE RETURN
: [chs, ch] BE { wrch(ch); wrch(' '); prf(chs) }



// Turing Machine Definition *******************************


AND s0 : '1' => s1 (right('0'))
       : 'A' => s2 (right(' '))
       :  c  => s0 (left ( c ))

AND s1 : 'A' => s3 (left ('A'))
       : 'X' => s1 (right('1'))
       :  c  => s1 (right( c ))

AND s2 : 'A' => s5 (right('A'))
       :  ?  => s2 (right(' ')) 

AND s3 : 'B' => s0 (left ('B'))
       : '1' => s4 (right('X'))
       :  c  => s3 (left ( c ))

AND s4 : ' ' => s3 (left ('X'))
       :  c  => s4 (right( c ))

AND s5 : ' ' =>      halt('A')
       : 'X' => s5 (right('1'))
       :  c  => s5 (right( c ))


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

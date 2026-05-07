GET "mcpl.h"

MANIFEST
  X, P, H,                            // For boarder, Peg and Hole.
  Centre=4*9+4, Last=9*9-1,
  North=-9, South=9, East=1, West=-1, // Directions
  Found=100                           // An exception

STATIC
  board,
  movev = VEC 31,
  dir   = [ East, South, West, North ]

FUN start : => 

  board := [ X,  X,  X,  X,  X,  X,  X,  X,  X,
             X,  X,  X,  P,  P,  P,  X,  X,  X,
             X,  X,  X,  P,  P,  P,  X,  X,  X,
             X,  P,  P,  P,  P,  P,  P,  P,  X,
             X,  P,  P,  P,  H,  P,  P,  P,  X,
             X,  P,  P,  P,  P,  P,  P,  P,  X,
             X,  X,  X,  P,  P,  P,  X,  X,  X,
             X,  X,  X,  P,  P,  P,  X,  X,  X,
             X,  X,  X,  X,  X,  X,  X,  X,  X
           ]

  try 31                                // There 31 pegs to remove
  HANDLE : Found => FOR i = 31 TO 1 BY -1 DO
                    { LET m = movev!i
                      writef("Move peg from %2d over %2d to %2d\n",
                              (m>>16)&255, (m>>8)&255, m&255)
                    }
                    RETURN 0
         .
  writef "Not found\n"
  RETURN 0

FUN try 
: 0 => IF board!Centre= P RAISE Found

: m  => FOR p = 0 TO Last IF board!p= P DO  // Find a peg
          FOR k = 0 TO 3 DO
            { LET d = dir!k  // Try a direction
              LET p1 = p  + d
              LET p2 = p1 + d
              IF board!p1= P AND board!p2= H DO // Is move possible?
              { movev!m := pack(p, p1, p2)  // It is, so try making it
                board!p, board!p1, board!p2 :=  H,  H,  P
                try(m-1)                    // Explore new position
                board!p, board!p1, board!p2 :=  P,  P,  H
              }
            }
  
FUN pack : a, b, c => a<<16 | b<<8 | c

FUN print_peg : X => writef "  "
              : H => writef " 0"
              : P => writef " *"

FUN print_board : =>
    FOR p = 0 TO Last DO
    { UNLESS p MOD 9 DO newline()
      print_peg (board!p)
    }
    newline()

/* print_board() will print the board e.g. as follows:

       0 0 0      
       0 0 0      
   0 0 0 0 0 0 0  
   0 0 0 * 0 0 0  
   0 0 0 0 0 0 0  
       0 0 0      
       0 0 0      
*/




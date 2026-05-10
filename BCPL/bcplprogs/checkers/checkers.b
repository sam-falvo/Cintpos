/*
                A Program to play checkers

Usage: TO/K,-t/S,-d/S

TO/S  sends output to a specified file
-t/S  Turn on tracing
-d/S  Turn on tracing and call abort at key places

This is a BCPL translation of a program to play checkers written by
Christopher Strachey and published in his paper "System Analysis and
Programming" in Scientific American, Vol 215, N0 3, Sept 1966.

His program was the following with minor edits to allow it to be
written using the ASCII character set, without his section brackets
and greek letters. These have been replaced by {, }, phi, psi and
theta. Where justaposition was used for multiplication, * has been
inserted. His program seems to contain some bugs which have been
corrected in the BCPL version.

ChoosePosition(P) = value of
{ let L = LeagalPositionFrom(P)
  if Null(L) then Resign
  let (p,v) = BestPosition(NIL, -INF, L, 0)
  result is p
}

BestPosition(P,V,L,d) = Null(L) -> (P,V), value of
{ let (p,l) = Next(L)
  let v = -PositionValue(p,d+1)
  result is (v>V) -> BestPosition(p,v,l,d),
                     BestPosition(P,V,l,d)
}

PositionValue(P,d) = Terminal(P,d) -> TerminalValue(P), value of
{ let L = LegalPositionsFrom(P)
  let (p,v) = BestPosition(NIL, -INF, L, d)
  result is v
}

LegalPositionsFrom(P) = value of
{ Let L = RemainingPositionList(P, Capture, 5)
  result is Null(L) -> RemainingPositionList(P, NonCapture, 5), L
}

RemainingPositionList(P, C, s) =
  PartialPositionList(P, C, s, FindMoveList(P, C, s))

PartialPositionList(P, C, s, L) =
  Null(L) -> ((s=-5) -> NIL,
               RemainingPositionList(P, C, NextMoveDirection(s)),
  value of
  { let phi = SingleDigitFrom(L)
    let ip = MakeMove(P, C, s, phi)
    let l = (C=Capture) -> CaptureTree(ip),
                           FinalPosition(ip)
    result is Join(l, PartialPositionList(P, C, s, L-phi))
  }

NextMoveDirection(s) = (s=5) ->  4,
                       (s=4) -> -4,
		                -5

FindMoveList(P, C, s) = value of
{ let (X, Y, K, sigma) = P
  let Empty = ~X & ~Y & Board
  let psi = (C=Capture) -> (Shift(Empty, sigma*s) & Y, Empty
  let phi = Shift(psi, sigma*s) &  X
  result is (s>0) -> phi, phi & K
}

MakeMove(P, C, s, phi) = value of
{ let (X, Y, K, sigma) = P
  let psi   = (C=Capture) -> Shift(phi, -sigma*s), NIL
  let theta = (C=Capture) -> Shift(psi, -sigma*s),
                             Shift(phi, -sigma*s)
  let Xk = Null(phi & K) -> (theta & LastRows), theta-phi
  result is ((X-phi+theta), (Y-psi), (K-psi & K + Xk), sigma, theta)
}

FinalPosition(ip) = value of
{ let (X, Y, K, sigma) = ip
  result is (Y, X, K, -sigma)
}

CaptureTree(ip) = value of
{ let L = PartialCapturePositionList(ip)
  result is Null(L) -> (FinalPosition(ip)),
                       CombineCaptureTrees(L)
}

PartialCapturePositionList(ip) = value of
{ let X, Y, K, sigma, phi) =ip
  let P = (X, Y, K, sigma)
  result is MinList(PCP(P, phi, 5),
                    PCP(P, phi, 4),
                    PCP(P, phi & K, -4),
                    PCP(P, phi & K, -5))
}

PCP(P, phi, s) = value of
{ let (X, Y, K, sigma) = P
  let psi = Shift(phi, -sigma*s) & Y
  let Empty = ~X & ~Y & Board
  let theta = Shift(psi, -sigma*s) & Empty
  let Xk = Null(phi & K) -> (theta & LastRows), (theta-phi)
  result is Null(theta) ->
              NIL,
              ((X-phi+theta), (Y-psi), K, -4, (K-psi & K+Xk), sigma, theta)
}

CombineCaptureTrees(L) = Null(L) -> NIL, value of
{ let (lp,l) = Next(L)
  result is join(CaptureTree(lp), CombineCaptureTrees(l))
}

Comments about this program.

It is written in what he describes as an informal and somewhat
extended version of CPL. In it his greek letter names have been
replace by phi, psi and theta.

Primitive list functions

Null(L) is true if list L is empty and false otherwise.
Head(L) is the first member of list L.
Tail(L) is what remains after the first member of list L is removed.
Next(L) is a list whose members are Head(L) and Tail(L).
Join(L1,L2) is a single list formed by the members of Lists L1 and L2.
MinList(L1,L2,...) is a single list formed from the members of L1, l2,...,
                   leaving out Null members and repetitions.

Occasionally L is a bit string representing a set of piece positions.
In the BCPL version these occurrences have been replaced by the
variable set.

Primitive bit string functions

~  NOT
&  AND
|  OR
x+y  same as x | y
x-y  same as x & ~y
SingleDigitFrom(x) is a bit string of the same length as x with a
                   single 1 in a position corresponding to the 1's
		   in bit string x.
Shift(x,n) is the bit string x shifted n places to the right. If n<0
           the shift is to the left. Digit shifted off the board are
	   lost. Digits shifted onto the board are 0's.
           Bit strings in Strachey's paper are numbered from left to
           right, so a right shift of a bit increases its bit number,
           and thus corresponds to a square closer to the white side
           of the board.

Primitive strategy functions

Terminal(P,d)    is true if position P is terminal.
TerminalValue(P) is the value of P (when look-ahead beyond P is undesirable).

Data structures

45-bit strings

X         is the player's men and kings.
Y         is the opponent's men and kings.
K         holds the kings on both sides.
phi       square moved from.
psi       captured piece (if any).
theta     the square moved to.
Board     1's on board squares, 0's elsewhere.
LastRows  1's is squares 5, 6, 7, 8, 36, 37, 38 and 39.

Positions
 
sigma  Next play, =1 if black to play, =-1 if white to play.
P,p    Ordinary positions with components X, Y, K, sigma.
ip     Intermediate positions with components X, Y, K, sigma, phi
       where phi indicates the piece being moved. This is used
       when exploring a capture tree.

Miscellaneous

C  is Capture or NonCapture
s  is the directions of move as view by the current side,
   = 5 forward left
   = 4 forward left
   =-4 backward left
   =-5 backward right
   The actual direction on the board is sigma*s
V,v are position values
d   is the depth of look-ahead

The bit positions of the board are as follows:

              Black
      *  5  *  6  *  7  *  8
      9  * 10  * 11  * 12  *
      * 14  * 15  * 16  * 17
     18  * 19  * 20  * 21  *
      * 23  * 24  * 25  * 26
     27  * 28  * 29  * 30  *
      * 32  * 33  * 34  * 35
     36  * 37  * 38  * 39  *
              White

Black has the first move.

The bits are numbered from 0 to 44 from left to right.
In the BCPL version they are numbered from right to left.

Board    = 0000_0_1111_1111_0_1111_1111_0_1111_1111_0_1111_1111_0_0000
LastRows = 0000_0_1111_0000_0_0000_0000_0_0000_0000_0_0000_1111_0_0000

Shift(psi, s) & Board  is a bit string representing the destination
                       position of a piece at position psi after
		       moving diagonally in direction s. The result
		       is zero if the move would place the piece
		       off the board. If s is positive the piece
                       moves to a higher numbered square.

The program obeys the rule that a capture move must be made if one is
possible and repeated until a the piece can no longer capture. As with
modern rules for checkers huffing is not allowed.

This program has been translated into the related language BCPL and
extended to include interactive i/o. This program constains minor
modifications to Strachey's version, including corrections to
what seem to be bugs in the original. The definitions of the strategy
functions Terminal(P,d) and TerminalValues(P) are rather simple.

This translation into BCPL is by Martin Richards (c) October 2019
*/

GET "libhdr.h"

GLOBAL {
  spacev: ug  // Space for node lists
  spacep
  spacet
  freelist
  
  Board       // Bit patterns of length 45
  LastRows
  black
  white
  kings

  sigma

  // Functions
  ChoosePosition
  BestPosition
  PositionValue
  LegalPositionsFrom
  RemainingPositionList
  PartialPositionList
  NextMoveDirection
  FindMoveSet
  MakeMove

  FinalPosition
  CaptureTree
  PartialCapturePositionList
  PCP
  CombineCaptureTrees
  Terminal
  TerminalValue

  Join
  MinList
  SingleDigitFrom
  Shift
  prboard
  prlist
  mknode
  freenode
  freenodelist
  listlength
  bits     // Function to count the number of 1s in a word
  contains

  stdin
  stdout
  tofilename
  tostream
  tracing
  debugging
  computeriswhite
  done
}

MANIFEST {
  h1=0; h2; h3; h4; h5; h6
  NIL=0        // An empty list
  INF=1000000
  Capture    = TRUE
  NonCapture = FALSE
}

LET start() = VALOF
{ LET argv = VEC 50
  
  UNLESS ON64 DO
  { writef("ERROR: This program can only be run using 64 bit BCPL*n")
    RESULTIS 0
  }

  stdin  := input()
  stdout := output()
  UNLESS rdargs("TO/K,-w/S,-b/S,-t/S,-d/S", argv, 50) DO
  { writef("Bad arguments for checkers*n")
    RESULTIS 0
  }

  tofilename, tostream := 0, 0
  tracing    := FALSE
  debugging  := FALSE
  computeriswhite := TRUE
  done := FALSE
  spacev := 0
  
  IF argv!0 DO tofilename := argv!0 // TO/K
  computeriswhite :=  argv!1        // -w/S
  computeriswhite := ~argv!2        // -b/S
  tracing   :=   argv!3             // -t/S
  debugging :=   argv!4             // -d/S
  tracing := debugging
  
  IF tofilename DO
  { tostream := findoutput(tofilename)
    UNLESS tostream DO
    { writef("Unable to open file %s for output*n", tofilename)
      GOTO fin
    }
    selectoutput(tostream)
  }

  spacev := getvec(100000)
  UNLESS spacev DO
  { writef("More space needed*n")
    abort(999)
    GOTO fin
  }
  spacet := spacev+100000
  spacep := spacet
  freelist := 0

  Board    := #b_1111_1111_0_1111_1111_0_1111_1111_0_1111_1111_0_0000
  LastRows := #b_1111_0000_0_0000_0000_0_0000_0000_0_0000_1111_0_0000

  // Starting positions, black has smaller square numbers than white
  black    := #b_0000_0000_0_0000_0000_0_0000_1111_0_1111_1111_0_0000
  white    := #b_1111_1111_0_1111_0000_0_0000_0000_0_0000_0000_0_0000
  kings    := #b_0000_0000_0_0000_0000_0_0000_0000_0_0000_0000_0_0000

  // A test positions used for debugging.
  //black    := #b_0000_0000_0_1000_0000_0_1011_0001_0_0000_0000_0_0000
  //white    := #b_0000_0110_0_0100_0100_0_0100_0000_0_0000_0000_0_0000
  //kings    := #b_0000_0000_0_0000_0000_0_0100_0000_0_0000_0000_0_0000

  //black    := #b_0000_0000_0_0000_0000_0_0000_0000_0_0100_0000_0_0000
  //white    := #b_0000_1000_0_0000_0100_0_0000_0000_0_0000_0000_0_0000
  //kings    := #b_0000_0000_0_0000_0000_0_0000_0000_0_0000_0000_0_0000

  { LET X, Y, K, Sig, Theta = black, white, kings, 1, 0 // Black moves first
    writef("*n*nInitial position, computer is %s.*n*n",
           computeriswhite -> "white", "block")
    playthegame(@X)
  }

//  IF tracing DO
  { LET len = listlength(freelist)
    writef("%n words used out of %n, length of freelist is %n (%n)*n",
            spacet-spacep,  spacet-spacev, len, 6*len) 
  }
  
fin:
  IF tostream DO endstream(tostream)
  IF spacev DO freevec(spacev)
  RESULTIS 0
}

AND playthegame(P) BE UNTIL done DO
{ // P -> [X, Y, K, Sigma, Theta]
  // This is the state from which the computer or user makes the next move.
  
  prboard(P, 0)
  
  TEST (h4!P=-1)=computeriswhite
  THEN { // Computer to make the next move.
         LET x, y, k, sigma, theta = 0, 0, 0, 0, 0
	 LET val = ChoosePosition(P, @x)
	 IF theta=0 DO
	 { // The game is lost
	   writef("*nThe computer resigns because it was unable to make a move.*n*n")
	   RETURN
	 }
         // x holds the position after the computer's move.
	 h1!P, h2!P, h3!P, h4!P, h5!P := x, y, k, sigma, theta
  }
  ELSE { // User to play.
         LET L = LegalPositionsFrom(P)
	 LET len = listlength(L)
	 LET n = 0
	 UNLESS len DO
         { writef("*nUser resigns because there are no possible moves.*n*n")
	   RETURN
	 }
	 writef("*nChoose one of the following %n available moves:*n*n", len)
	 prlist(L)
	 writef("*nType your chosen move number: ")
	 deplete(cos)
	 n := readn()
	 newline()

	 TEST 1 <= n <= len
         THEN { UNTIL n=1 DO L, n := h1!L, n-1
	        // Copy the selected position into P.
	        h1!P, h2!P, h3!P, h4!P, h5!P := h2!L, h3!L, h4!L, h5!L, h6!L
              }
         ELSE { LET X, Y, K, Sigma, Theta = h1!P, h2!P, h3!P, h4!P, h5!P
	        writef("*nUser chose to resign.*n*n")
                h1!P, h2!P, h3!P, h4!P, h5!P := Y, X, K, -Sigma, 0
	      }

         freenodelist(L)
       }
}

LET mknode(link, P) = VALOF
{ // P ->  [X, Y, K, Sigma, Theta] // The position after a move.
  LET t = freelist

  TEST t
  THEN { freelist := h1!t          // Use a freelist node.
       }
  ELSE { t := spacep - 6           // Allocate a new node.
         IF t<spacev DO
         { writef("More space needed*n")
           abort(999)
           RESULTIS link
         }
         spacep := t
       }

  h1!t, h2!t, h3!t, h4!t, h5!t, h6!t := link, h1!P, h2!P, h3!P, h4!P, h5!P

  RESULTIS t
}

AND freenode(node) BE
{ h1!node := freelist
  freelist := node
}
  
AND freenodelist(L) BE IF L DO
{ // Free every node in the given node list.
  LET t = L
  WHILE h1!t DO t := h1!t
  h1!t := freelist
  freelist := L
}
  
AND listlength(L) = VALOF
{ LET len = 0
  WHILE L DO L, len := h1!L, len+1
  RESULTIS len
}
  
LET ChoosePosition(P, r) = VALOF
{ // P -> [X, Y, K, Sigma, Theta]
  // r -> [x, y, k, sigma, theta]
  // X and Y are the player's and opponent's pieces.
  // K holds the kings on both sides.
  // Sigma=1 if the player's pieces are black, =-1 otherwise.
  // Theta is either zero or the position of the opponent's
  // piece that made the previous move.
  // This function attempts to select the player's best move.
  // If no move is possible or the player chooses to resign,
  // the result is -INF and r is set to [Y,X,K,-Sigma,0].
  // Otherwise r is set to the new position after making the
  // selected move, leaving it ready for the opponent.
  // theta will hold the position of the piece that made the
  // move. The result will be the player's estimate of the value
  // of the move.

  LET L = LegalPositionsFrom(P) // List of all possible new
                                // positions that can be reached
				// by the player in one move.
  LET val = 0 // This will hold the estimated value of the chosen move.
  
  //writef("ChoosePosition: Current position is:*n")
  //prboard(P, 0)
  //writef("ChoosePosition: List of all possible positions after one move*n")
  //IF L DO prlist(L)
  //abort(8211)
  
  val :=  BestPosition(L, 0, r) // Set r to best new position.
  //writef("ChoosePosition: From the following position:*n")
  //prboard(P, 0)
  //writef("The best position is as follows with value=%n*n", val)
  //prboard(r, 0)
  //abort(1235)
  freenodelist(L)
  
  RESULTIS val
}

AND BestPosition(L, d, r) = VALOF
{ // This function is logically the same as Strachey's version
  // but  uses a loop rather than recursion to iterate over the
  // elements of L.
  // A typical item in L will be [link, x, y, k, sigma, theta]
  
  LET P = 0    // Will hold the best position so far.
  AND V = -INF // To hold the value of position P.

  // L is the list of positions reached by one move from the
  // current one. The values of these positions are estimated
  // and one with the best value is selected. This is then
  // copied into r. Its x element will be the opponent's pieces
  // and sigma will indicate their colour.

  WHILE L DO
  { LET p = @h2!L  // This is the next reachable position to consider.
    // p => [x, y, k, sigma, theta]
    //       x corresponds to the opponent's pieces.
    // Each p is potentially a position the opponent could be given.
    LET v = -PositionValue(p, d+1) // Negate the opponent's estimate
                                   // of the position's value.
    IF v>V DO P, V := p, v // Update P and V if this seems to be a better move.
    IF tracing DO
    //IF d=1 DO
    { writef("Considering position at depth %n, value=%n*n", d, v)
      prboard(p, 0)
      IF debugging DO abort(4321)
    }
    L := h1!L // Consider another possible move, if any.
  }

  IF r TEST P
       THEN h1!r, h2!r, h3!r, h4!r, h5!r := h1!P, h2!P, h3!P, h4!P, h5!P
       ELSE h1!r, h2!r, h3!r, h4!r, h5!r := 0,    0,     0,    0,    0

  IF tracing DO
  //IF d<3 DO
  { TEST h5!r
    THEN { writef("Best Position at depth %n, value=%n*n", d, V)
           prboard(r, 0)
	 }
    ELSE { writef("The opponent could not make a move at depth %n, value=%n*n", d,V)
         }

    IF debugging DO abort(4322)
  }
  RESULTIS V
}

AND PositionValue(P, d) = Terminal(P, d) -> TerminalValue(P), VALOF
{ LET L   = LegalPositionsFrom(P)
  LET val = 0

  IF FALSE & tracing DO
  { writef("PositionValue: Possible opponent positions*n")
    prlist(L)
    IF debugging DO abort(7890)
  }
  val := BestPosition(L, d, 0) // 0 mean don't save the position.
                               // d is the depth of lookahead so far.
  freenodelist(L)

  IF tracing DO
  { writef("PositionValue: val=%n depth=%n*n", val, d)
    prboard(P, 0)
    IF debugging DO abort(7890)
  }
  RESULTIS val
}

AND LegalPositionsFrom(P) = VALOF
{ // First look for Capture moves.
  LET L = PositionList(P, Capture)
  //IF L DO
  //{ writef("LegalPositionsFrom: Positions reachable by capture moves*n")
  //  prlist(L)
  //}
  UNLESS L DO
  { // No Capture moves are possible so look for NonCapture moves.
    L := PositionList(P, NonCapture)
    //IF L DO
    //{ writef("LegalPositionsFrom: Positions reachable by non capture move*n")
    //  prlist(L)
    //}
  }
  //IF tracing DO
  //{ UNLESS L DO writef("LegalPositionsFrom: There are no legal moves*n")
  //  writef("LegalPositionsFrom: Returning list L=%n*n", L)
  //  IF debugging DO abort(1222)
  //}
  RESULTIS L
}

AND PositionList(P, C) = VALOF
{ // This returns the list of positions that can be reached
  // by moves of kind C (Capture or NonCapture) in any direction.
  LET L = NIL

  FOR i = 0 TO 3 DO
  { LET s = i ! TABLE 5, 4, -4, -5 // Iterate through the directions.
    L := Join(L, PartialPositionList(P, C, s))
  }
  
  RESULTIS L
}

AND PartialPositionList(P, C, s) = VALOF
{ // This returns the list of positions that can be reached
  // by moves of kind C (Capture or NonCapture) in direction s.

  LET L   = NIL
  LET set = FindMoveSet(P, C, s)
  // set holds the pieces that can make valid moves of kind C in
  // direction s. It uses a while loop rather than recursion to
  // iterate over the elements of the set.
  
  WHILE set DO
  { // Choose a piece from the non empty set of pieces that can
    // make valid moves of the right kind in direction s.
    LET phi = SingleDigitFrom(set)
    // phi holds the square occupied by a piece that can make a move
    // of the right kind in direction s.
    LET x, y, k, sigma, theta = 0, 0, 0, 0, 0 // For the new position.
    LET list = 0
    MakeMove(P, C, s, phi, @x)  // Make a single step of the move.

    // @x -> [x, y, k, sigma, theta]          // Position after this step.
    //                                        // theta holds the new square
                                              // of the piece that moved.

    list := C=Capture -> CaptureTree(@x),     // Explore the capture tree
                                              // return a list of all its
					      // final positions.
                         FinalPosition(@x)    // Return unit list containing
			                      // the final position of a
					      // NonCapture move.
    L := Join(L, list)
    set := set - phi // Remove piece phi from the set. Note that
                     // set & phi was non zero.
  }

  //writef("List of positions reachable by %sCapture moves in *
  //       *direction %n*n", C=Capture->"","Non", s)
  //prlist(L)

  RESULTIS L
}

AND FindMoveSet(P, C, s) = VALOF
{ // P -> [X, Y, K, Sigma, Theta]
  // C = Capture or NonCapture
  // s = 5, 4, -4 or -5
  // Return the bit pattern representing the set of pieces of
  // side X that can make a move of kind C moves in direction s.
  LET X, Y, K, Sigma, t = h1!P, h2!P, h3!P, h4!P, 0
  // X represents the pieces of the side to make a move.
  // Sigma=1 if X is contains black pieces.
  // s>0 represents a forward moves for men and kings of side X
  // s<0 represents a backward moves for kings of side X
  LET Empty = Board & ~(X + Y)  // Has a 1 in every empty square.
  LET psi = (C=Capture) -> (Shift(Empty, -Sigma*s) & Y), Empty
  
  // If C=Capture, psi is the set of pieces of side Y that have
  //       an empty square next to it in direction s.
  // If C=NonCapture, psi is the set of all empty squares.
 
  LET phi = Shift(psi, -Sigma*s) &  X

  // If C=Capture, phi is the set of pieces of X that can make
  //    capture move in direction s.
  // If C=NonCapture, phi is the set of pieces of X that can make
  //    NonCapture moves in direction s.
  
  //writef("FindMoveSet: X:     %40b*n", X)
  //writef("FindMoveSet: Y:     %40b*n", Y)
  //writef("FindMoveSet: K:     %40b*n", K)
  //writef("FindMoveSet: Sigma=%n s=%i2 C=%sCapture*n", Sigma, s, C->"","Non")
  //writef("FindMoveSet: psi:   %40b*n", psi)
  //writef("FindMoveSet: phi:   %40b*n", phi)
  //abort(1345)
  
  // Only kings can make moves backwards left or right.
  RESULTIS (s>0) -> phi, phi & K
}

AND MakeMove(P, C, s, phi, r) BE
{ // Move the piece in square phi in direction s and set the new
  // state in r. This function is only called for a valid first step of a move.
  // C is Capture or NonCapture
  // phi is the piece to move.
  // s is the direction of the move, for side X.
  // P -> [X, Y, K, Sigma, t]
  // sigma=1 if X are the black, otherwise -1.
  // r -> [x, y, k, sigma, theta]  The position after the move.
  //                               theta is the new position of the piece phi.
  //                               This is needed if the move is in a
  //                               capture sequence.
  LET X, Y, K, Sigma, t = h1!P, h2!P, h3!P, h4!P, 0

  LET psi, theta = 0, Shift(phi, Sigma*s) // NonCapture move
                                          // psi is not used in NonCapture moves.
					  
  IF C=Capture DO                         // Capture move
    psi, theta := theta, Shift(theta, Sigma*s)

  // theta is non zero since the move is known to be valid.
  // psi is the position of the captured piece. for Capture moves.
  //writef("MakeMove: X:     %40b*n", X)
  //writef("MakeMove: Y:     %40b*n", Y)
  //writef("MakeMove: K:     %40b*n", K)
  //writef("MakeMove: sigma=%n s=%i2 C=%sCapture*n", Sigma, s, C=Capture->"","Non")
  
  X := X & ~phi | theta       // Remove the old position of the piece
                              // at phi and add its new position at theta.
  Y := Y & ~psi               // Remove the captured piece, if any.

  IF (K & phi)~=0 |           // Add the K bit at position theta if
     (theta & LastRows)~=0 DO // the piece was already a king, or if
    K := K | theta            // has moved to the last row.

  K := K & ~psi & ~phi        // Ensure that the K bit is not set in either
                              // positions phi to psi.

  //writef("MakeMove: phi:   %40b*n", phi)
  //writef("MakeMove: psi:   %40b*n", psi)
  //writef("MakeMove: theta: %40b*n", theta)

  h1!r, h2!r, h3!r, h4!r, h5!r := X, Y, K, Sigma, theta

  //writef("MakeMove: The board after making the %sCapture move*n",
  //        C=Capture->"", "Non")
  //prboard(r, 0)

  UNLESS theta DO
  { writef("MakeMove: System error*n")
    abort(999)
  }

  //abort(1234)
}

AND FinalPosition(P) = VALOF
{ // P is either the position after a NonCapture move or
  // the position at the end of a Capture sequence.
  // Return a unit list containing this final position.
  LET X, Y, K, Sigma, Theta = h2!P, h1!P, h3!P, -h4!P, h5!P
  // Note that X and Y are swapped and Sigma is negated,
  // ready for the opponent to make a move.

  //IF tracing DO
  //{ writef("A final position*n")
  //  prboard(@X, 0)
  //  IF debugging DO abort(6334)
  //}
  RESULTIS mknode(NIL, @X)
}

AND CaptureTree(ip) = VALOF
{ // ip -> [ X, Y, K, Sigma, Phi] // An intermediate position.
  // Return a list of all final positions in the capture tree
  // belonging to piece phi from this position.
  LET L = PartialCapturePositionList(ip)
  //writef("CaptureTree: Partial position*n")
  //prboard(ip, 0)
  //writef("CaptureTree: Partial capture positions*n")
  //prlist(L)
  //abort(7654)

  UNLESS L RESULTIS FinalPosition(ip)  // ip is a final position.
  RESULTIS CombineCaptureTrees(L)      // More capture moves exist.
}

AND PartialCapturePositionList(ip) = VALOF
{ // ip -> [X, Y, K, Sigma, phi]
  LET K   = h3!ip
  LET phi = h5!ip
  RESULTIS MinList(PCP(ip, phi,      5),
                   PCP(ip, phi,      4),
                   PCP(ip, phi & K, -4), // Only allow backward moves
                   PCP(ip, phi & K, -5)) // if phi is a king.
}

AND PCP(P, phi, s) = VALOF
{ // P -> [X, Y, K, Sigma, t]
  // phi is zero or a piece that can possibly capture in direction s.
  LET X, Y, K, Sigma, t = h1!P, h2!P, h3!P, h4!P, 0
  LET psi = Shift(phi, Sigma*s) & Y
  // psi is a piece to capture, if that is possible.
  LET Empty = ~X & ~Y & Board
  LET theta = Shift(psi, Sigma*s) & Empty // The position of phi after the
                                          // capture if it was possible.

  UNLESS theta RESULTIS NIL   // The capture was not possible.

  X := X & ~phi | theta       // X with phi replaced by theta.
  Y := Y & ~psi               // Y with captured piece removed.
  
  K := K & ~phi & ~psi        // Positions phi and psi cannot be kings.
  IF (phi & K)~=0 |           // If phi was a king or the piece
     (theta & LastRows)~=0 DO // moved into the last row, ensure
    K := K | theta            // theta is a king.
  
  t := theta                  // The new position of phi.
  
  //writef("PCP: A capture move was possible to position:*n")
  //prboard(@X, 0)
  //abort(4567)
  // Return the list of final positions from the reached by a
  // capture moves by piece now at theta.
  RESULTIS CaptureTree(@X)
}

AND CombineCaptureTrees(L) =
  L -> NIL, Join(CaptureTree(@h2!L), CombineCaptureTrees(h1!L))

AND Join(L1, L2) = VALOF
{ LET res = L1

  UNLESS L1 RESULTIS L2
  
  // Append L2 onto the end of L1
  WHILE h1!L1 DO L1 := h1!L1
  h1!L1 := L2
  RESULTIS res
}

AND MinList(L1, L2, L3, L4) = VALOF
{ // It is as if a new list is formed containing all the
  // elements of L1 to L4. These are deleted and the combined
  // list returned. Duplicate items are removed. A much faster
  // version of this algorithm is possible.
  
  LET L = 0
  WHILE L1 DO { LET node = L1
                L1 := h1!L1
		L := insert(L, node)
              }
  WHILE L2 DO { LET node = L2
                L2 := h1!L2
		L := insert(L, node)
              }
  WHILE L3 DO { LET node = L3
                L3 := h1!L3
		L := insert(L, node)
              }
  WHILE L4 DO { LET node = L4
                L4 := h1!L4
		L := insert(L, node)
              }
 RESULTIS L
}

AND insert(L, node) = VALOF
{ // L is a non empty list of nodes.
  // If node is already in the list free it,
  // otherwise insert it at the start of L.
  TEST contains(L, node)
  THEN { freenode(node)   // Delete the node.
         RESULTIS L
       }
  ELSE { h1!node := L     // Insert the node at the start of L.
         RESULTIS node
       }
}

AND contains(L, node) = VALOF
{ // Return TRUE if node is in the node list L, otherwise FALSE.
  WHILE L DO
  { IF h2!L=h2!node & h3!L=h2!node &
       h4!L=h4!node & h5!L=h5!node &
       h6!L=h6!node RESULTIS TRUE
    L := h1!L
  }
  RESULTIS FALSE
}

AND SingleDigitFrom(x) = x & -x

AND bits(x) = VALOF
{ // Return the number of 1s in a bit pattern.
  LET n = 0
  WHILE x DO { x := x & (x-1) // Remove the least significant 1.
               n := n+1
	     }
  RESULTIS n
}

// Note that bits in BCPL bit patterns are numbered from right to left
// whereas in Strachey's paper they are numbered from left to right.
// So if n is positive Shift(x,n) must shift to the left, unlike Shift
// in the paper.
AND Shift(x,n) = (n>=0 -> x<<n, x>>-n) & Board

AND Terminal(P,d) = d>4

AND TerminalValue(P) = VALOF
{ // This is a very primitive strategy value function.
  LET kings     = h3!P
  LET playmen   = h1!P & ~kings
  LET playkings = h1!P &  kings
  LET oppomen   = h2!P & ~kings
  LET oppokings = h2!P &  kings
  LET pm, pk = bits(playmen), bits(playkings)
  LET om, ok = bits(oppomen), bits(oppokings)
  LET val = 3 * (pm - om) + 10 * (pk - ok)

  //writef("X=    %40b*n", h1!P)
  //writef("Y=    %40b*n", h2!P)
  //writef("K=    %40b*n", h3!P)
  //writef("Sigma=%n*n",   h4!P)
  //writef("Theta=%40b*n", h5!P)
  //prboard(P, 0)

  IF tracing DO
  { writef("TerminalValue: pm=%n pk=%n om=%n ok=%n*n", pm, pk, om, ok)
    writef("Value of the following terminal position = %n*n", val)
    prboard(P, 0)
    //IF debugging DO abort(1000)
  }
  RESULTIS val
}

/*
The bit positions of the board are as follows:

      *  5  *  6  *  7  *  8
      9  * 10  * 11  * 12  *
      * 14  * 15  * 16  * 17
     18  * 19  * 20  * 21  *
      * 23  * 24  * 25  * 26
     27  * 28  * 29  * 30  *
      * 32  * 33  * 34  * 35
     36  * 37  * 38  * 39  *

*/

AND prboard(P,n) BE IF P DO
{ // P = 0 or -> [X, Y, K, Sigma, Theta]

  TEST n THEN writef("%i2: ", n)
         ELSE ps()
  writef("  ----------Black-----------  ")
  TEST h4!P>0 THEN writef("  Black to play*n")
              ELSE writef("  White to play*n")
  ps();pb();bl();sq( 5,P);bl();sq( 6,P);bl();sq( 7,P);bl();sq( 8,P);pb();nl()
  ps();pb();sq( 9,P);bl();sq(10,P);bl();sq(11,P);bl();sq(12,P);bl();pb();nl()
  ps();pb();bl();sq(14,P);bl();sq(15,P);bl();sq(16,P);bl();sq(17,P);pb();nl()
  ps();pb();sq(18,P);bl();sq(19,P);bl();sq(20,P);bl();sq(21,P);bl();pb();nl()
  ps();pb();bl();sq(23,P);bl();sq(24,P);bl();sq(25,P);bl();sq(26,P);pb();nl()
  ps();pb();sq(27,P);bl();sq(28,P);bl();sq(29,P);bl();sq(30,P);bl();pb();nl()
  ps();pb();bl();sq(32,P);bl();sq(33,P);bl();sq(34,P);bl();sq(35,P);pb();nl()
  ps();pb();sq(36,P);bl();sq(37,P);bl();sq(38,P);bl();sq(39,P);bl();pb();nl()
  ps();writef("  ----------White-----------  "); nl()

//IF debugging DO abort(1357)
}

AND ps() BE writef("    ")
		    
AND pb() BE writef(" | ")

AND bl() BE writef(" + ")

AND nl() BE newline()

AND sq(n, P) BE
{ // P -> [X, Y, K, Sigma, Theta]
  LET B, W, K, Sigma, theta = h1!P, h2!P, h3!P, h4!P, h5!P
  LET bit = 1<<n
  LET ch = '*s'
  
  IF Sigma<0 DO B, W := h2!P, h1!P  
  // B holds the black pieces, and W the white one.

  IF (B&bit)>0 TEST (K&bit)>0 THEN ch := 'B' ELSE ch := 'b'
  IF (W&bit)>0 TEST (K&bit)>0 THEN ch := 'W' ELSE ch := 'w'
  TEST (theta & bit)=0
  THEN writef(" %c ", ch)
  ELSE writef("(%c)", ch)
}

AND prlist(L) BE
{ LET count = 0

  UNLESS L DO
  { writef("The list is empty*n")
    RETURN
  }
  WHILE L DO
  { count := count+1
    prboard(@h2!L, count)
    L := h1!L
  }
}

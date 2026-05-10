/*
Noughts and Crosses is a game played by two players (O and X) on a
board with nine positions numbered as follows:

                    1 | 2 | 3
                   ---+---+---
                    4 | 5 | 6
                   ---+---+---
                    7 | 8 | 9

The players place their marks (O and X) in unoccupied positions on the
board until the game is complete.  A completed game when there is
either

   (1) a straight line of three Xs giving a win for X,

or (2) a straight line of three Os giving a win for O,

or (3) all nine positions are occupied, in which case the game is drawn. 

O is the first player to move.

It is required to construct a structure representing the tree of
all possible games. Each nodes of the tree should represent a
reachable board state, with the root being the empty board, and the
leaf nodes corresponding to won, lost or drawn games.


The followingis a solution in ML


datatype side = O | X;

datatype tree = N of side        (* side to play            *)
                   * int  list   (* positions occupied by O *)
                   * int  list   (* positions occupied by X *)
                   * tree list;  (* successor positions     *)

fun mem(x,    []) = false
  | mem(x, y::ys) = x=y orelse mem(x, ys);

fun len [] res = res
  | len (x::xs) res = len xs (res+1)

fun won ps = len ps 0 > 2 andalso
             (
             mem(5,ps) andalso ( mem(1,ps) andalso mem(9,ps)
                                 orelse
                                 mem(2,ps) andalso mem(8,ps)
                                 orelse
                                 mem(3,ps) andalso mem(7,ps)
                                 orelse
                                 mem(4,ps) andalso mem(6,ps)
                               )
             orelse
             mem(1,ps) andalso ( mem(2,ps) andalso mem(3,ps)
                                 orelse
                                 mem(4,ps) andalso mem(7,ps)
                               )
             orelse
             mem(9,ps) andalso ( mem(3,ps) andalso mem(6,ps)
                                 orelse
                                 mem(7,ps) andalso mem(8,ps)
                               )
             );

fun mksucs (s, Os, Xs,      [],  _) res = res
  | mksucs (O, Os, Xs, (p::ps), qs) res = 
          mksucs(O, Os, Xs, ps, p::qs) (mknode(X, p::Os, Xs, ps@qs)::res)
  | mksucs (X, Os, Xs, (p::ps), qs) res = 
          mksucs(X, Os, Xs, ps, p::qs) (mknode(O, Os, p::Xs, ps@qs)::res)

and mknode(s, Os, Xs, []) = N(s, Os, Xs, []) 
  | mknode(s, Os, Xs, free) =
             if won Os orelse won Xs 
             then N(s, Os, Xs, [])
             else N(s, Os, Xs, mksucs(s, Os, Xs, free, []) []);

fun mktree() = mknode(O,[],[],[1,2,3,4,5,6,7,8,9]);

fun Owins []                  res = res 
  | Owins (N(s,Os,Xs,[])::ps) res =  
         if s=X andalso won Os then Owins ps (res+1)
                               else Owins ps res
  | Owins (N(_,_,_,ss)::ps) res = Owins ss (Owins ps res);

val t = ref (N(O,[],[],[]));
t := mktree();
Owins[!t] 0;
*/




// Here is a solution in BCPL


GET "libhdr"

MANIFEST {
 O=0; X
 L123 = #b111_000_000
 L456 = #b000_111_000
 L789 = #b000_000_111
 L147 = #b100_100_100
 L258 = #b010_010_010
 L369 = #b001_001_001
 L159 = #b100_010_001
 L357 = #b001_010_100
 All  = #b111_111_111
}

GLOBAL {
  stv:ug
  stt
  stp
  nodecount
  won
  owins
  mkN
  mksucs
  mknode
  mktree
}

/*
The tree representing all possible games uses nodes of 5 elements
representing states as follows.

side   The side O or X to make the next move.

os     A bit pattern representing the set of board positions
       containing Os.

xs     A bit pattern representing the set of board positions
       containing Xs. Note that (os&xs)=0.

sucs   The list of states that can be reached by the current
       side making one move.

next   The link to the next state in a list of successors.

A final state is one that has no successors. This correspond
to a state tha is a win for O or X, or a state in which all
9 board positions are occupied.
*/

LET won : set => (set&L123)=L123 | (set&L456)=L456 | (set&L789)=L789 |
                 (set&L147)=L147 | (set&L258)=L258 | (set&L369)=L369 |
                 (set&L159)=L159 | (set&L357)=L357 -> TRUE, FALSE

LET owins // Returns the number of different ways O can win.

: 0 => 0  // End of a list of successors or alernatives.

: [X, os, xs,    0, next] =>    // A final made by X
    won(os) -> owins(next) + 1, // Increment count if a win for O
               owins(next)      // Not a win for O.
	       
: [s, os, xs, sucs, next] => owins(sucs) + // Wins from this successor.
                             owins(next)   // Wins from other successors.

LET mkN : side, os, xs, sucs, next => VALOF
{ // Create a new state node.
  stp := stp-5                // 5 words per node
  IF stp<stv DO
  { writef("More space needed*n")
    abort(999)
  }
  //writef("mkN: side=%n os=%b9 ox=%b9 sucs=%n next=%n*n",
  //             side,   os,    xs,    sucs,   next)
  nodecount := nodecount+1
  stp!0 := side
  stp!1 := os
  stp!2 := xs
  stp!3 := sucs  // List of successor positions.
  stp!4 := next  // Successor list link.
  RESULTIS stp
}

AND mksucs // Return the list of successors of the given state.
: O, os, xs  => VALOF
  { LET bits = All - os - xs // Bit pattern of empty locations.
    LET sucs = 0  // To hold the list of successor positions.
    //writef("mksucs: side=O os=%b9 zs=%b9 bits=%b9*n", os, xs, bits)
    //abort(1000)
    IF won(xs) RESULTIS 0 // O has just lost.
    WHILE bits DO // Iterate over all free board positions.
    { LET bit = bits & -bits // Select a free position.
      bits -:= bit
      sucs := mknode(X, os+bit, xs, sucs)
    }
    RESULTIS sucs
  }

: X, os, xs  => VALOF
  { LET bits = All - os - xs // Bit pattern of empty locations.
    LET sucs = 0  // To hold the list of successor positions.
    //writef("mksucs: side=X os=%b9 zs=%b9*n", os, xs)
    //abort(1001)
    IF won(os) RESULTIS 0 // X has just lost.
    WHILE bits DO // Iterate over all free locations.
    { LET bit = bits & -bits // Select a free location
      bits -:= bit
      sucs := mknode(O, os, xs+bit, sucs)
    }
    RESULTIS sucs
  }

AND mknode : side, os, xs, next => VALOF
{ LET sucs = mksucs(side, os, xs)
  RESULTIS mkN(side, os, xs, sucs, next)
}

AND mktree : => mknode(O, 0, 0, 0)

LET start : => VALOF
{ LET upb = 5 * 550_000  // Just enough space, 5 words per node.
  AND tree = 0
  stv := getvec(upb)
  stt := stv + upb
  stp := stt
  UNLESS stv DO abort(999)
  nodecount := 0
  tree := mktree()
  writef("Number of nodes is %n*n", nodecount)
  writef("Number of winning games for O is %n*n", owins(tree))
  freevec(stv)
  RESULTIS 0
}

/*
It writes:     Number of nodes is 549946
               Number of winning games for O is 131184
*/

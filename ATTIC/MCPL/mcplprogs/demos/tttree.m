/*
ML long Question

Naughts and Crosses is a game played by two players ({\bf O} and
{\bf X}) on a board with nine positions numbered as follows:

                    1 | 2 | 3
                   ---+---+---
                    4 | 5 | 6
                   ---+---+---
                    7 | 8 | 9

The players place their marks ({\bf O} and {\bf X}) in unoccupied
positions on the board until the game is complete.  A completed game
when there is either

   (1) a straight line of three {\bf X}s giving a win for {\bf X},

or (2) a straight line of three {\bf O}s giving a win for {\bf O},

or (3) all nine positions are occupied, in which case the game is drawn. 

{\bf O} is the first player to move.

It is required to construct an ML structure representing the tree of
all possible games. Each nodes of the tree should represent a
reachable board state, with the root being the empty board, and the
leaf nodes corresponding to won, lost or drawn games.

Define the ML data type {\tt tree} that you would use to represent this 
game tree.                                                  [3 marks]
                                  
Define the function {\tt mktree : unit->tree} to construct the
complete game tree, explaining carefully how it works. There is
no need for your implementation to be efficient is either space
or time.                                                    [10 marks]

Briefly discuss ways in which your implementation of {\tt mktree} could
be made more efficient.                                     [4 marks]

Define a function {\tt Owins : tree->int} which when applied to the
complete tree will yield the number of distinct games in which {\bf O}
wins.                                                       [3 marks]






What follows is a solution in ML followed by a straightforward
MCPL translation.

On a 200 MHz Pentium Pro, an interpretive version of the MCPL program
take 12.5 seconds. Using the native version of the compiler it
takes 0.76 seconds.

On my 90 MHz laptop, Moscow ML take 3 minutes to decide there is not
enough heap space, but much of the time was spent thrashing the virtual
memory.







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







GET "mcpl.h"

MANIFEST O, X,
         L123 = #b111_000_000, L456 = #b000_111_000, L789 = #b000_000_111,
         L147 = #b100_100_100, L258 = #b010_010_010, L369 = #b001_001_001,
         L159 = #b100_010_001, L357 = #b001_010_100, All  = #b111_111_111

FUN won : set => IF set&L123=L123 OR set&L456=L456 OR set&L789=L789 OR
                    set&L147=L147 OR set&L258=L258 OR set&L369=L369 OR
                    set&L159=L159 OR set&L357=L357 RETURN TRUE
                 RETURN FALSE
.
FUN mksucs
: O, os, xs, free => LET bits=free, res=0
                     WHILE bits DO
                     { LET bit = bits & -bits
                       bits -:= bit
                       res := mknode(X, os+bit, xs, free-bit, res)
                     }
                     RETURN res

: X, os, xs, free => LET bits=free, res=0
                     WHILE bits DO
                     { LET bit = bits & -bits
                       bits -:= bit
                       res := mknode(O, os, xs+bit, free-bit, res)
                     }
                     RETURN res
.
FUN mknode : side, os, xs, free, next =>
  LET sucs = (free=0 OR won os OR won xs) -> 0,
             mksucs(side, os, xs, free)
  RETURN mkN(side, os, xs, sucs, next)
.
FUN mktree : => mknode(O, 0, 0, All, 0)
.
STATIC stv, stt, stp, nodecount=0

FUN owins // Returns the number of different ways O can win.
:                       0 => 0
: [X, os, xs,    0, next] => won os -> owins next + 1,
                                       owins next
: [s, os, xs, sucs, next] => owins sucs + owins next
.
FUN mkN : s, os, xs, sucs, next =>
  stp := @ stp!-5          // 5 words per node
  IF stp<stv DO abort 999
  nodecount++
  MATCH stp : [:=s,:=os,:=xs,:=sucs,:=next] => stp
            .
.

FUN start : =>
  stv := getvec  (5 * 550_000) // Allocate just enough space.
  stt := @ stv ! (5 * 550_000) // (5 words per node)
  stp := stt
  UNLESS stv DO abort 777
  LET t = mktree()
  writef("Number of nodes is %d\n", nodecount)
  writef("Number of winning games for O is %d\n", owins t)
  freevec stv
  RETURN 0
.
/*
It writes:     Number of nodes is 549946
               Number of winning games for O is 131184
*/

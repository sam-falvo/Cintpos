/*
This is an experimental implementation os Conway's game of life.

Implementented by martin Richards (c) November 2016

The game of life consists of a 2D array of cells each of which may be
alive or dead. At every tick of the clock the state of every cell may
change based on the following rules.

A live cell will stay alive if exactly two or three of its eight
nearest neighbours were alive, otherwise it dies.

A dead cell will only become alive if exactly three of its eight
nearest neighbours were alive, otherwise it remains dead.

There are many ways to implement this game, but this program uses a
bit pattern technique that is probably best suited when the population
has a high proportion of live cells.

This program currently assumes that it is running on a 32-bit version
of BCPL.

There are two arrays map1 and map2 that hold bits representing the
state of previous and next generation. A one bit represents a live
cell and a zero bit represents a dead cell. These are packed into
32-bit words. map1!0 holds the 32 bits representing the state of the
leftmost cells in the top row of the 2D cell array, and
map1!(rowwords-1) holds the bits for the rightmost 32 cells in the top
row.  The next row of cells is from map1!rowwords to
map1!(rowwords+rowwords-1). The nth row of cell words is from
map1!(n*rowwords) to map1!(n*rowwords+rowwords-1).  The cells in the
top and bottom rows are always forced to be dead, and the left and
rightmost 32 cells of each row are also forced to be dead. If nupb is
the number of the bottom row, the active rows are numbered from 1 to
nupb-1 and the active column words are numbered from 1 to rowwords-2.

If p points to an active word in map1 and q points to the
corresponding word in map2, then the bit pattern !q is given a value
based on !p and the immediate neighbours of !p. Let cellsC be set to
!p, and let us consider bit i of cellsC. This will indicate whether a
particular cell in the current row is alive or dead. The rules of life
require us to count how many of the eight neighbouring cells are
alive. We can define cellsNW to be a bit pattern in which bit i is a
one if the cell north west of the current cell is alive. Other
variables, cellsN, cellsNE, cellsW, cellsE, cellsSW, cellsS and cellsSE can
be similarly defined. These typically require a bitwise left or right
shift and care must be taken with bits at the left and right hand end
of the cell word. We need to count how many of the ith bits of these
eight variable are ones. These 32 counts can be done simultaneously
using longitudinal arithmetic as follows. We will form 32 counts in
variable A, B, C and D. Bit i of these variables will hold the binary
digits the 4-bit counter corresponding to the ith cell in !p. If the
count was 5 then the ith bits of D, C, B and A would be 0, 1, 0 and 1
respectively. Note that since no count can be greater than 8, if the
ith bit of D is a one then the ith bits of C, B and A will all be
zero. Look at the definition of step to see how these counts are
incremented.


The game of life rules are equivalent to the following with
x standing for either 0 or 1.

  Di Ci Bi Ai  cellsCi   new cellsCi

  0  0  0  0        x     =>      0
  0  0  0  1        x     =>      0
  0  0  1  0        0     =>      0
  0  0  1  0        1     =>      1
  0  0  1  1        x     =>      1
  0  1  x  x        x     =>      0
  1  x  x  x        x     =>      0   DCBA can only be 1000

So the next bit pattern for the cellsC can be set in map2 by the assignment:

      !q := (~(C|D)) & B & (A | cellsC)


The algorithm interates over all values of p in the active area of map1 using
the appropriate values of q.

Having done this it swaps map1 and map2 and repeats the process,
displaying a portion of the array each time.

Usage: r/n,c/n,xs/n,ys/n,t/n,seed/n,p/n,-t/s

r and and c are the number of rows and columns in the 2D array of cells.
            Default r=1024 and c=1024
xs and ys is the display window size. Default: xs=800 ys=600
t            the test number
             t=0  fill a square region at the center with random values
             t=1  place a carefully selected initial state in the cells
r            This sets the seed of the random number generator, default 12345
p            This is the percentage probabily that a given cell is alive
             when random data is used.
-t           Turn on tracing
d            This specifies a delay time in msecs after each generarion
             has been diplayed. The default is zero.
*/

GET "libhdr"
GET "sdl.h"
GET "sdl.b"                 // Insert the SDL library source code
.
GET "libhdr"
GET "sdl.h"

GLOBAL {
  map1: ug
  map2
  mapupb
  mapupb
  rows           // The number of active cells in each column
  cols           // The number of active cells in each row
  xsize          // The window size
  ysize
  screencentrex  // The coordinates of the screen centre
  screencentrey
  cellcentrex    // The coordinates of the centre of the cell array
  cellcentrey
  testno
  seed
  percentage

  rowwords       // Each word represents 32 cells
  nupb           // =rows+2  ie including the top and bottom dead rows

  col_black
  col_blue
  col_red
  col_lightgray

  tracing
  delaymsecs
}

LET start() = VALOF
{ LET argv = VEC 50

  UNLESS rdargs("r/n,c/n,xs/n,ys/n,t/n,s/n,p/n,d/n,-t/s", argv, 50) DO
  { writef("Bad arguments for LIFE*n")
    RESULTIS 0
  }

  rows, cols := 1024, 1024
  //rows, cols := 32, 64
  xsize, ysize := 800, 600
  seed := 12345
  testno := 0
  percentage := 20
  delaymsecs := 0

  IF argv!0 DO rows       := !(argv!0)     // r/n
  IF argv!1 DO cols       := !(argv!1)     // c/n
  IF argv!2 DO xsize      := !(argv!2)     // xs/n
  IF argv!3 DO ysize      := !(argv!3)     // ys/n
  IF argv!4 DO testno     := !(argv!4)     // t/n
  IF argv!5 DO seed       := !(argv!5)     // s/n
  IF argv!6 DO percentage := !(argv!6)     // p/n
  IF argv!7 DO delaymsecs := !(argv!7)     // d/n
  tracing := argv!8                        // -t/s

  setseed(seed)

  // There must be at least 32 rows and 32 cols
  IF rows < 32 DO rows := 32
  IF cols < 32 DO cols := 32

  // cols must be a multiple of 32
  UNTIL cols MOD 32 = 0 DO cols := cols + 1

  // The window size must be no larger than the cell array
  IF xsize > cols DO xsize := cols
  IF ysize > rows DO ysize := rows
  screencentrex, screencentrey := xsize/2, ysize/2

  writef("*nGame of LIfe*n*n")

  writef("Cell array:  (%n,%n)*n", cols, rows)
  writef("Window size: (%n,%n)*n", xsize, ysize)
  writef("Testno     = %n*n", testno)
  writef("seed       = %n*n", seed)
  writef("Percentage = %n%%*n", percentage)
  
  rowwords := (cols-1)/32 + 3 // The number of elements in each row including
                              // the left and right hand zero words.
  nupb := rows + 2            // The number of rows including the rows of zeros
                              // at the top and the bottom.
  writef("rowwords=%n nupb=%n*n", rowwords, nupb)

  cellcentrex, cellcentrey := cols/2, rows/2 // Centre of of cell array
  writef("cellcentrex=%n cellcentrey=%n*n", cellcentrex, cellcentrey)

  mapupb := nupb * rowwords - 1
  writef("mapupb = %n*n", mapupb)
//abort(1002)
  initsdl()
  mkscreen("The game of Life", xsize, ysize)

  col_black       := maprgb(  0,   0,   0)
  col_blue        := maprgb(  0,   0, 255)
  col_red         := maprgb(255,   0,   0)
  col_lightgray   := maprgb(180, 180, 180)

  map1, map2 := 0, 0

  fillsurf(col_lightgray)

  //updatescreen()

  //setcolour(col_black)
  //drawf(250, 30, "Game of Life")

  updatescreen()   //Update the screen

  initcells()

  FOR k = 1 TO 1_000_000 DO
  //FOR k = 1 TO 91 DO
    step()

  sdldelay(30_000) //Pause for 30 secs
  closesdl()       //Quit SDL

  IF map1 DO freevec(map1)
  IF map2 DO freevec(map2)

  RESULTIS 0
}

AND initcells() = VALOF
{ map1 := getvec(mapupb)
  map2 := getvec(mapupb)
  UNLESS map1 & map2 RESULTIS FALSE
  // Clear both maps
  FOR i = 0 TO mapupb DO map1!i, map2!i := 0, 0
  
  // Now set map1
  SWITCHON testno INTO
  { DEFAULT:
    CASE 0: FOR x = - xsize/8 TO + xsize/8 DO
              FOR y = - ysize/8 TO + ysize/8 DO
                IF randno(100)<=percentage DO putbit(x, y)
            RESULTIS TRUE

    CASE 1: putbit(-1, 0)
            putbit( 0, 0)
            putbit(+1, 0)
            RESULTIS TRUE

    CASE 2: glider( 20, 20)
            glider(-20,-20)
            glider(-20, 20)
            glider( 20,-20)
            glider(  0,  0)
            RESULTIS TRUE

    CASE 3: glidergun( 60, 60)
            glidergun(-60,-60)
            glidergun(-60, 60)
            glidergun( 60,-60)
            glidergun(  0,  0)
            RESULTIS TRUE

    CASE 4: FOR x = -screenxsize/10 TO +screenxsize/10 DO
              IF randno(100) > percentage DO putbit( x, 0)
            RESULTIS TRUE

    CASE 5: glider(-10-100, -20)
            glider1(10-100, -20)  // Two blocks

            glider(-10-60, -20)
            glider1(10-60, -21)  // Disappear

            glider(-10-20, -20)
            glider1(10-20, -22)  // Disappear

            glider(-10+20, -20)
            glider1(10+20, -23)  // Lozenge

            glider(-10+60, -20)
            glider1(10+60, -24)  // Cross/Square

            RESULTIS TRUE

    CASE 6: glider(-10-100, -20)
            glider2(10-100, -20)  // Two blocks

            glider(-10-60, -20)
            glider2(10-60, -21)  // Disappear

            glider(-10-20, -20)
            glider2(10-20, -22)  // Disappear

            glider(-10+20, -20)
            glider2(10+20, -23)  // Lozenge

            glider(-10+60, -20)
            glider2(10+60, -24)  // Cross/Square

            RESULTIS TRUE
  }
}

AND glider(x,y) BE
{ putbit(x+0,y-1)     //      #
  putbit(x+1,y+0)     //        #
  putbit(x-1,y+1)     //    # # #
  putbit(x+0,y+1)
  putbit(x+1,y+1)
}

AND glider1(x,y) BE
{ putbit(x+0,y-1)     //      #
  putbit(x-1,y+0)     //    #
  putbit(x-1,y+1)     //    # # #
  putbit(x+0,y+1)
  putbit(x+1,y+1)
}

AND glider2(x,y) BE
{ putbit(x-1,y+0)     //      
  putbit(x+1,y+0)     //    #   #
  putbit(x-1,y+1)     //    # #
  putbit(x+0,y+1)     //      #
  putbit(x+0,y+2)
}

AND glidergun(x,y) BE
{ // The following is a picture of the Bill Gosper's Glider Gun

  //     0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3
  //     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
  //     -----------------------------------------------------------------------
  // -4 |. . . . . . . . . . . . . . . . . . . . . . . . # . . . . . . . . . . .|
  // -3 |. . . . . . . . . . . . . . . . . . . . . . # . # . . . . . . . . . . .|
  // -2 |. . . . . . . . . . . . # # . . . . . . # # . . . . . . . . . . . . # #|
  // -1 |. . . . . . . . . . . # . . . # . . . . # # . . . . . . . . . . . . # #|
  //  0 |# # . . . . . . . . # . . . . . # . . . # # . . . . . . . . . . . . . .|
  // +1 |# # . . . . . . . . # . . . # . # # . . . . # . # . . . . . . . . . . .|
  // +2 |. . . . . . . . . . # . . . . . # . . . . . . . # . . . . . . . . . . .|
  // +3 |. . . . . . . . . . . # . . . # . . . . . . . . . . . . . . . . . . . .|
  // +4 |. . . . . . . . . . . . # # . . . . . . . . . . . . . . . . . . . . . .|
  //     -----------------------------------------------------------------------

  putbit(x+00, y+0); putbit(x+00, y+1)
  putbit(x+01, y+0); putbit(x+01, y+1)
  putbit(x+10, y+0); putbit(x+10, y+1); putbit(x+10, y+2)
  putbit(x+11, y-1); putbit(x+11, y+3)
  putbit(x+12, y-2); putbit(x+12, y+4)
  putbit(x+13, y-2); putbit(x+13, y+4)
  putbit(x+14, y+1)
  putbit(x+15, y-1); putbit(x+15, y+3)
  putbit(x+16, y+0); putbit(x+16, y+1); putbit(x+16, y+2)
  putbit(x+17, y+1)
  putbit(x+20, y-2); putbit(x+20, y-1); putbit(x+20, y+0)
  putbit(x+21, y-2); putbit(x+21, y-1); putbit(x+21, y+0)
  putbit(x+22, y-3); putbit(x+22, y+1)
  putbit(x+24, y-4); putbit(x+24, y-3); putbit(x+24, y+1); putbit(x+24, y+2)
  putbit(x+34, y-2); putbit(x+34, y-1)
  putbit(x+35, y-2); putbit(x+35, y-1)
}

AND putbit(x, y) BE
{ // Make the cell at position x,y live
  // The (0,0) is at the centre of the cell array
  // First move the origin to (cellcentrex,cellcentrey)
  LET p = ?     // Will point to the word in map1 containing the cell
  AND bitsh = ? // The position of the cell bit within the word
  //writef("putbit: x=%i5   y=%i5*n", x, y)
  plotlivecell(x,y)
  x, y := x+cellcentrex, y+cellcentrey
  //writef("putbit: x=%i5   y=%i5 relative to the top leftmost cell*n", x, y)
  p := rowwords*y + 1 + x/32
  //writef("putbit: position of the cell word relative to map1 p=%i5*n", p)
  p := map1 + p
  bitsh := 31 - x MOD 32 // The leftmost bit is the most significant bit
  !p := !p | 1<<bitsh
  //writef("putbit: bitsh=%i2 p=%i5  !p=%32b*n", bitsh, p, !p)
  //abort(1000)
  //FOR i = 1 TO nupb-2 DO
  //{ LET p = map1 + i*rowwords 
  //  writef("%i2: ", i)
  //  FOR i = 1 TO rowwords-2 DO
  //    writef(" %x8", p!i)
  //  newline()
  //}
  //abort(1001)
  
}

AND getbit(x, y) = VALOF
{ // Get the cell value at position x,y
  // The (0,0) is at the centre of the cell array
  // First move the origin to (cellcentrex,cellcentrey)
  LET p = ?     // Will point to the word in map1 containing the cell
  AND bitsh = ? // The position of the cell bit within the word
  //writef("getbit: x=%i5   y=%i5*n", x, y)
  x, y := x+cellcentrex, y+cellcentrey
  //writef("getbit: x=%i5   y=%i5 relative to the top leftmost cell*n", x, y)
  p := rowwords*y + 1 + x/32
  //writef("getbit: position of the cell word relative to map1 p=%i5*n", p)
  p := map1 + p
  bitsh := 31 - x MOD 32 // The leftmost bit is the most significant bit
  RESULTIS (!p >> bitsh) & 1
}

AND step() BE
{ // Step the game of life by one generation.
  // map1 holds the current generation
  // The next generation is placed in map2
  // then the pointers map1 and map2 are swapped.

  // First display map1
  display(map1)

  //FOR i = 14 TO 20 DO
  //{  writef("%i2:", i)
  //   FOR c = 1 TO rowwords-2 DO
  //     writef(" %32b", map1!(i*rowwords+c))
  //   newline()
  //}
  //newline()
//abort(1000)

  FOR i = 1 TO rowwords-2 DO
  { LET p = map1 + rowwords + i  // Pointer to top active cell word in column i
    LET q = map2 + rowwords + i  // Corresponding pointer in map2
    LET cellsNW, cellsN, cellsNE = 0, 0, 0
    LET cellsC = !p
    LET cellsW = (p!-1 & 1)<<31 | cellsC>>1 
    LET cellsE = cellsC<<1 | (p!1>>31 & 1)
    LET cellsSW, cellsS, cellsSE = ?, ?, ?

    FOR j = 1 TO rows DO
    { LET A, B, C, D, carryA, carryB = 0, 0, 0, 0, 0, 0
 
      //IF tracing & i=2 & 14 <= j <= 20 DO
      //{ LET nw, n, ne = p!(  -rowwords-1), p!(  -rowwords), p!(  -rowwords+1)
      //  LET w,  c, e  = p!(           -1), p!(          0), p!(           +1)
      //  LET sw, s, se = p!(  +rowwords-1), p!(  +rowwords), p!(  +rowwords+1)
      //  //IF (nw|n|ne|w|c|e|sw|s|se)~=0 DO
      //  {
      //    writef("col=%i4   row=%i4*n", i, j)

      //    writef("%32b %32b %32b*n", nw, n,ne)
      //    writef("%32b %32b %32b*n",  w, c, e)
      //    writef("%32b %32b %32b*n", sw, s,se)
      //    newline()
  //abort(2000)
      //  }
      //}

      //writef("map1=%i5  p=%i5  rel to map1 p=%i5*n", map1, p, p-map1)
      p := p + rowwords      // Pointer to the next cell word in this column
      cellsS  := !p
      cellsSW := (p!-1 & 1)<<31 | cellsS>>1 
      cellsSE := cellsS<<1 | (p!1>>31 & 1)

      // We now longitudinally add the eight bits patterns.
      // Note the the eighth one to add is the only time D can be chenged.

      A      := cellsNW           // Add cellsNW   Max count 1

      B      := A  &  cellsN      // Add cellsN    Max count 10
      A      := A XOR cellsN

      carryA := A  &  cellsNE     // Add cellsNE   Max count 11
      A      := A XOR cellsNE
      //C      := B  &  carryA
      B      := B XOR carryA

      carryA := A  &  cellsW      // Add cellsW    Max count 100
      A      := A XOR cellsW
      carryB := B  &  carryA
      B      := B XOR carryA
      C      := C  |  carryB      // Once bit i of C is 1 it remains 1

      carryA := A  &  cellsE      // Add cellsE    Max count 101
      A      := A XOR cellsE
      carryB := B  &  carryA
      B      := B XOR carryA
      C      := C  |  carryB

      carryA := A  &  cellsSW     // Add cellsSW    Max count 110
      A      := A XOR cellsSW
      carryB := B  &  carryA
      B      := B XOR carryA
      C      := C  |  carryB

      carryA := A  &  cellsS      // Add cellsS     Max count 111
      A      := A XOR cellsS
      carryB := B  &  carryA
      B      := B XOR carryA
      C      := C  |  carryB

      carryA := A  &  cellsSE     // Add cellsSE    Max count 1000
      A      := A XOR cellsSE     // This is the only time
      carryB := B  &  carryA      // the count of neighbours can be 8
      B      := B XOR carryA
      D      := C  &  carryB
      C      := C XOR carryB
/*
The game of life rules are equivalent to the following with
x standing for either 0 or 1.

  Di Ci Bi Ai  cellsCi   new cellsCi

  0  0  0  0        x     =>      0
  0  0  0  1        x     =>      0
  0  0  1  0        0     =>      0
  0  0  1  0        1     =>      1
  0  0  1  1        x     =>      1
  0  1  x  x        x     =>      0
  1  x  x  x        x     =>      0   DCBA can only be 1000

So the next bit pattern for the cellC can be set in map2 by the assignment:
*/
      !q := (~(C|D)) & B & (A | cellsC)

// There are 2^32-1 other possible rules for an extended version of life
// based on cellC and the count of how many of the eight nearest neighbours
// are alive. Each possibility has a corresponding logical expression.
// One possibility the variation called HighLife devised by Nathan Thompson
// in 1994. It is identical to Conway's game of Life except it has the
// additional rule that a dead cell become alive if exactly six of its
// immediate neighbours are alive. You can try this version by commenting
// out IF FALSE DO below.

      IF FALSE DO
      { !q := ( cellsC & ((~A &  B & ~C)   | // Alive with 2 or 3 neighbours
                          ( A &  B & ~C) ))|
              (~cellsC & (( A &  B & ~C)   | // Dead with  3 or 6 neighbours
                          (~A &  B &  C) ))
      }

// Other experiments is the following

      IF FALSE DO
      { !q := ( cellsC & ((~A &  B & ~C)   | // Alive with 2 or 3 neighbours
                          ( A &  B & ~C) ))|
              (~cellsC & (( A &  B & ~C)   | // Dead with  3 or 5 neighbours
                          ( A & ~B &  C) ))
      }

      IF FALSE DO
      { !q := ( cellsC & ((~A &  B & ~C)   | // Alive with 2 or 3 neighbours
                          ( A &  B & ~C) ))|
              (~cellsC & (( A &  B & ~C)   | // Dead with  3,5 or 6 neighbours
                          ( A & ~B &  C)   |
                          (~A &  B &  C) ))
      }

      //IF tracing &
      //   (cellsNW | cellsN | cellsNE |
      //    cellsW  | cellsC | cellsE  |
      //    cellsSW | cellsS | cellsSE )~=0 DO
      IF tracing & 1<=i<=2 & 14 <= j <= 20 DO
      {
        writef("col=%i4   row=%i4*n", i, j)

        writef("%i2: %32b %32b %32b*n", j-1, p!(-2*rowwords-1), p!(-2*rowwords), p!(-2*rowwords+1))
        writef("%i2: %32b %32b %32b*n", j,   p!(  -rowwords-1), p!(  -rowwords), p!(  -rowwords+1))
        writef("%i2: %32b %32b %32b*n", j+1, p!(           -1), p!(          0), p!(           +1))
        newline()
        writef("cellsNW  %32b*n", cellsNW)
        writef("cellsN   %32b*n", cellsN)
        writef("cellsNE  %32b*n", cellsNE)
        writef("cellsW   %32b*n", cellsW)
        writef("cellsC   %32b*n", cellsC)
        writef("cellsE   %32b*n", cellsE)
        writef("cellsSW  %32b*n", cellsSW)
        writef("cellsS   %32b*n", cellsS)
        writef("cellsSE  %32b*n", cellsSE)
        newline()

        writef("A =      %32b*n", A)
        writef("B =      %32b*n", B)
        writef("C =      %32b*n", C)
        writef("D =      %32b*n", D)
        newline()
        writef("cellsC = %32b*n", cellsC)
        newline()
        writef("!q =     %32b*n", !q)
        abort(1003)
      }

      q := q+rowwords

      cellsNW, cellsN, cellsNE := cellsW,  cellsC, cellsE
      cellsW,  cellsC, cellsE  := cellsSW, cellsS, cellsSE
    }
  }

  { // Swap map1 and map2
    LET map = map1
    map1 := map2
    map2 := map
  }
}

AND display(map) BE
{
  fillsurf(col_lightgray)
  //updatescreen()   //Update the screen

  setcolour(col_black)

  FOR i = 1 TO xsize DO
    FOR j = 1 TO ysize DO
    { LET x = i - screencentrex
      LET y = j - screencentrey
      IF getbit(x,y) DO plotlivecell(x, y)
    }

  drawf(screencentrex-140, ysize-18, "Live cell count= %n",
        livecells(map1, mapupb))

  updatescreen()   //Update the screen
//abort(1006)
  IF delaymsecs DO sdldelay(delaymsecs)

}

AND plotlivecell(x,y) BE
{ x, y := screencentrex + 3*x, screencentrey - 3*y
  drawfillrect(x, y, x+1, y+1)
  //updatescreen()   //Update the screen
}

AND livecells(map, upb) = VALOF
{ // Return the total number of live cells
  LET res, bit = 0, 1
  LET counts = VEC 20 // For the longitudinal counts 
  FOR i = 0 TO 20 DO counts!i := 0
  FOR i = 0 TO upb DO increment(map!i, counts)
  FOR i = 0 TO 20 DO
  { res := res + bit * bits(counts!i)
    bit := bit + 1
  }
  RESULTIS res
}

AND increment(bits, p) BE
{ // Perform longitudinal increment
  LET w = !p
  LET carry = w & bits
  !p := w XOR bits
  UNLESS carry RETURN
  bits, p := carry, p+1
} REPEAT

AND bits(w) = w=0 -> 0, 1 + bits(w & (w-1))

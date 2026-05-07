/*

This is a demonstration of the well known A* algorithm for finding the
shortest path between two cells on a 2D grid in which some cells must
be avoided. Every step of the path must be one of the eight immediate
neighbours. The cost of moving diagonally is take to be 14 and
horizontally or vertically as 10, being approximately the distance
between the cell centres in arbitrary units.

Implemented in BCPL by Martin Richards (c) 3 Jan 2017

Usage: -m=msecs/n,-s=seed/n,-t=tracing/s,-d=dijkstra/s

-m/n      Delay time in msecs between steps.
-s/n      The random number seed, used to select the start and goal
          positions. Small values select some hand chosen positions.
-t/s      Turn on tracing.
-d/s      Perform Dijkstra's algorithm rather than A*

History

24/12/2016
Changed cell size from 5x5 to 9x9 so that backtracking arrow
are more visible.
*/

SECTION "sdllib"
GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the library source code
.
SECTION "astar"
GET "libhdr"
GET "sdl.h"

MANIFEST {
  Unvisited=0 // Undiscovered cell
  Fringe      // Dicovered cell still being evaluated
  Closed      // Evaluated cell
  Wall        // A cell blocking the path
  Path        // =TRUE if on the shortest path to the goal

  // Cell node selectors
  s_state=0   // = Unvisited, Fringe, Closed or Wall
  s_pos       // position of the cell in the vector areav
  s_frompos   // position of the best predecessor cell
  s_g         // The shortest path distance from the start cell
  s_f         // The g value + the shortest distance to the goal ignoring walls
  s_priqpos   // The position of this cell in the priority queue, or zero.

  s_size
  s_upb=s_size-1

  csize=9    // cells are now 9x9 (no longer 5x5)
}

GLOBAL {
  stdin:ug
  stdout
  tracing
  delaytime
  randseed
  dijkstra  // =TRUE if performing Dijkstra'a algorithm

  dijkstra_heuristic
  astar_heuristic
  heuristic

  spacevupb // The upper bound of spacev
  spacev    // Vector of free storage
  spacep    // Point to the most recent subvector allocated
  spacet    // The limit of spacev
  areav     // A 2D array of cell nodes

  xsize     // Number of cells per row, somewhat less than screenxsize
  ysize     // Number of cells per column

  priq      // The heap structure used to represent the priority queue
  priqn     // The number of cells in the priority queue
  priqnmax  // The largest value of priqn used
  priqupb   // The maximum possible number of cells in the priority queue

  getleast  // Function to extract the cell with the least f value from
            // the priority queue
  insert    // Insert a cell into the priority queue
  positioncell  // (cell, p) Reposition the cell at position p since
                // its f value has just been reduces.
  chkpriq   // Check that the priority queue structure is valid.
  prpriq    // Output the priority queue showing its structure.
  prindent  // Output the indent character (used by prpriq)

  newvec    // Allocate space from spacev
  newcell   // Allocate a cell node

  startcell
  goalcell

  position  // (x,y) calculate the position of a cell in areav
  allocarea
  plotarea
  cellcolour

  findshortestpath
  neighbourcost
  drawwall
  plotcell

  col_black
  col_blue
  col_green
  col_yellow
  col_red
  col_majenta
  col_cyan
  col_white
  col_darkgray
  col_darkblue
  col_darkgreen
  col_darkyellow
  col_darkred
  col_darkmajenta
  col_darkcyan
  col_gray
  col_lightgray
  col_lightblue
  col_lightgreen
  col_lightyellow
  col_lightred
  col_lightmajenta
  col_lightcyan
}

LET start() = VALOF
{ LET argv = VEC 50

  spacev, priq := 0, 0

  UNLESS rdargs("-m=msecs/n,-s=seed/n,-t=tracing/s,-d=dijkstra/s",
                argv, 50) DO
  { writef("Bad arguments for astar*n")
    GOTO fin
  }

  delaytime := 0  // msecs
  randseed  := 0  // Used to select the start and goal positions

  IF argv!0 DO delaytime := !argv!0  // -m=msecs/n
  IF argv!1 DO randseed  := !argv!1  // -s=seed/n
  tracing := argv!2                  // -t=tracing/s
  dijkstra := argv!3                 // -d=dijkstra/s

  IF tracing DO writef("delaytime=%7.3d randseed=%n dijkstra=%n*n",
                        delaytime, randseed, dijkstra)

  spacevupb := 30_000
  spacev := getvec(spacevupb)
  priqupb := 500
  priq := getvec(priqupb)

  initsdl()

  TEST dijkstra
  THEN { mkscreen("Dijkstra's Algorithm Demo", 550, 550)
         heuristic := dijkstra_heuristic
       }
  ELSE { mkscreen("A** Algorithm Demo",        550, 550)
         heuristic := astar_heuristic
       }
  // The calls of mkscreen above sets screenxsize and
  // screenysize both to 550.

  xsize := screenxsize/csize - 5 // The number of cells in a row
  ysize := screenysize/csize - 5 // The number of cells in a column

  // Define some colours
  col_black       := maprgb(  0,   0,   0)
  col_blue        := maprgb(  0,   0, 255)
  col_green       := maprgb(  0, 255,   0)
  col_yellow      := maprgb(  0, 255, 255)
  col_red         := maprgb(255,   0,   0)
  col_majenta     := maprgb(255,   0, 255)
  col_cyan        := maprgb(255, 255,   0)
  col_white       := maprgb(255, 255, 255)
  col_darkgray    := maprgb( 64,  64,  64)
  col_darkblue    := maprgb(  0,   0,  64)
  col_darkgreen   := maprgb(  0,  64,   0)
  col_darkyellow  := maprgb(  0,  64,  64)
  col_darkred     := maprgb(128,   0,   0)
  col_darkmajenta := maprgb( 64,   0,  64)
  col_darkcyan    := maprgb( 64,  64,   0)
  col_gray        := maprgb(128, 128, 128)
  col_lightblue   := maprgb(100, 100, 255)
  col_lightgreen  := maprgb(100, 255, 100)
  col_lightyellow := maprgb(128, 255, 255)
  col_lightred    := maprgb(255, 128, 128)
  col_lightmajenta:= maprgb(255, 128, 255)
  col_lightcyan   := maprgb(255, 255, 128)

  FOR i = 1 TO 50 DO // Perform 50 random tests
  { spacet := spacev + spacevupb
    spacep := spacet

    priqn := 0
    priqnmax := 0

    // Allocate areav and all cells
    // and also initialise the wall cells
    // Display the result.
    // Note that the other cells are displayed as they change.

    writef("*nSeed = %n*n", randseed)

    allocarea() // Initialise the area and place the walls.

    selectstartandgoal(randseed)

    plotcell(startcell)
    plotcell(goalcell)

    // Run the A* or Dijkstra algorithm
    findshortestpath(startcell, goalcell)

    writef("Space used %n out of %n*n", spacet-spacep, spacevupb)
    writef("Priority queue used %n out of %n*n", priqnmax, priqupb)

    sdldelay(10_000)  // Delay between tests to allow the solution
                      // to be viewed.

    randseed := randseed+1
  }

fin:
  closesdl()
  IF spacev DO freevec(spacev)
  IF priq   DO freevec(priq)

  writef("*nEnd of test*n")
  RESULTIS 0
}

AND prcell(cell) BE
{ writef("[%n (%i3,%i3) (%i3,%i3) %n+%n=%n]*n",
         s_state!cell,
         xcoord(s_pos!cell), ycoord(s_pos!cell),
         xcoord(s_frompos!cell), ycoord(s_frompos!cell),
         s_g!cell, heuristic(cell, goalcell), s_f!cell)
}

AND selectstartandgoal() BE
{ LET goalx,  goaly  = 0, 0
  LET startx, starty = 0, 0

  SWITCHON randseed INTO
  { DEFAULT: ENDCASE

    CASE 0:  startx, starty := 14, 22
             goalx,  goaly  := 52, 41
             ENDCASE
    CASE 1:  startx, starty := 15, 21
             goalx,  goaly  := 46, 40
             ENDCASE
    CASE 2:  startx, starty := 22, 42
             goalx,  goaly  := 36, 15
             ENDCASE
    CASE 3:  startx, starty := 47, 25
             goalx,  goaly  :=  5, 30
             ENDCASE
    CASE 4:  startx, starty := 30, 15
             goalx,  goaly  := 25, 53
             ENDCASE
    CASE 5:  startx, starty := 10, 45
             goalx,  goaly  := 38, 19
             ENDCASE
  }

  { LET pos = position(startx, starty)
    startcell := areav!pos
    // Ensure that the start cell is Unvisited.
    IF s_state!startcell = Unvisited BREAK
    startx,  starty  := randno(xsize-1), randno(ysize-1)
  } REPEAT

  { LET pos = position(goalx, goaly)
    goalcell := areav!pos
    // Ensure that the goal cell is Unvisited
    // and not too close to the start cell.
    IF s_state!goalcell = Unvisited &
       ABS(startx-goalx) + ABS(starty-goaly) > 40 BREAK
    goalx,  goaly  := randno(xsize-1), randno(ysize-1)
  } REPEAT

  writef("start=(%n,%n) goal=(%n,%n)  dist=%n*n",
          startx, starty,
          goalx,  goaly,
          ABS(startx-goalx) + ABS(starty-goaly))
}

AND findshortestpath(startcell, goalcell) = VALOF
{ // Return FALSE if no path exists

  setseed(randseed)

  s_state!startcell := Fringe

  s_frompos!startcell := -1      // The start cell has no predecessor
  s_g!startcell := 0
  s_f!startcell := heuristic(startcell, goalcell)
  insert(startcell)  // Put it in the priority queue 

  plotcell(startcell)
  //chkpriq()                // Debugging check

  IF tracing DO
  { writef("*nStart cell becomes a fringe cell "); prcell(startcell)
    prpriq(1, 0, 0)
    abort(1000)
  }


  { // Start of main loop
    LET currentcell = getleast()

    UNLESS currentcell DO
    { writef("The goal cannot be reached from the start cell*n")
      RESULTIS FALSE // No path exists
    }

    IF currentcell=goalcell DO
    { writef("Shortest path found*n")
      createpath(goalcell, startcell)
      RESULTIS TRUE
    }

    // Close the current cell
    s_state!currentcell := Closed
    plotcell(currentcell)
    //chkpriq()                // Debugging check

    // Look at the 8 immediate neigbours of the current cell

    { LET pos = s_pos!currentcell
      LET g   = s_g!currentcell
      LET tg, newf = ?, ?

      FOR dx = -1 TO 1 FOR dy = -1 TO 1 UNLESS dx=0=dy DO
      { LET npos = pos + dy*xsize + dx // Position of an immediate neighbour
        LET cell = areav!npos
        LET state = s_state!cell

        // Ignore neighbours that are walls or are already evaluated
        IF state = Closed | state = Wall LOOP

        tg := g + neighbourcost(dx, dy)

        IF state = Unvisited DO
        { s_state!cell := Fringe // Make this cell a Fringe cell
          s_g!cell := tg
          s_f!cell := tg + heuristic(cell, goalcell)
          s_frompos!cell := pos

          insert(cell)  // Insert this cell into the priority queue
          plotcell(cell)
          //chkpriq()                // Debugging check
          IF tracing DO
          { writef("*nNew fringe cell created "); prcell(cell)
            prpriq(1, 0, 0)
            abort(1000)
          }
          sdldelay(delaytime)
          LOOP
        }

        UNLESS state = Fringe DO
        { writef("Sytem error: this cell should be a Fringe cell "); prcell(cell)
          abort(999)
        }

        IF tg >= s_g!cell LOOP  // There is already a cheaper route

        // We have found a shorter route to this Fringe cell
        s_frompos!cell := pos
        s_g!cell := tg
        s_f!cell := tg + heuristic(cell, goalcell)
        positioncell(cell, s_priqpos!cell) // Re-position cell in the queue
        plotcell(cell)
        //chkpriq()                // Debugging check

        IF tracing DO
        { writef("*nf value of a fringe cell decreased "); prcell(cell)
          prpriq(1, 0, 0)
          abort(1000)
        }

        sdldelay(delaytime)
        // Consider the next neighbour, if any
      }
    }
    // Deal with another Fringe cell
  } REPEAT
}

AND createpath(p, q) BE
{ UNTIL p=q DO
  { p := areav!(s_frompos!p)
    s_state!p := Path
    plotcell(p)
  }
}

AND newvec(upb) = VALOF
{ LET p = spacep - upb - 1
  IF p < spacev DO
  { writef("More space needed*n")
    abort(999)
    RESULTIS 0
  }
  spacep := p
  RESULTIS p
}

AND newcell() = VALOF
{ LET cell = newvec(s_upb)

  s_state!cell   := Unvisited
  s_pos!cell     := -1      // Not yet in the area
  s_frompos!cell := -1      // No from cell yet
  s_g!cell       := -1      // Not yet visited
  s_f!cell       := -1      // Unset value
  s_priqpos!cell :=  0      // Not in the priority queue

  RESULTIS cell
}

// Note that x and y are in the range 0 to xsize and 0 to ysize.
AND position(x, y) = y * xsize + x
AND xcoord(pos)    = pos MOD xsize
AND ycoord(pos)    = pos  /  xsize

AND neighbourcost(dx, dy) = dx=0 | dy=0 -> 10, 14

AND allocarea() BE
{ // Allocate areav and create all cell node
  // and initialise the wall.
  // Finally display the area and its walls.
  // Note that the cells are displayed later as they change.

  spacep := spacet             // Allocate a brand new area
  areav := newvec(position(xsize-1, ysize-1))

  IF tracing DO
    writef("areav=%n upb=%n*n", position(xsize-1,xsize-1))

  FOR x = 0 TO xsize-1 FOR y = 0 TO ysize-1 DO
  { LET pos = position(x, y)
    LET cell = newcell()
    s_pos!cell := pos    // The position of this cell in areav
    s_frompos!cell := -1 // No from cell yet
    s_g!cell := -1       // g value unset
    s_f!cell := -1       // f value unset
    s_priqpos!cell := 0  // The cell is not in the priority queue
    areav!pos  := cell
  }

  fillsurf(col_gray)  // Fill the area background colour

  // Fill in the outside walls
  drawwall(  0,  0,  56,  1)  // Base wall
  drawwall(  0, 55,  56, 56)  // Top wall
  drawwall(  0,  1,   1, 56)  // Left wall
  drawwall( 55,  1,  56, 56)  // Right wall

  drawwall( 20, 47,  35, 48)  //          ##########
  drawwall( 20, 38,  21, 47)  //          #        #
  drawwall( 34, 38,  35, 47)  //          #        #
  drawwall( 20, 37,  35, 38)  //          ##########

  drawwall( 39, 34,  50, 35)  //                      ########
  drawwall( 49, 25,  50, 34)  //                             #
                              //                             #
                              //   ###############           #
  drawwall( 10, 26,  30, 27)  //                 #
  drawwall( 29, 18,  30, 26)  //                 #
  drawwall( 18, 17,  30, 18)  //        ##########

  drawwall( 12, 11,  50, 12)  //     #########################
  drawwall( 12,  5,  13, 11)  //     #                       #
  drawwall( 49,  5,  50, 11)  //     #                       #
  drawwall( 12,  4,  30,  5)  //     #############   #########
  drawwall( 34,  4,  50,  5)
}

AND drawwall(x1,y1, x2,y2) BE
{ // The coordinates are all in the range 0 to 56

  FOR x = x1 TO x2-1 FOR y = y1 TO y2-1 DO
  { LET cell = areav!position(x,y)
    IF cell<0 DO
    { writef("drawwall: System error x=%n y=%n*n", x, y)
      abort(999)
    }
    s_state!cell := Wall
    plotcell(cell)
  }
}

AND drawpoints(x, y, bits) BE
{ x := x+8
  WHILE bits DO
  { UNLESS (bits&1)=0 DO drawpoint(x, y)
    x, bits := x-1, bits>>1
  }
}

AND plotcell(cell) BE
{ LET pos = s_pos!cell
  LET x = xcoord(pos)
  LET y = ycoord(pos)
  LET dir = -1
  LET frompos = s_frompos!cell

  LET px = (screenxsize-csize*xsize)/2 + csize*x
  LET py = (screenysize-csize*ysize)/2 + csize*y

  LET col = cellcolour(cell)
  IF cell=startcell DO col := col_green
  IF cell=goalcell  DO col := col_red

  IF x > xsize | y > ysize DO
  { writef("plotcell: x=%n y=%n out of range*n", x, y)
    abort(999)
    RETURN
  }

  UNLESS s_state!cell=Path IF frompos>=0 DO
  { LET fx = xcoord(frompos)
    LET fy = ycoord(frompos)
    LET dx = fx - x
    LET dy = fy - y
    dir := (dy+1)*3 + dx + 1  //  dir        6 7 8
                              //  towards    3 4 5 
                              //  parent     0 1 2
  }
  setcolour(col)
  drawfillrect(px, py, px+(csize-2), py+(csize-2))
  setcolour(col_darkgray)
  UNLESS cell=goalcell SWITCHON dir INTO
  { DEFAULT:
    CASE -1:
      ENDCASE
    CASE 0:  //  Down left
      drawpoints(px, py+8, #b_0_0_0_0_0_0_0_0_0) //  8 + + + + + + + + +
      drawpoints(px, py+7, #b_0_0_0_1_1_0_0_0_0) //  7 + + + # # + + + +
      drawpoints(px, py+6, #b_0_0_0_1_1_0_0_0_0) //  6 + + + # # + + + +
      drawpoints(px, py+5, #b_0_0_0_1_1_0_0_0_0) //  5 + + + # # + + + +
      drawpoints(px, py+4, #b_0_0_1_1_1_1_1_1_0) //  4 + + # # # # # # +
      drawpoints(px, py+3, #b_0_0_1_1_1_1_1_1_0) //  3 + + # # # # # # +
      drawpoints(px, py+2, #b_0_1_1_1_1_0_0_0_0) //  2 + # # # # + + + +
      drawpoints(px, py+1, #b_0_1_1_0_0_0_0_0_0) //  1 + # # + + + + + +
      drawpoints(px, py+0, #b_1_0_0_0_0_0_0_0_0) //  0 # + + + + + + + +
      ENDCASE
    CASE 1:  //  Down
      drawpoints(px, py+8, #b_0_0_0_0_0_0_0_0_0) //  8 + + + + + + + + +
      drawpoints(px, py+7, #b_0_1_1_0_0_0_1_1_0) //  7 + # # + + + # # +
      drawpoints(px, py+6, #b_0_0_1_1_0_1_1_0_0) //  6 + + # # + # # + +
      drawpoints(px, py+5, #b_0_0_1_1_0_1_1_0_0) //  5 + + # # # # # + +
      drawpoints(px, py+4, #b_0_0_0_1_1_1_0_0_0) //  4 + + + # # # + + +
      drawpoints(px, py+3, #b_0_0_0_1_1_1_0_0_0) //  3 + + + # # # + + +
      drawpoints(px, py+2, #b_0_0_0_1_1_1_0_0_0) //  2 + + + # # # + + +
      drawpoints(px, py+1, #b_0_0_0_0_1_0_0_0_0) //  1 + + + + # + + + + 
      drawpoints(px, py+0, #b_0_0_0_0_1_0_0_0_0) //  0 + + + + # + + + +
      ENDCASE
    CASE 2:  //  Down right
      drawpoints(px, py+8, #b_0_0_0_0_0_0_0_0_0) //  8 + + + + + + + + +
      drawpoints(px, py+7, #b_0_0_0_0_1_1_0_0_0) //  7 + + + + # # + + + 
      drawpoints(px, py+6, #b_0_0_0_0_1_1_0_0_0) //  6 + + + + # # + + +
      drawpoints(px, py+5, #b_0_0_0_0_1_1_0_0_0) //  5 + + + + # # + + +
      drawpoints(px, py+4, #b_0_1_1_1_1_1_1_0_0) //  4 + # # # # # # + +
      drawpoints(px, py+3, #b_0_1_1_1_1_1_1_0_0) //  3 + # # # # # # + +
      drawpoints(px, py+2, #b_0_0_0_0_1_1_1_1_0) //  2 + + + + # # # # +
      drawpoints(px, py+1, #b_0_0_0_0_0_0_1_1_0) //  1 + + + + + + # # +
      drawpoints(px, py+0, #b_0_0_0_0_0_0_0_0_1) //  0 + + + + + + + + #
      ENDCASE
    CASE 3:  //  Left
      drawpoints(px, py+8, #b_0_0_0_0_0_0_0_0_0) //  8 + + + + + + + + +
      drawpoints(px, py+7, #b_0_0_0_0_0_0_0_1_0) //  7 + + + + + + + # +
      drawpoints(px, py+6, #b_0_0_0_0_0_1_1_1_0) //  6 + + + + + # # # +
      drawpoints(px, py+5, #b_0_0_1_1_1_1_1_0_0) //  5 + + # # # # # + +
      drawpoints(px, py+4, #b_1_1_1_1_1_1_0_0_0) //  4 # # # # # # + + +
      drawpoints(px, py+3, #b_0_0_1_1_1_1_1_0_0) //  3 + + # # # # # + +
      drawpoints(px, py+2, #b_0_0_0_0_0_1_1_1_0) //  2 + + + + + # # # +
      drawpoints(px, py+1, #b_0_0_0_0_0_0_0_1_0) //  1 + + + + + + + # +
      drawpoints(px, py+0, #b_0_0_0_0_0_0_0_0_0) //  0 + + + + + + + + +
      ENDCASE
    CASE 5:  //  Right
      drawpoints(px, py+8, #b_0_0_0_0_0_0_0_0_0) //  8 + + + + + + + + +
      drawpoints(px, py+7, #b_0_1_0_0_0_0_0_0_0) //  7 + # + + + + + + + 
      drawpoints(px, py+6, #b_0_1_1_1_0_0_0_0_0) //  6 + # # # + + + + +
      drawpoints(px, py+5, #b_0_0_1_1_1_1_1_0_0) //  5 + + # # # # # + +
      drawpoints(px, py+4, #b_0_0_0_1_1_1_1_1_1) //  4 + + + # # # # # #
      drawpoints(px, py+3, #b_0_0_1_1_1_1_1_0_0) //  3 + + # # # # # + +
      drawpoints(px, py+2, #b_0_1_1_1_0_0_0_0_0) //  2 + # # # + + + + +
      drawpoints(px, py+1, #b_0_1_0_0_0_0_0_0_0) //  1 + # + + + + + + +
      drawpoints(px, py+0, #b_0_0_0_0_0_0_0_0_0) //  0 + + + + + + + + +
      ENDCASE
    CASE 6:  //  Up left
      drawpoints(px, py+0, #b_1_0_0_0_0_0_0_0_0) //  8 # + + + + + + + +
      drawpoints(px, py+7, #b_0_1_1_0_0_0_0_0_0) //  7 + # # + + + + + +
      drawpoints(px, py+6, #b_0_1_1_1_1_0_0_0_0) //  6 + # # # # + + + +
      drawpoints(px, py+5, #b_0_0_1_1_1_1_1_1_0) //  5 + + # # # # # # +
      drawpoints(px, py+4, #b_0_0_1_1_1_1_1_1_0) //  4 + + # # # # # # +
      drawpoints(px, py+3, #b_0_0_0_1_1_0_0_0_0) //  3 + + + # # + + + +
      drawpoints(px, py+2, #b_0_0_0_1_1_0_0_0_0) //  2 + + + # # + + + +
      drawpoints(px, py+1, #b_0_0_0_1_1_0_0_0_0) //  1 + + + # # + + + +
      drawpoints(px, py+0, #b_0_0_0_0_0_0_0_0_0) //  0 + + + + + + + + +
      ENDCASE
    CASE 7:  //  Up
      drawpoints(px, py+8, #b_0_0_0_0_1_0_0_0_0) //  8 + + + + # + + + +
      drawpoints(px, py+7, #b_0_0_0_0_1_0_0_0_0) //  7 + + + + # + + + + 
      drawpoints(px, py+6, #b_0_0_0_1_1_1_0_0_0) //  6 + + + # # # + + +
      drawpoints(px, py+5, #b_0_0_0_1_1_1_0_0_0) //  5 + + + # # # + + +
      drawpoints(px, py+4, #b_0_0_0_1_1_1_0_0_0) //  4 + + + # # # + + +
      drawpoints(px, py+3, #b_0_0_1_1_1_1_1_0_0) //  3 + + # # # # # + +
      drawpoints(px, py+2, #b_0_0_1_1_0_1_1_0_0) //  2 + + # # + # # + +
      drawpoints(px, py+1, #b_0_1_1_0_0_0_1_1_0) //  1 + # # + + + # # +
      drawpoints(px, py+0, #b_0_0_0_0_0_0_0_0_0) //  0 + + + + + + + + +
      ENDCASE
    CASE 8:  //  Up right
      drawpoints(px, py+8, #b_0_0_0_0_0_0_0_0_1) //  8 + + + + + + + + #
      drawpoints(px, py+7, #b_0_0_0_0_0_0_1_1_0) //  7 + + + + + + # # + 
      drawpoints(px, py+6, #b_0_0_0_0_1_1_1_1_0) //  6 + + + + # # # # +
      drawpoints(px, py+5, #b_0_1_1_1_1_1_1_0_0) //  5 + # # # # # # + +
      drawpoints(px, py+4, #b_0_1_1_1_1_1_1_0_0) //  4 + # # # # # # + +
      drawpoints(px, py+3, #b_0_0_0_0_1_1_0_0_0) //  3 + + + + # # + + +
      drawpoints(px, py+2, #b_0_0_0_0_1_1_0_0_0) //  2 + + + + # # + + +
      drawpoints(px, py+1, #b_0_0_0_0_1_1_0_0_0) //  1 + + + + # # + + +
      drawpoints(px, py+0, #b_0_0_0_0_0_0_0_0_0) //  0 + + + + + + + + +
      ENDCASE
  }

  updatescreen()
}

AND cellcolour(cell) = VALOF SWITCHON s_state!cell INTO
{ DEFAULT:        RESULTIS col_darkred

  CASE Unvisited: RESULTIS col_gray
  CASE Closed:    RESULTIS col_lightblue
  CASE Fringe:    RESULTIS col_white
  CASE Wall:      RESULTIS col_black
  CASE Path:      RESULTIS col_lightmajenta
}

AND getleast() = priqn=0 -> 0, VALOF
{ // Extract the cell with the least f value from
  // the priority queue. Return 0 if the queue is empty.
  LET p = 1
  LET mincell = priq!1  // The cell with the least f value in the queue
  LET cell = priq!priqn // The last cell of priq
  LET cellf = s_f!cell  // Its f value

  s_priqpos!mincell := 0 // Not in the priority queue anymore.

  // Decrease the size of the priority queue
  priqn := priqn-1

  // Insert cell into the priority queue knowing that
  // element at position 1 is empty

  { LET smallerchild, smallerf = ?, ?
    LET q = p+p
    // Position p in the queue is now empty

    IF q > priqn BREAK // The cell at position p has no children.

    // There is at least one child
    smallerchild := priq!q        // The first child cell
    smallerf := s_f!smallerchild 

    IF q < priqn DO
    { // There is a second child
      LET child2 = priq!(q+1)
      LET child2f = s_f!child2
      IF child2f < smallerf DO
      { // The second child has a smaller f value
        q := q+1
        smallerchild := child2
        smallerf := child2f
      }
    }

    // If the f value of cell is no larger than that of the smaller
    // child, break out of the loop.

    IF cellf <= smallerf BREAK

    // Move the smaller child one level towards the root, and
    // set p to the position of the new hole.

    priq!p := smallerchild
    s_priqpos!smallerchild := p
    p := q // p is now the position where the smaller child was.
  } REPEAT

  priq!p := cell
  s_priqpos!cell := p
  //chkpriq()            // A debugging aid

  RESULTIS mincell
}

AND insert(cell) BE
{ // Insert cell into the priority queue.

  // Increase the size of the priority queue.
  priqn := priqn+1
  IF priqn > priqupb DO
  { writef("Need a larger priority queue, priqn=%n priqupb=%n*n",
            priqn, priqupb)
    abort(999)
  }

  IF priqnmax < priqn DO priqnmax := priqn

  positioncell(cell, priqn)
}

AND positioncell(cell, p) BE
{ // Position p in the priority queue is empty. Insert cell
  // at the appropriate position between p and the root.
  LET f = s_f!cell

  WHILE p > 1 DO
  { // p is the position of an empty element in the queue
    LET q = p/2          // q is the position of its parent
    LET parent = priq!q  // This is the parent cell
    // Break out of the loop if the f value of the parent is no
    // larger than that of cell.
    IF s_f!parent <= f BREAK
    priq!p := parent // Move the parent one level further from the root
    s_priqpos!parent := p
    p := q
  }

  priq!p := cell      // Insert cell in its new position
  s_priqpos!cell := p // Set its new position in the cell node.
  //chkpriq()            // A debugging aid
}
 
AND chkpriq() BE
{ FOR i = 1 TO priqn DO
  { LET parent = priq!i
    LET f = s_f!parent
    LET priqpos = s_priqpos!parent
    LET q = i+i       // The position of the first child if it exists.

    // Check the cell's state and priqpos value.
    UNLESS s_state!parent=Fringe & priqpos = i DO
    { writef("Error at %i3: priqpos=%n in cell ", i, s_priqpos!parent)
      prcell(parent)
      prpriq(1, 0, 0)
      abort(999)
    }

    IF q<=priqn DO
    { LET childf = s_f!(priq!q)
      UNLESS f <= childf DO
      { writef("Parent at position %n and child at %n have f values %n and %n*n",
                i, q, f, childf)
        prpriq(1, 0, 0)
        abort(999)
      }
    }

    IF q+1<=priqn DO
    { LET childf = s_f!(priq!(q+1))
      UNLESS f <= childf DO
      { writef("Parent at position %n and child at %n have f values %n and %n*n",
                i, q+1, f, childf)
        prpriq(1, 0, 0)
        abort(999)
      }
    }
  }
}

AND prpriq(p, depth, indentbits) BE IF p<=priqn DO 
{ // This function outputs the priority queue.
  // The output includes an indication of the binary heap
  // structure to assist debugging of the priority queue code.
  // If tracing is TRUE, prpriq(1, 0, 0) is called every
  // time the priority queue is modified.
  LET cell = priq!p
  LET f = s_f!cell
  LET pos = s_pos!cell
  LET x, y = xcoord(pos), ycoord(pos)
  LET q = p+p      // The position of the first child, if any.
  writef("%n         at (%n,%n)  p=%n*n", f, x, y, p)

  IF q<=priqn DO   // Output the first child tree
  { prindent(depth, indentbits)
    writef("**--")
    prpriq(q, depth+1, indentbits<<1 | 1)
  }

  IF q+1<=priqn DO  // Output the second child tree
  { prindent(depth, indentbits)
    writef("**--")
    prpriq(q+1, depth+1, indentbits<<1)
  }
}

AND prindent(depth, bits) BE IF depth>0 DO
{ prindent(depth-1, bits>>1)
  writes((bits&1)=0 -> "   ", "|  ")
}

AND astar_heuristic(cell1, cell2) = VALOF
{ LET pos1 = s_pos!cell1
  LET pos2 = s_pos!cell2
  LET dx = ABS(pos1 MOD xsize - pos2 MOD xsize)
  LET dy = ABS(pos1  /  xsize - pos2  /  xsize)

  // Assuming dx>=0 and dy>=0 and dx>dy, return the cost of a path
  // consisting of (dx-dy) steps in the x direction followed by dy
  // steps diagonally towards the goal, giving a cost of
  // 10*(dx-dy)+14*dy = 10*dx+4*dy
  // The calculation is similar for other directions.
  IF dx>=dy RESULTIS 10*dx + 4*dy
  RESULTIS 10*dy + 4*dx
}

AND dijkstra_heuristic(dx, dy) = 0
                            

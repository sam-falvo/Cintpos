/*

This program attempts to obtain the best winning strategy for the pig
dice game on the assumption that it depends only on the current turn
score, the own score and the opponent score. It is based on
pigstrategy.b multiprecision numbers allowing 16 decimal digits after
the decimal point. This is to check that the surprising optimal
strategy was a due to the limited precision of the original program.


Implemented by Martin Richards (c) March 2014

History

21/10/2018
Many bugs corrected.

08/10/2018
Updated to display the strategy cube as it is being calculated.

Usage:  pigstrategy "lim/n,-t/s,-p/s"
        lim is the size of the cube, typically 100 (the dafault)
        -t     Turns on tracing
        -p     Dispays the cube using the SDL graphics library
               while the stategy is being calculated.
               This option is under development.
        -f     Generate the files pigcube.txt and pigstrat.txt

All the scores are positive and less than 100 so there are one million
possible game states corresponding to the grid points in a 100x100x100
cube. The optimum strategy can be represented by setting flags for
every game state indicating whether to play the die or hold. The grid
points of this cube is represented by a million word array called
cube. The game state opponentscore, ownscore and turnscore is held in
cube!i where

       i = opponentscore*100000+ownscore*100+turnscore

The element cube!i holds (prob<<1 | flag), where prob is the
probability of winning and flag=1 if the best strategy is to hold.
The probabilities are represented using scaled numbers with 8 decimal
digits after the decimal point.

Initially all elements of cube hold a probability of 0.0 and a flag of
one representing HOLD.  The program then repeated recomputes all the
elements of the cube until no further improvement is possible.

At the end, if -f is given, the program outputs the file pigcube.txt
giving the probability and flag of every element of the cube, and the
file pigstrat.txt giving the turn score on which the hold for each
opponent and own score. If the command argument -d is not given, the
format of this file is suitable to be read in by the program pig.b,
allowing it to play the game with the optimum strategy.

*/

SECTION "sdllib"
GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the library source code
.
SECTION "pigstrategy"
GET "libhdr"
GET "sdl.h"

MANIFEST {
  tstop=-1; tstmy=-1; tstts=-1 // Trace all states
  tstop=25; tstmy=21; tstts=10 // Trace only this state
  //tstop=96; tstmy=97; tstts= 0 // Trace only this state
  //tstop=2; tstmy=0; tstts= 1 // Trace only this state

  lim = 100         // Normally = 100, the goal of the game
  limsq = lim*lim
  cubeupb = lim*lim*lim-1
}

GLOBAL {
  stdout:ug
  tracing
  plotting      // Draw the strategy cube as it is being computed.
  filing        // Generate the files pigcube.txt and pigstrat.txt
  One0          // Normally 1_000_000_00
  One1          // Next 8 digits
  cube0         // To hold 100x100x100 values of prob<<1 | flag
                // The vector holds digits 1 to 8 after the decimal point.
  cube1         // This vector holds digit 9 to 16 after the decimal point
  maxchange0
  maxchange1
  newchange0
  newchange1
  prevmaxchange0
  prevmaxchange1
  playcount
  playchanges
  itercount
  p1x; p1y; p2x; p2y; tx; ty
  ox; oy
  colourv       // Colors for turn score betwee0 and 100
  col_red
  col_green
  col_darkblue
  col_white
  col_leftside    // Colour of the left surface of a column
  col_frontside   // Colout of the front surface of a column

  // Functions
  plotcube
  pr16
  add
  sub
  divby6
  less
}

LET testarith() BE
{ LET a, b = 100000000, 0
  LET c, d = 0, 0

  pr16(a, b); newline()
  c := divby6(a,b)
  d := result2
  writef("Divide by 6*n")
  pr16(c, d); newline()

  a, b := 99999999, 99999999
  pr16(a, b); newline()
  c := divby6(a,b)
  d := result2
  writef("Divide by 6*n")
  pr16(c, d); newline()

  a, b := 0, 3
  pr16(a, b); newline()
  c := divby6(a,b)
  d := result2
  writef("Divide by 6*n")
  pr16(c, d); newline()

  a, b := 100000000, 0
  c, d := 0, 1
  pr16(a, b); writef(" - "); pr16(c, d)
  c := sub(a,b, c,d)
  d := result2
  writef(" = "); pr16(c, d); newline()

  a, b := 0, 99999999
  c, d := 0, 1
  pr16(a, b); writef(" + "); pr16(c, d)
  c := add(a,b, c,d)
  d := result2
  writef(" = "); pr16(c, d); newline()
}

LET start() = VALOF
{ LET argv = VEC 50

  //testarith()
  //RESULTIS 0

  itercount := 0

  stdout := output()

  UNLESS rdargs("-t/s,-p/s,-f/s", argv, 50) DO
  { writef("Bad arguments for pigstrategy*n")
    RESULTIS 0
  }


  tracing  := argv!0              // -t/s
  plotting := argv!1              // -p/s
  filing   := argv!2              // -f/s

  UNLESS filing DO plotting := TRUE

  One0 := 100_000_000     // Using scaled arithmetic for probabilities
                          // with first 8 digits after the decimal point.
  One1 := 0               // The next 8 digits

  cube0 := getvec(cubeupb)
  cube1 := getvec(cubeupb)

  UNLESS cube0 & cube1 DO
  { writef("More space needed*n")
    RESULTIS 0
  }

  FOR i = 0 TO cubeupb DO cube0!i, cube1!i := 0+1, 0 // = 0.0<<1 | 1

  colourv := 0

  IF plotting DO
  { UNLESS sys(Sys_sdl, sdl_avail) DO
    { writef("*nUnable to display the cube since SDL is not available*n")
      RESULTIS 0
    }
    initsdl()
    mkscreen("Pig Dice Game Strategy Cube", 750, 710)
updatescreen()
//abort(3000)

    colourv := getvec(100)
    UNLESS colourv DO
    { writef("More space needed for colourv*n")
      GOTO fin
    }
    FOR t = 0 TO 100 DO
    { LET r = ABS(t-15)*4
      LET g = ABS(t-30)*6
      LET b = ABS(t-50)*4

      IF (t & 1)=0 DO r, g, b := r + 25, g + 25, b + 30

      IF r>255 DO r := 255
      IF g>255 DO g := 255
      IF b>255 DO b := 255

      colourv!t  := maprgb(255-g,b,r)
    }

    col_leftside  := maprgb( 70, 70, 70)
    col_frontside := maprgb( 90, 90, 90)
    col_red       := maprgb(255,  0,  0)
    col_green     := maprgb(  0,255,  0)
    col_darkblue  := maprgb(  0,  0,180)
    col_white     := maprgb(255,255,255)
  }

  maxchange0, maxchange1 := 0, 0
  playcount, playchanges := 0, 0

  { prevmaxchange0 := maxchange0
    prevmaxchange1 := maxchange1

    IF plotting DO plotcube() 

    maxchange0, maxchange1 := 0, 0
    playcount, playchanges := 0, 0

    // Perform one iteration
    FOR op = lim-1 TO 0 BY -1 DO      // These loops are best done
      FOR my = lim-1 TO 0 BY -1 DO    // from high to low.
        FOR ts = lim-1 TO 0 BY -1 DO
        { IF tstop>=0 DO
            tracing := op=tstop & my=tstmy & ts=tstts -> TRUE, FALSE
          p(op, my, ts)
          //IF tracing DO abort(5555)
        }

    itercount := itercount+1
    writef("*n%i3: The largest change in probability of winning: %10.8d%8z*n",
            itercount, maxchange0, maxchange1)
    writef("     The number of states where PLAY is the best strategy: %n*n",
            playcount, playchanges)
    writef("     The number of states that were changed: %n*n",
            playchanges)

    IF FALSE & itercount>=1 DO
    { prstrategy()
      newline()
      BREAK
    }
  } REPEATWHILE (maxchange0 | maxchange1 | playchanges) & itercount<150

  IF plotting DO plotcube()
 
  IF filing DO { prstrategy(); newline() }

  writef("Optimum strategy found -- Checksum=%n*n", checksum())
  writef("Aborting -- Press C to finish*n")
  abort(1000)

fin:
  IF plotting DO closesdl()
  IF colourv DO freevec(colourv)
  IF cube0 DO freevec(cube0)
  IF cube1 DO freevec(cube1)
  RESULTIS 0
}

AND checksum() = VALOF
{ LET res = 0
  FOR i = 0 TO cubeupb DO res := res XOR (cube0!i>>1) XOR cube1!i
  RESULTIS res
}

AND p(op, my, ts) BE
{ // Calculate the probability of wining when in position (op,my,ts)
  // and store it and whether to PLAY or HOLD in cube0, cube1.
  LET i = op*limsq + my*lim + ts
  LET h0, h1 = ?, ?  // Prob of winning if HOLDing
  LET t0, t1 = ?, ?
  LET playprob0, playprob1 = ?, ?
  LET holdprob0, holdprob1 = ?, ?

  IF tracing DO writef("*nitercount: %n*n", itercount)

  IF tracing DO
  { writef("*nCalculating the probability of winning for (%n,%n,%n)*n", op,my,ts)
    writef("Previous value:   ")
    pr(op,my,ts)
    newline()
  }

  IF ts>=lim RETURN // The game has been won

  IF ts+my >= lim DO
  { // The game is won, so HOLD gives us a winning probability of 1.0.
    IF tracing DO writef("ts+my >= lim*n")
    newchange0 := sub(One0,One1, cube0!i>>1, cube1!i)
    newchange1 := result2
    IF less(maxchange0,maxchange1, newchange0,newchange1) DO 
      maxchange0, maxchange1 := newchange0, newchange1


    IF (cube0!i & 1) = 0 DO playchanges := playchanges+1

    cube0!i := One0<<1 | 1  // Prob 1.0  flag:Hold
    cube1!i := 0

    IF tracing DO
    { writef("Already won: ")
      pr(op, my, op)
      newline()
    }
    RETURN
  }

  IF tracing DO writef("my+ts < lim*n")

  IF ts>0 DO
  { // The player has a positive turn score but has not already won,
    // it may be better to HOLD than PLAY.
    IF tracing DO writef("ts>0*n")

    // Calculate the probability of winning if HOLDing now.
    IF FALSE & tracing DO
      writef("HOLD probability is 1.0 - p(%n,%n,%n)*n", my+ts, op, 0)
    t0 := cubeval(my+ts, op, 0)
    t1 := result2
    holdprob0 := sub(One0,One1, t0,t1)
    holdprob1 := result2

    IF tracing DO
    { writef("HOLD probability: ")
      prh(op,my,ts, holdprob0,holdprob1)
      newline()
    }

    IF FALSE & tracing DO
    { writef("Calculate PLAY probability, namely*n")
      writef("  ( 1.0 - p(%2i,%2i,%2i)*n", my, op,    0)
      writef("        + p(%2i,%2i,%2i)*n", op, my, ts+2)
      writef("        + p(%2i,%2i,%2i)*n", op, my, ts+3)
      writef("        + p(%2i,%2i,%2i)*n", op, my, ts+4)
      writef("        + p(%2i,%2i,%2i)*n", op, my, ts+5)
      writef("        + p(%2i,%2i,%2i)*n", op, my, ts+6)
      writef("  ) / 6*n")
    }
    
    // Calculate the probability of winning if PLAYing the die.
    t0 := cubeval(my, op,    0); t1 := result2   // PLAYed a 1
    playprob0 := sub(One0,One1, t0,t1);           playprob1 := result2
    t0 := cubeval(op, my, ts+2); t1 := result2   // PLAYed a 2
    playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
    t0 := cubeval(op, my, ts+3); t1 := result2   // PLAYed a 3
    playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
    t0 := cubeval(op, my, ts+4); t1 := result2   // PLAYed a 4
    playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
    t0 := cubeval(op, my, ts+5); t1 := result2   // PLAYed a 5
    playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
    t0 := cubeval(op, my, ts+6); t1 := result2   // PLAYed a 6
    playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2

    playprob0 := divby6(playprob0,playprob1);     playprob1 := result2

    IF tracing DO
    { writef("PLAY probability: ")
      prp(op,my,ts, playprob0,playprob1)
      newline()
writef("Calculated from:*n")
      pr(my+ts, op, 0); newline()
      pr(my, op, 0)
      pr(op, my, ts+2)
      pr(op, my, ts+3); newline()
      pr(op, my, ts+4)
      pr(op, my, ts+5)
      pr(op, my, ts+6); newline()
    }

    // holdprob is the probability of wining if we HOLD now
    // playprob is the probability of wining if we PLAY now
    TEST less(playprob0,playprob1, holdprob0,holdprob1)
    THEN { // It is definitely better to HOLD
           IF tracing DO writef("It is better to HOLD*n")

           TEST less(holdprob0,holdprob1, cube0!i>>1,cube1!i)
           THEN newchange0 := sub(cube0!i>>1,cube1!i, holdprob0,holdprob1)
           ELSE newchange0 := sub(holdprob0,holdprob1, cube0!i>>1,cube1!i)
           newchange1 := result2
           IF less(maxchange0,maxchange1, newchange0,newchange1) DO 
             maxchange0, maxchange1 := newchange0, newchange1

           IF tracing DO
           { // Remember ts > 0
             pr(my+ts, op, 0); newline()
             pr(my, op, 0)
             pr(op, my, ts+2)
             pr(op, my, ts+3); newline()
             pr(op, my, ts+4)
             pr(op, my, ts+5)
             pr(op, my, ts+6); newline()
             writef("(%i2 %i2 %i2):%10.8d%8zH %10.8d%8zP => %10.8d%8zH => ",
                     op, my, ts, holdprob0,holdprob1, playprob0,playprob1,
                                 holdprob0,holdprob1)
           }

           IF (cube0!i & 1) = 0 DO
             playchanges := playchanges+1 // PLAY changing to HOLD
           cube0!i := holdprob0<<1 | 1    // holdprob  flag:Hold
           cube1!i := holdprob1
           RETURN
         }
    ELSE { // It is better the PLAY
           IF tracing DO writef("It is better to PLAY*n")
           TEST less(playprob0,playprob1, cube0!i>>1,cube1!i)
           THEN newchange0 := sub(cube0!i>>1,cube1!i, playprob0,playprob1)
           ELSE newchange0 := sub(playprob0,playprob1, cube0!i>>1,cube1!i)
           newchange1 := result2

           IF less(maxchange0,maxchange1, newchange0,newchange1) DO 
             maxchange0, maxchange1 := newchange0, newchange1

           IF (cube0!i & 1) = 1 DO
             playchanges := playchanges+1 // HOLD changing to PLAY
           playcount := playcount+1

           IF tracing DO
           { pr(my+ts, op, 0); newline()
             pr(my, op, 0)
             pr(op, my, ts+2)
             pr(op, my, ts+3); newline()
             pr(op, my, ts+4)
             pr(op, my, ts+5)
             pr(op, my, ts+6); newline()
             writef("(%i2 %i2 %i2):%10.8d%8zH %10.8d%8zP => %10.8d%8zP => ",
                     op, my, ts, holdprob0,holdprob1, playprob0,playprob1,
                                                      playprob0,playprob1)
           }

           cube0!i := playprob0<<1        // playprob  flag:Play
           cube1!i := playprob1
           RETURN
         }
  }

  // The current turn score is zero, so PLAY
  IF tracing DO writef("ts=0*n")

  // Calculate the probability of winning if PLAYing the die.
  IF FALSE & tracing DO
  { writef("ts=0 so better to PLAY*n*n")
    writef("  ( 1.0 - p(%2i,%2i,%2i)*n", my, op,    0)
    writef("        + p(%2i,%2i,%2i)*n", op, my, ts+2)
    writef("        + p(%2i,%2i,%2i)*n", op, my, ts+3)
    writef("        + p(%2i,%2i,%2i)*n", op, my, ts+4)
    writef("        + p(%2i,%2i,%2i)*n", op, my, ts+5)
    writef("        + p(%2i,%2i,%2i)*n", op, my, ts+6)
    writef("  ) / 6*n")
  }

  // Calculate the probability of winning if PLAYing.

  t0 := cubeval(my, op,    0); t1 := result2   // PLAYed a 1
  playprob0 := sub(One0,One1, t0,t1);           playprob1 := result2
  //writef("PLAY 1 => "); pr16(playprob0,playprob1); newline()
  t0 := cubeval(op, my, ts+2); t1 := result2   // PLAYed a 2
  playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
  t0 := cubeval(op, my, ts+3); t1 := result2   // PLAYed a 3
  playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
  t0 := cubeval(op, my, ts+4); t1 := result2   // PLAYed a 4
  playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
  t0 := cubeval(op, my, ts+5); t1 := result2   // PLAYed a 5
  playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
  t0 := cubeval(op, my, ts+6); t1 := result2   // PLAYed a 6
  playprob0 := add(playprob0,playprob1, t0,t1); playprob1 := result2
  playprob0 := divby6(playprob0,playprob1);     playprob1 := result2

  // playprob is the probability of winning if we PLAY now
  IF tracing DO
  { writef("PLAY probability: ")
    prp(op,my,ts, playprob0,playprob1)
    newline()
  }

  TEST less(playprob0,playprob1, cube0!i>>1,cube1!i)
  THEN newchange0 := sub(cube0!i>>1,cube1!i, playprob0,playprob1)
  ELSE newchange0 := sub(playprob0,playprob1, cube0!i>>1,cube1!i)
  newchange1 := result2

  IF less(maxchange0,maxchange1, newchange0,newchange1) DO 
    maxchange0, maxchange1 := newchange0, newchange1

  IF (cube0!i & 1) = 1 DO
    playchanges := playchanges+1 // Changing HOLD to PLAY
  playcount := playcount+1

  IF FALSE & tracing DO
  { writef("Since ts=0 it is best to PLAY*n")
    pr(my, op, 0)
    pr(op, my, 2)
    pr(op, my, 3); newline()
    pr(op, my, 4)
    pr(op, my, 5)
    pr(op, my, 6); newline()
  }

  cube0!i := playprob0<<1        // playprob  flag:Play
  cube1!i := playprob1
  RETURN
}

AND cubeval(op, my, ts) = VALOF
{ LET i = op*limsq+my*lim+ts
  IF ts+my >= lim DO
  { result2 := One1
    RESULTIS One0
  }
  result2 := cube1!i
  RESULTIS cube0!i >> 1
}

AND prstrategy() BE
{ LET outfilename = "pigcubehd.txt"
  LET outstream = findoutput(outfilename)
  UNLESS outstream DO
  { writef("Trouble with file: %s*n", outfilename)
    RETURN
  }

  selectoutput(outstream)

  // Output the strategy with 8 digit precision
  FOR op = 0 TO lim-1 DO
    FOR my = 0 TO lim-1 DO
    { FOR ts = 0 TO lim-1 DO
      { LET i = op*limsq + my*lim + ts
        LET w = cube0!i
        LET prob = w>>1
        LET letter = (w & 1) = 0 -> 'P', 'H'
        IF ts MOD 5 = 0 DO
          writef("*n(%i3 %i3 %i3): ", op, my, ts)
        writef(" %10.8d%c", prob, letter)
      }
      newline()
    }
  endstream(outstream)
  outstream := 0
RETURN
  outfilename := "pigstrathd.txt"
  outstream := findoutput(outfilename)
  UNLESS outstream DO
  { writef("Trouble with file: %s*n", outfilename)
    RETURN
  }

  selectoutput(outstream)

//abort(6666)
  FOR op = 0 TO lim-1 DO
  { sawritef("*nOpponent score %i3", op)
    FOR my = 0 TO lim-1 DO
    { LET holdval = lim
      FOR ts = 0 TO lim-1 DO
      { LET i = op*limsq + my*lim + ts
          IF (cube0!i & 1) > 0 & holdval=lim DO holdval := ts
      }
      IF my MOD 10 = 0 DO
      { newline()
        writef("(%i3 %i3):  ", op, my)
      }
      writef(" %i3", holdval)
    }
    newline()
  }
  newline()
  endstream(outstream)
  selectoutput(stdout)
}

AND pr(op, my, ts) BE
{ LET i = op*limsq + my*lim + ts
  LET prob0, prob1 = One0<<1, One1
  IF ts<lim DO prob0, prob1 := cube0!i, cube1!i
  writef("(%i3 %i3 %i3):%10.8d%8z%c ",
            op, my, ts, prob0>>1, prob1, (prob0&1)=0 -> 'P', 'H')
}

AND prh(op, my, ts, p0,p1) BE
{   writef("(%i3 %i3 %i3):%10.8d%8zH ",
              op, my, ts, p0, p1)
}

AND prp(op, my, ts, p0,p1) BE
{ writef("(%i3 %i3 %i3):%10.8d%8zP ",
            op, my, ts, p0, p1)
}

AND prdiff(op, my, ts, diff0,diff1) BE //IF diff DO
{ pr(op, my, ts)
  writef("diff=%10.8d%8z*n", diff0,diff1)
}

AND plotaxes() BE
{
//writef("ox=%n oy=%n*n", ox, oy)
  setcolour(col_red)
  moveto(ox, oy)
  drawby(100*p1x, 100*p1y)
  moveto(ox+100*tx, oy+100*ty)
  drawby(100*p1x, 100*p1y)
  //moveto(ox+100*p2x, oy+100*p2y)
  //drawby(100*p1x, 100*p1y)
  moveto(ox+100*(p2x+tx), oy+100*(p2y+ty))
  drawby(100*p1x, 100*p1y)

  setcolour(col_green)
  moveto(ox, oy)
  drawby(100*p2x, 100*p2y)
  moveto(ox+100*tx, oy+100*ty)
  drawby(100*p2x, 100*p2y)
  moveto(ox+100*p1x, oy+100*p1y)
  drawby(100*p2x, 100*p2y)
  moveto(ox+100*(p1x+tx), oy+100*(p1y+ty))
  drawby(100*p2x, 100*p2y)

  setcolour(col_darkblue)
  moveto(ox, oy)
  drawby(100*tx, 100*ty)
  moveto(ox+100*p1x, oy+100*p1y)
  drawby(100*tx, 100*ty)
  moveto(ox+100*p2x, oy+100*p2y)
  drawby(100*tx, 100*ty)
  moveto(ox+100*(p1x+p2x), oy+100*(p1y+p2y))
  drawby(100*tx, 100*ty)
}

LET plotcube() BE
{ LET op, my, ts = ?, ?, ?
  LET p = 0
  LET k = 0

  p1x, p1y :=  5, 1
  p2x, p2y := -2, 2
  tx,  ty  :=  0, 4

  IF p1x < k DO k := p1x
  IF p2x < k DO k := p2x
  IF p1x+p2x < k DO k := p1x+p2x
  ox, oy := 20 - 100*k, 5
//writef("k=%n ox=%n oy=%n*n", k, ox, oy)
//writef("Calling fillsurf screenxsize=%n screenysize=%n*n", screenxsize, screenysize)
  fillsurf(maprgb(90, 70,255))
//abort(2345)
//sawritef("Calling plotaxes*n")
  plotaxes()

  setcolour(maprgb(255,255,255))
  drawf(420, 35, "Iteration                   %i8",    itercount)
  drawf(420, 20, "Max prob change  %11.8d%8z", maxchange0, maxchange1)
  drawf(420,  5, "Number of PLAY/HOLD changes %i8",    playchanges)

  updatescreen()
//abort(1001)


  FOR op = 99 TO 0 BY -1 DO
  { FOR my = 99 TO 0 BY -1 DO
    { LET tv = @cube0!(op*limsq + my*lim)
      LET p = 0 // Position on the turn score axis
      ts := 0   // Position of the top of the last column
//sawritef("op=%i3 my=%i3*n", op, my)

      { WHILE p<100 & (tv!p & 1)=0 DO p := p+1
        // p is now the position of the top of a column of PLAYs
//sawritef("Calling plotcolumn*n")
        plotcolumn(op, my, ts, p-ts) // Plot PLAY column
        ts := p
        IF ts>=100 BREAK

        WHILE p<100 & (tv!p & 1)=1 DO p := p+1
        // p is now the position of the top of a clumn of PLAYs
        //plotcolumn(op, my, ts, p-ts) // Don't plot the HOLD column
        ts := p
        IF ts>=100 BREAK
      } REPEAT

      plotaxes()

      IF my=0 DO
        updatescreen()
    }
  }
}

AND plotcolumn(op, my, lo, h) BE //IF lo=0 DO
{ // The columns are drawn from furthest to nearest and from right to left
  // so that hidden surfaces are removed. 
  LET x0 = ox + my*p1x + op*p2x + lo*tx  //   +------+
  LET y0 = oy + my*p1y + op*p2y + lo*ty  //   |\     |\
  LET x1, y1 = x0+p1x, y0+p1y            //   | x------x
  LET x2, y2 = x0+p2x, y0+p2y            //   | |    | |
  LET x3, y3 = x0+p1x+p2x, y0+p1y+p2y    //   | |    | |
                                         //   | |    | |
  LET tx0, ty0 = x0+h*tx, y0+h*ty        //   | |    | |
  LET tx1, ty1 = x1+h*tx, y1+h*ty        //   1-|----2 |
  LET tx2, ty2 = x2+h*tx, y2+h*ty        //    \|     \|
  LET tx3, ty3 = x3+h*tx, y3+h*ty        //     0----- 3

  LET t = lo+h
  //LET r = ABS(t-15)*4
  //LET g = ABS(t-30)*6
  //LET b = ABS(t-50)*4

//writef("(%i3 %i3) column %i3 to %i3*n", op, my, lo, lo+h)
//abort(1234)
  //IF ((lo+h) & 1)=0 DO r, g, b := r + 25, g + 25, b + 30

  //IF r>255 DO r := 255
  //IF g>255 DO g := 255
  //IF b>255 DO b := 255

  playcount := playcount+h

  // Draw top
//writef("Draw top*n")
  //setcolour(maprgb(255-g,b,r))
  setcolour(colourv!t)
  drawquad(tx0,ty0, tx1,ty1, tx3,ty3, tx2,ty2)

  // Draw left side
//writef("Draw left side*n")
  //TEST TRUE | (my & 1)=0
  //THEN setcolour(maprgb(60,60,60))
  //ELSE setcolour(maprgb(70,70,70))
  setcolour(col_leftside)
  drawquad(tx0,ty0, x0,y0, x2,y2, tx2,ty2)

  // Draw front
//writef("Draw front side*n")
  //TEST TRUE | (op & 1)=0
  //THEN setcolour(maprgb(90,90-op/4,90))
  //ELSE setcolour(maprgb(99,99,99))
  setcolour(col_frontside)
  drawquad(tx0,ty0, x0,y0, x1,y1, tx1,ty1)

//  updatescreen()
}

AND less(a,b, c,d) = VALOF
{ LET res = less1(a,b, c,d)
  //IF tracing DO
  //{ pr16(a,b); writef(" < "); pr16(c,d)
  //  writef(" => %n*n", res)
  //}
  chk16(a,b)
  chk16(c,d)
  RESULTIS res
}

AND less1(a,b, c,d) = VALOF
{ // Return TRUE if the positive 16 digit number a,b is
  // less than the positive 16 digit number c,d.
  IF a < c RESULTIS TRUE
  IF a=c RESULTIS b < d
  RESULTIS FALSE
}

AND add(a,b, c,d) = VALOF
{ LET r0 = add1(a,b, c,d)
  LET r1 = result2
  //IF tracing DO
  //{ pr16(a,b); writef(" + "); pr16(c,d)
  //  writef(" => "); pr16(r0,r1); newline()
  //}
  chk16(a,b)
  chk16(c,d)
  chk16(r0,r1)
  result2 := r1
  RESULTIS r0
}

AND add1(x0,x1, y0,y1) = VALOF
{ // Add the 16 digits x and y together, returning the senior digits
  // as the result, and the next 8 digits
  LET r0, r1 = x0+y0, x1+y1
  r0 := r0 + r1  /  100_000_000 // Carry if necessary
  result2 := r1 MOD 100_000_000 
  RESULTIS r0
}

AND sub(a,b, c,d) = VALOF
{ LET r0 = sub1(a,b, c,d)
  LET r1 = result2
  //IF tracing DO
  //{ pr16(a,b); writef(" - "); pr16(c,d)
  //  writef(" => "); pr16(r0,r1); newline()
  //}
  chk16(a,b)
  chk16(c,d)
  chk16(r0,r1)
  result2 := r1
  RESULTIS r0
}

AND sub1(x0,x1, y0,y1) = VALOF
{ // Subtract positive the 16 digits y from x, returning the
  // senior digits as the result, and the next 8 digits.
  // The result is assumed to be positive.
  LET r0, r1 = x0-y0, x1-y1
  IF r1<0 DO r0, r1 := r0-1, r1+100_000_000
  result2 := r1
  RESULTIS r0
}

AND divby6(a,b) = VALOF
{ LET r0 = divby6x(a,b)
  LET r1 = result2
  //IF tracing DO
  //{ pr16(a,b); writef(" / 6 => ")
  //  pr16(r0,r1); newline()
  //}
  chk16(a,b)
  chk16(r0,r1)
  result2 := r1
  RESULTIS r0
}

AND divby6x(x0,x1) = VALOF
{ // Divide the positive the 16 digits x by 6, returning the
  // senior digits as the result, and the next 8 digits.
  // The result is assumed to be positive.
  LET r0, r1 = x0, x1+3
  IF r1 >= 100_000_000 DO r0, r1 := r0+1, r1-100_000_000
  //IF tracing DO writef("divby6: %10.8d%8z + 3 in pos 16 => %10.8d%8z*n", x0,x1, r0,r1)
  r1 := (r1 + (r0 MOD 6) * 100_000_000) / 6
  r0 := r0 / 6
  result2 := r1
  RESULTIS r0
}

AND chk16(a, b) BE
{ IF 0 <= a < 1_000_000_000 &
     0 <= b <   100_000_000 RETURN
  writef("ch16: ERROR a=%10.8d b=%8z*n", a, b)
  abort(999)
}
AND pr16(x0, x1) BE writef("%10.8d%8z", x0, x1)


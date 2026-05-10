/*

This is a simple bat and ball game. The aim is to three balls into a
bucket with the aid of a bat which must remain at ground level.


The file bucketnew.b is used for development and will replace
com/bucket.b when it works well enough.

Implemented by Martin Richards (c) February 2013

History:

13/08/2025
Checking the use of all variables relating to time in msecs.

07/07/2025
Added command arguments including to/K to allow debugging output to be
sent to a file, -m/N gives the the cyclecount when output to the
tofilename begins and -n/N giving the number of iteration steps to
perform before closing down.  The time spent executing debugging code
is accumulate in dbgmsecs making it possible to perform timing
calculations not corrupted by the time spent debugging code.

09/06/2025
Stopped damping of the bat motion when pushing is active, ie when
relms<pushtimelimit.

02/06/2025

Currently bucketnew.b uses scaled arithmetic with 3 decimal digits
after the decimal point. This change was needed to avoid overflow in
muldiv which occasionally happened. Possibly 4 fractional digits would
still avoid overflow and provide improved precision. Another possible
change is to used floating point instead of scaled arithmetic. Such a
version of bucket will be held in bucketflt.b.

28/05/2025
Made major change to use scaled arithmetic with 3 digits rather
than 5 everywhere particularely x-y coords, velocities and
accelerations. Time is unaffect since it is in msecs which can
be viewed a scaled with 3 digits after the decimal point when
viewed as secs.

23/09/2023
The computer's control of the bat is being modified.

15/09/2019
Modified to run under 32 and 64 bit BCPL on 32 and 64 bit
machines. All fields holding machine addresses are allocated
two BCPL words even though one word is often sufficient.

17/02/2013
Successfully reimplemented the first version to make it much more
efficient.  */

SECTION "sdllib"
GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the SDL library source code
.
SECTION "bucket"
GET "libhdr"
GET "sdl.h"

MANIFEST {
  One  =    1_000 // The constant 1.000 scaled with 3 decimal
                  // digits after the decimal point.
  OneK = 1000_000

  batradius       = 10_000
  ballradius      = 25_000
  endradius       = 15_000
  bucketthickness = 2 * endradius

  ag = -50_000 // Gravity acceleration, taken to be
               // 50 pixels per sec per sec downwards.
}

GLOBAL {
  done:ug         // Set to TRUE when no more cycles are needed.

  stdout
  stdin
  tofilename
  tostream
  
  stepping        // =FALSE if not stepping the ball or bat motion.

  cyclecount      // Initially zero and incremented every time a
                  // new cycle is started.

  mincyclecount   // Set by -m/N arg or zero if not set.
                  // This specified the cyclecount value that
		  // causes debugging output to tostream.
  maxcyclecount   // Set by -n/N arg or zero if not set,
                  // This is the cyclecount value that causes
		  // the program to terminate.

  activebat       // =TRUE when the computer controls the bat.

  debugging       // Display debugging values in the bucket window.
                  // It is set by the -d/S argument or the D command.
  showselected    // Indicate on the screen which ball is selected
                  // and the bouncex and pushx locations.
                  // It is set by the -v/S argument.

  help            // Display help information on the bucket window.
                  // It is set by the -h/S argument.

  finished        // All balls are now back in the bucket.
  finishedms        // The time when finished became TRUE

  started         // =TRUE when the base of the bucket is closed.
  startedchanged  // =TRUE whenever the value of started changes.
                  // If TRUE, at the start of a cycle startedms
		  // is set and  several other variables are
		  // initialised mostly to zero.

// Note: rtn_msec!rootnode is the number of msecs since midnight

  startedms    // This is set to the time in msecs since midnight
                  // at the start of the cycle just after the value of
		  // started was was changed.
		  // When startedchanges=TRUE it is set at the start
		  // of the next cycle.
                  // All other times are msecs relative to
		  // startedms less dbgmsecs.

  dbgmsecs        // This is the time spent in debugging code since
		  // since startedms was last set.

  cps             // An estimate of the number of cycles per second
                  // based on typically 10 completed cycles.
		  
  cyclestartms     // msecs since startedms less dbgmsecs when
                      // the current cycle was started.
  prevcyclestartms // Zero or the previous setting of
                      // cyclestartms.

  oldcyclestartms  // A previous value of cyclestartmsec typically
                      // when cyclcount was last a multiple of 10.
		      // It is used to estimate of how long a cycle
		      // takes, and is used to set cps, the number of
		      // cycles per second.

  mspercycle       // Estimated time per cycle of the main loop excluding
                      // debugging time. This is an average over recent cycles.
		      // It is only valid when non zero.
  
  stepdonems       // msecs from cyclestartms less dbgmsecs determined
                      // during the previous cycle.
  lastcyclems      // msecs used by the last cycle less dbgmsecs
  sumofstepdonems  // Sum of step msecs of recent cycles. Set to zero
                      // when cyclecount is a multiple of 10.

  relmsecs        // This is the current time in msecs since
                  // startedms less dbmsecs.

  pushtimelimit   // If non zero this is the time relative to
                  // startedms less dbgmsecs when a push will
		  // finish. During this time batxdotdot is set
		  // to pushaccel.

  pushaccel       // This is the bat acceleration if currently pushing.
                  // It is otherwis zero.
  
  bouncems     // Estimated time less dbgmsecs since startedms
                  // that the selected ball will hit the ground based on
		  // sely, selydot and the force of gravity (ag),
		  // It is zero if no ball is selected.

  bouncex         // Estimated bounce x position or midx if no ball
                  // selected.
  pushx           // Suggested starting position of a push.
  
  usage           // Estimated utilisation of the CPU as an integer
                  // percentage.
  
  safemuldiv      // (a,b,c) Ensures c>0 and that there is no overflow
  

  // All surfaces now need two BCPL words since machine
  // addresses may need 64 bits and the BCPL word length
  // may be only 32 bits. All functions that used to return
  // surfaces now take an extra argument pointer to a
  // surface pair. These functions return FALSE if
  // unsuccessful.
  // The variables ending in ok are TRUE if the corresponding
  // surface has been created.

  bucketwallsurf  // Surface for the bucket walls
  bucketwallsurf1 // Surface for the bucket walls
  bucketwallsurfok
  
  bucketbasesurf  // Surface for the bucket base
  bucketbasesurf1 // Surface for the bucket base
  bucketbasesurfok
  
  ball1surf       // Surfaces for the three balls
  ball1surf1      // Surfaces for the three balls
  ball1surfok
  
  ball2surf
  ball2surf1
  ball2surfok
  
  ball3surf
  ball3surf1
  ball3surfok
  
  batsurf         // Surface for the bat
  batsurf1        // Surface for the bat
  batsurfok

  backcolour      // Background colour
  bucketcolour
  bucketendcolour
  ball1colour
  ball2colour
  ball3colour
  batcolour

  wall_lx         // Left wall
  wall_rx         // Right wall
  floor_yt        // Floor
  ceiling_yb      // Ceiling
  midx
  midxby2

  bucket_lxl; bucket_lxc; bucket_lxr // Bucket left wall
  bucket_rxl; bucket_rxc; bucket_rxr // Bucket right wall
  bucket_tyb; bucket_tyc; bucket_tyt // Bucket top
  bucket_byb; bucket_byc; bucket_byt // Bucket base

  // Ball bounce limits allowing for the radius of the balls.
  xlim_lwall; xlim_rwall
  ylim_floor; ylim_ceiling
  xlim_bucket_ll; xlim_bucket_lc; xlim_bucket_lr 
  xlim_bucket_rl; xlim_bucket_rc; xlim_bucket_rr
  ylim_topt
  ylim_baseb; ylim_baset 
  ylim_bat

   // Positions, velocities and accelerations of the balls
  cgx1; cgy1; cgx1dot; cgy1dot; ax1; ay1
  cgx2; cgy2; cgx2dot; cgy2dot; ax2; ay2
  cgx3; cgy3; cgx3dot; cgy3dot; ax3; ay3

   // Position, velocity and acceleration of the bat
   // These must all be defined and live in consective globals
   // for the bouncing mechanism to work
  batx; baty; batxdot; batydot; batxdotdot

  currballno       // = 0 for unset otherwise 1, 2 or 3
  selx; sely       // The position of the selected ball
  selxdot; selydot // Velocity of the selected ball

  // The global functions
  
  dbgcall          // (f,arg) Call f(a,arg) accumulating the time
                   // while in this function in dbgmsecs.

  comploop         // (n) Execute a busy loop n times.

  dbgcomploop      // (n) Call comploop using ts
  dbgabort         // abort called using ts
  dbgdelay
  dbgwritef
  
  incontact        // (p1, p2, d)
  cbounce          // (p1, p2, m1, m2)
  rebound          // (vel)
  cosines          // (dx, dy)
  inprod           // (dx, dy, c, s)

  ballbounces      // (pv)

  prstate
  extractballparams
  selectaball
  isinbucket
  isabovebucket
  TorFstr      // Returns TRUE or FALSE

  step
  userbat
  
  initbucketwallsurf // (surfptr)
  initbucketbasesurf // (surfptr, col)
  initballsurf       // (surfptr, col)
  initbatsurf        // (surfptr)

  plotscreen         // ()
  resetbucket        // ()

  processevents      // ()

  sqrt               // (x)
  timetozero         // (x, v, a) msecs until x + vy + at^2/2 <= 0
  tst                // (x, v, a) test timetozero

  initgraphics
  initbucketgeometry
  initbucketstate

  cyclebody
}

// The next four function allow abort, comploop and abort
// to run accumulating in dbgmsecs how much time is spent.

LET dbgcall(fn, a, b, c, d, e, f, g, h) = VALOF
{ // Execute fn(a, b, c, d, e, f, g, h)
  // accumulating the time used in dbgmsecs.
  LET t0 = rtn_msecs!rootnode
  LET res = fn(a, b, c, d, e, f, g, h)
  dbgmsecs := dbgmsecs + rtn_msecs!rootnode - t0
  RESULTIS res
}

LET comploop(n) BE FOR i = 1 TO n DO
{ FOR i = 0 TO 50_000 LOOP
}

LET dbgcomploop(n) BE dbgcall(comploop, n)

LET dbgabort(n) BE dbgcall(abort, n)

LET dbgdelay(n) BE dbgcall(sdldelay, n)

LET dbgwritef(a,b,c,d,e,f,g,h) BE dbgcall(writef, a, b, c, d, e, f, g, h)

LET safemuldiv(a,b,c) = VALOF
{ // Calculate muldiv(a,b,c) avoiding overflow.
  LET x = ABS(a/#x10000)
  LET y = ABS(c/#x10000)
  LET z = ABS c
  IF c=0 DO
  { writef("ERROR: muldiv(%n, %n, %n)*n",a,b,c)
    writef("       The divisor must not be 0*n")
    abort(999)
  }
  IF muldiv(x,y,z) >= #x100_0000 DO
  { writef("ERROR: muldiv(%n, %n, %n) overflows*n",a,b,c)
    abort(999)
  }
  RESULTIS muldiv(a, b, c)
}

LET incontact(p1,p2, d) = VALOF
{ // p1 and p2 point to the coordinates of the centres of
  // two circles. It returns TRUE if they are less then
  // d units apart.
  LET x1, y1 = p1!0, p1!1 // Scaled with 3 digits after the decimal point
  LET x2, y2 = p2!0, p2!1
  LET dx, dy = x1-x2, y1-y2
  IF ABS dx > d | ABS dy > d RESULTIS FALSE // An optimisation
  RESULTIS safemuldiv(dx,dx,One) + safemuldiv(dy,dy,One) <=
           safemuldiv(d,d,One) RESULTIS FALSE
}

AND cbounce(p1, p2, m1, m2) BE
{ // p1!0 and p1!1 are the x and y coordinates of a ball, bat or bucket end.
  // p1!2 and p1!3 are the corresponding velocities
  // p2!0 and p2!1 are the x and y coordinates of a ball.
  // p2!2 and p2!3 are the corresponding velocities
  // m1 and m2 are the masses of the two objects in arbitrary units
  // m2 = 0 if p1 is a bucket end.
  // m1=m2  if the collition is between two balls
  // m1=5 and m2=1 is for collisions between the bat and ball assuming the bat
  // has five times the mass of the ball.

  LET c = cosines(p2!0-p1!0, p2!1-p1!1) // Direction p1 to p2
  LET s = result2
  //writef("c=%6.3d s=%6.3d*n", c, s)
  //abort(9998)

  IF m2=0 DO
  { // Object 1 is fixed, ie a bucket corner
    LET xdot = p2!2
    LET ydot = p2!3
    // Transform to (t,w) coordinates
    // where t is in the direction of the two centres
    LET tdot = inprod(xdot,ydot,  c, s)
    LET wdot = inprod(xdot,ydot, -s, c)

    IF tdot>0 RETURN

    // Object 2 is getting closer so reverse tdot (but not wdot)
    // and transform back to world (x,y) coordinates.
    tdot := rebound(tdot) // Reverse tdot with some loss of energy
    // Transform back to real world (x,y) coordinates
    p2!2 := inprod(tdot, wdot, c, -s)
    p2!3 := inprod(tdot, wdot, s,  c)

    RETURN
  }

  IF m1=m2 DO
  { // Objects 1 and 2 are both balls of equal mass
    // Find the velocity of the centre of gravity
    LET cgxdot = (p1!2+p2!2)/2
    LET cgydot = (p1!3+p2!3)/2
    // Calculate the velocity of object 1
    // relative to the centre of gravity
    LET rx1dot = p1!2 - cgxdot
    LET ry1dot = p1!3 - cgydot
    // Transform to (t,w) coordinates
    LET t1dot = inprod(rx1dot,ry1dot,  c,s)
    LET w1dot = inprod(rx1dot,ry1dot, -s,c)
//writef("rx1dot=%7.3d ry1dot=%7.3d*n", rx1dot, ry1dot)
//writef("t1dot =%7.3d w1dot =%7.3d*n", t1dot,  w1dot)
    IF t1dot<=0 RETURN

    // Reverse t1dot with some loss of energy
    t1dot := rebound(t1dot)
//writef("t1dot=%7.3d w1dot=%7.3d after bounce*n", t1dot,  w1dot)

    // Transform back to (x,y) coordinates relative to cg
    rx1dot := inprod(t1dot,w1dot,  c,-s)
    ry1dot := inprod(t1dot,w1dot,  s, c)
//writef("rx1dot=%7.3d ry1dot=%7.3d after bounce*n", rx1dot, ry1dot)

    // Convert to world (x,y) coordinates
    p1!2 :=  rx1dot + cgxdot
    p1!3 :=  ry1dot + cgydot
    p2!2 := -rx1dot + cgxdot
    p2!3 := -ry1dot + cgydot
//writef("p1!2=%7.3d p1!3=%7.3d p2!2=%7.3d p2!3=%7.3d*n",
//        p1!2,       p1!3,       p2!2,       p2!3)

    // Apply a small repulsive force between balls
    p1!0 := p1!0 - safemuldiv(0_400, c, One)
    p1!1 := p1!1 - safemuldiv(0_400, s, One)
    p2!0 := p2!0 + safemuldiv(0_400, c, One)
    p2!1 := p2!1 + safemuldiv(0_400, s, One)

    RETURN
  }

  { // Object 1 is the bat and object 2 is a ball
    // Find the velocity of the centre of gravity
    LET cgxdot = (p1!2*m1+p2!2*m2)/(m1+m2)
    LET cgydot = (p1!3*m1+p2!3*m2)/(m1+m2)
    // Calculate the velocities of the two objects
    // relative to the centre of gravity
    LET rx1dot = p1!2 - cgxdot
    LET ry1dot = p1!3 - cgydot
    LET rx2dot = p2!2 - cgxdot
    LET ry2dot = p2!3 - cgydot
    // Transform to (t,w) coordinates
    LET t1dot = inprod(rx1dot,ry1dot,  c,s)
    LET w1dot = inprod(rx1dot,ry1dot, -s,c)
    LET t2dot = inprod(rx2dot,ry2dot,  c,s)
    LET w2dot = inprod(rx2dot,ry2dot, -s,c)

    IF t1dot<=0 RETURN

    // Reverse t1dot and t2dot with some loss of energy
    t1dot := rebound(t1dot)
    t2dot := rebound(t2dot)

    // Transform back to (x,y) coordinates relative to cg
    rx1dot := inprod(t1dot,w1dot,  c,-s)
    ry1dot := inprod(t1dot,w1dot,  s, c)
    rx2dot := inprod(t2dot,w2dot,  c,-s)
    ry2dot := inprod(t2dot,w2dot,  s, c)

    // Convert to world (x,y) coordinates
    p1!2 := rx1dot + cgxdot
    p1!3 := ry1dot + cgydot // The bat cannot move vertically
    p2!2 := rx2dot + cgxdot
    p2!3 := ry2dot + cgydot

    // Apply a very small repulsive force
    p1!0 := p1!0 - safemuldiv(0_050, c, One)
    p1!1 := p1!1 - safemuldiv(0_050, s, One)
    p2!0 := p2!0 + safemuldiv(0_050, c, One)
    p2!1 := p2!1 + safemuldiv(0_050, s, One)

    RETURN
  }
}

AND rebound(vel) = vel/7 - vel // Returns the rebound speed of a bounce

AND cosines(dx, dy) = VALOF
{ LET d = ABS dx + ABS dy
  LET c = safemuldiv(dx, One, d)  // Approximate cos and sin
  LET s = safemuldiv(dy, One, d)  // Direction good, length not.
  LET a = safemuldiv(c,c,One)+safemuldiv(s,s,One) // 0.5 <= a <= 1.0
  d := One // With this initial guess only 3 iterations
           // of Newton-Raphson are required.
//writef("a=%8.3d  d=%8.3d  d^2=%8.3d*n", a, d, safemuldiv(d,d,One))
  d := (d + safemuldiv(a, One, d))/2
//writef("a=%8.3d  d=%8.3d  d^2=%8.3d*n", a, d, safemuldiv(d,d,One))
  d := (d + safemuldiv(a, One, d))/2
//writef("a=%8.3d  d=%8.3d  d^2=%8.3d*n", a, d, safemuldiv(d,d,One))
  d := (d + safemuldiv(a, One, d))/2
//writef("a=%8.3d  d=%8.3d  d^2=%8.3d*n", a, d, safemuldiv(d,d,One))

  s := safemuldiv(s, One, d) // Corrected cos and sin
  c := safemuldiv(c, One, d)
//writef("dx=%7.3d  dy=%7.3d => cos=%8.3d sin=%8.3d*n", dx, dy, c, s)

  result2 := s
  RESULTIS c
}

AND inprod(dx, dy, c, s) = safemuldiv(dx, c, One) + safemuldiv(dy, s, One)

AND ballbounces(pv) BE 
{ // Deal with a ball bouncing with the bucket, floor, ceiling, or walls.
  // pv points to a structure holding the position and velocity of
  // the ball.
  // This function does not deal with bounces with bat or other balls.
 
  LET cx, cy, vx, vy = pv!0, pv!1, pv!2, pv!3
  TEST xlim_bucket_ll <= cx <= xlim_bucket_rr &
       ylim_baseb     <= cy <= ylim_topt
  THEN { // The ball cannot be in contact with the floor, cieling or
         // either wall, so we only need to check for contact with
         // the bucket

         IF cy > bucket_tyc DO
         { LET ecx, ecy, evx, evy = bucket_lxc, bucket_tyc, 0, 0
           IF incontact(@ecx, pv, endradius+ballradius) DO
           { cbounce(@ecx, pv, 1, 0)
             // No other bounces possible
             RETURN
           }
           ecx := bucket_rxc
           IF incontact(@ecx, pv, endradius+ballradius) DO
           { cbounce(@ecx, pv, 1, 0)
             // No other bounces possible
             RETURN
           }
           // No other bounces possible
           RETURN
         }

         IF cy >= bucket_byc DO
         { // Possibly bouncing with bucket walls

           IF cx <= bucket_lxc DO
           { // Bounce with outside of bucket left wall
             pv!0 := xlim_bucket_ll
             IF vx>0 DO pv!2 := rebound(vx)
           }
           IF bucket_lxc < cx <= xlim_bucket_lr DO
           { // Bounce with inside of bucket left wall
             pv!0 := xlim_bucket_lr
             IF vx<0 DO pv!2 := rebound(vx)
           }
           IF xlim_bucket_rl <= cx < bucket_rxc DO
           { // Bounce with inside of bucket right wall
             pv!0 := xlim_bucket_rl
             IF vx>0 DO pv!2 := rebound(vx)
           }
           IF bucket_rxc < cx DO
           { // Bounce with outside of bucket right wall
             pv!0 := xlim_bucket_rr
             IF vx<0 DO pv!2 := rebound(vx)
           }
         }

         // Bounce with base
         IF started DO
         { // The bucket base is present
           IF bucket_lxc <= cx <= bucket_rxc DO
           {
             IF cy < bucket_byc DO
             { // Bounce on the outside of the base
               pv!1 := ylim_baseb
               IF vy>0 DO pv!3 := rebound(vy)
               // No other bounces are possible
               RETURN
             }
             IF bucket_byc <= cy <= ylim_baset DO
             { // Bounce on the top of the base
               pv!1 := ylim_baset
               IF vy<0 DO pv!3 := rebound(vy)
               // No other bounces are possible
               RETURN
             }
           }
         }

         // Bounces with the bottom corners
         IF cy < bucket_byc DO
         { LET ecx, ecy, evx, evy = bucket_lxc, bucket_byc, 0, 0
           IF incontact(@ecx, pv, endradius+ballradius) DO
           { // Bounce with bottom left corner
             cbounce(@ecx, pv, 1, 0)
             // No other bounces are possible
             RETURN
           }
           ecx := bucket_rxc
           IF incontact(@ecx, pv, endradius+ballradius) DO
           { // Bounce with bottom right corner
             cbounce(@ecx, pv, 1, 0)
             // No other bounces are possible
             RETURN
           }
         }
       }
  ELSE { // The ball can only be in contact with the bat, side walls,
         // ceiling or floor

         // Bouncing with the bat
         IF incontact(@batx, pv, batradius+ballradius) DO
         { pv!4, pv!5 := 0, 0
           cbounce(@batx, pv, 5, 1)
           batydot := 0 // Immediately damp out the bat's vertical motion
         }

         // Left wall bouncing
         IF cx <= xlim_lwall DO
         { pv!0 := xlim_lwall
           IF vx<0 DO pv!2 := rebound(vx)
         }

         // Right wall bouncing
         IF cx >= xlim_rwall DO
         { pv!0 := xlim_rwall
           IF vx>0 DO pv!2 := rebound(vx)
         }

         // Ceiling bouncing
         IF cy >= ylim_ceiling DO
         { pv!1 := ylim_ceiling
           IF vy>0 DO pv!3 := rebound(vy)
           // No other bounces are possible
           RETURN
         }

         // Floor bouncing
         IF cy <= ylim_floor DO
         { pv!1 := ylim_floor
	 //sawritef("bounces: ball in contact with the floor*n")
           IF vy<0 DO pv!3 := rebound(vy)
         }

         // No other bounces are possible
         RETURN
       }
}

AND prstate() BE
{ 
  //dbgwritef("cyclecount=%i4 activebat=%n*n",
  //           cyclecount,    activebat)
  //dbgwritef("started=%s     finished=%n*s",
  //           TorF(started), TorF(finished))
  //dbgwritef("cgx1=%7.3d cgy1=%7.3d cgx1dot=%7.3d cgy1dot=%7.3d*n",
  //           cgx1,      cgy1,      cgx1dot,      cgy1dot)
  //dbgwritef("cgx2=%7.3d cgy2=%7.3d cgx2dot=%7.3d cgy2dot=%7.3d*n",
  //           cgx2,      cgy2,      cgx2dot,      cgy2dot)
  //dbgwritef("cgx3=%7.3d cgy3=%7.3d cgx3dot=%7.3d cgy3dot=%7.3d*n",
  //           cgx3,      cgy3,      cgx3dot,      cgy3dot)
  IF FALSE DO
  IF currballno DO
  { dbgwritef("currballno=%n selx=%7.3d sely=%7.3d*n",
               currballno,   selx,      sely)
    dbgwritef("selxdot=%7.3d selydot=%7.3d*n",
               selxdot,      selydot)
  }
  //writef("ax1= %7.3d ax2= %7.3d ax3=    %7.3d*n", ax1, ax2, ax3)
  //writef("ay1= %7.3d ay2= %7.3d ay3=    %7.3d*n", ay1, ay2, ay3)
  //dbgwritef("batx=%7.3d batxdot=%7.3d batxdotdot=%7.3d*n",
  //           batx,      batxdot,      batxdotdot)
  IF started DO dbgwritef("relmsecs=%8.3d*n", relmsecs)
  IF pushtimelimit>0 DO
    dbgwritef("Continue pushing pushaccel=%7.3d pushtimelimit=% %8.3d*n",
                                pushaccel,      pushtimelimit)
  IF FALSE DO
  IF currballno DO
    dbgwritef("bouncems= %8.3d bouncex=%7.3d*n",
               bouncems,       bouncex)
  //delay(2_000)
  //dbgabort(3996)
}

LET extractballparams() BE SWITCHON currballno INTO
{ DEFAULT: writef("SYSTEM ERROR in extractballparams currballno=%n*n",
                                                     currballno)
           abort(999)

  CASE 0: selx, selxdot := 0,    0 // A ball will never be at (0,0)
          sely, selydot := 0,    0
          ENDCASE

  CASE 1: selx, selxdot := cgx1, cgx1dot
          sely, selydot := cgy1, cgy1dot
          ENDCASE
              
  CASE 2: selx, selxdot := cgx2, cgx2dot
          sely, selydot := cgy2, cgy2dot
          ENDCASE
              
  CASE 3: selx, selxdot := cgx3, cgx3dot
          sely, selydot := cgy3, cgy3dot
          ENDCASE
}

LET selectaball() BE
{ //writef("selectaball: entered*n")

  // No ball can be selected

  UNLESS started DO
  { currballno := 0
    RETURN
  }
  
  // Deselect the current ball if it is above the bucket
  currballno := VALOF SWITCHON currballno INTO
  { DEFAULT: writef("SYSTEM ERROR in selectaball, currballno=%n*n",
                                                  currballno)
             abort(999)
    CASE 0:  RESULTIS 0
    CASE 1:  IF isabovebucket(cgx1, cgy1) RESULTIS 0
    CASE 2:  IF isabovebucket(cgx2, cgy2) RESULTIS 0
    CASE 3:  IF isabovebucket(cgx3, cgy3) RESULTIS 0
  }
    
  // If there is no selected ball, try to find one that is
  // not above the bucket.
  //abort(6661)
  //UNLESS currballno DO writef("There is no selected ball so try to select one*n")

  currballno := VALOF
  { UNLESS currballno | isabovebucket(cgx1, cgy1) RESULTIS 1
    UNLESS currballno | isabovebucket(cgx2, cgy2) RESULTIS 2
    UNLESS currballno | isabovebucket(cgx3, cgy3) RESULTIS 3
    RESULTIS 0
  }

  //TEST currballno
  //THEN writef("Ball %n is selected*n", currballno)
  //ELSE writef("No ball selected*n")
//abort(1001)
}

AND isinbucket(x, y) = VALOF
{ IF ylim_baseb < y < bucket_tyt &
     bucket_lxc < x < bucket_rxc RESULTIS TRUE
  RESULTIS FALSE
}

AND isabovebucket(x, y) = VALOF
{ IF ylim_baseb < y              &
     bucket_lxc < x < bucket_rxc RESULTIS TRUE
  RESULTIS FALSE
}

LET TorFstr(b) = b -> "TRUE", "FALSE"

LET step() BE
{ // Move tha bat and balls based on ther current positions and
  // velocities, and deal with all possible collisions.
  
  //dbgwritef("%n: step entered started=%s finished=%s*n",
  //           cyclecount, TorFstr(started), TorFstr(finished))

//prstate()

  IF started UNLESS finished DO
    relmsecs := rtn_msecs!rootnode - // msecs since the
                startedms       - // started last became TRUE
		dbgmsecs             // less the dbgmsecs
		                     // Note: dbgmsecs is the time
				     // spent in debugging code since
				     // started last became TRUE.

  //writef("B:relmsecs=%7.3d*n", relmsecs)
  //dbgcomploop(1)
  
  // Check whether to set started to TRUE and close the bucket base
  UNLESS started UNLESS isinbucket(cgx1, cgy1) |
                        isinbucket(cgx2, cgy2) |
                        isinbucket(cgx3, cgy3) DO
  { //dbgwritef("started set to TRUE*n")
    // This causes the bucket base to be drawn.
    started       := TRUE
    finished      := FALSE
    pushaccel, pushtimelimit := 0, 0
    dbgmsecs      := 0
    startedms  := rtn_msecs!rootnode // msecs since midnight
  //dbgwritef("startedms=%7.3d since midnight*n", startedms)
    cyclestartms := 0
    prevcyclestartms := 0
  //dbgwritef("e:cyclestartms=%7.3d relative to startedms*n",
  //             cyclestartms)
    stepdonems     := 10 // Relative to cyclestartms
  //dbgwritef("e:stepdonems=     %7.3d relative to relcyclestartems*n",
  //             stepdonems)
    lastcyclems    := 20 // Relative to cyclestartms
                            // So initial usage = 50%
  //dbgwritef("e:lastcyclems=    %7.3d relative to relcyclestartems*n",
  //             lastcyclems)
    relmsecs          := 0  // Relative to startedms

    //dbgwritef("cgx1=%7.3d  cgy1=%7.3d*n", cgx1, cgy1)
    //dbgwritef("cgx2=%7.3d  cgy2=%7.3d*n", cgx2, cgy2)
    //dbgwritef("cgx3=%7.3d  cgy3=%7.3d*n", cgx3, cgy3)
    //dbgabort(6632)
  }


  // Ensure finished is TRUE if all three balls are safely in the bucket.
  UNLESS finished IF started  &
                     isinbucket(cgx1, cgy1) &
                     isinbucket(cgx2, cgy2) &
                     isinbucket(cgx3, cgy3) &
                     ABS cgy1dot < 2_000 &
                     ABS cgy2dot < 2_000 &
                     ABS cgy3dot < 2_000 DO
		     { finished := TRUE
		       finishedms := cyclestartms
		     }

//dbgwritef("step: Deal with ball motion*n")

  // Calculate the accelerations of the balls
  // Initialise with force of gravity
  ax1, ay1 := 0, ag
  ax2, ay2 := 0, ag
  ax3, ay3 := 0, ag

  // Add a little random horizontal motion
  ax1 := ax1 + randno(2001) - 1001
  ax2 := ax2 + randno(2001) - 1001
  ax3 := ax3 + randno(2001) - 1001

  // Deal with ball bounces with
  // the bucket, floor, ceiling or walls.
  ballbounces(@cgx1)
  ballbounces(@cgx2)
  ballbounces(@cgx3)

  // Ball on ball bounce
  IF incontact(@cgx1, @cgx2, ballradius+ballradius) DO
  { ay1, ay2 := 0, 0
    cbounce(@cgx1, @cgx2, 1, 1)
  }

  IF incontact(@cgx1, @cgx3, ballradius+ballradius) DO
  { ay1, ay3 := 0, 0
    cbounce(@cgx1, @cgx3, 1, 1)
  }

  IF incontact(@cgx2, @cgx3, ballradius+ballradius) DO
  { ay2, ay3 := 0, 0
    cbounce(@cgx2, @cgx3, 1, 1)
  }

  // Apply forces to the balls
  cgx1dot := cgx1dot + ax1/cps
  cgy1dot := cgy1dot + ay1/cps
  cgx2dot := cgx2dot + ax2/cps
  cgy2dot := cgy2dot + ay2/cps
  cgx3dot := cgx3dot + ax3/cps
  cgy3dot := cgy3dot + ay3/cps

  cgx1, cgy1 := cgx1 + cgx1dot/cps, cgy1 + cgy1dot/cps
  cgx2, cgy2 := cgx2 + cgx2dot/cps, cgy2 + cgy2dot/cps
  cgx3, cgy3 := cgx3 + cgx3dot/cps, cgy3 + cgy3dot/cps

  // Now choose haw to control the bat, setting the bat
  // velocity (batxdot) and acceleration (batxdotdot)
  // appropriately.

  TEST activebat THEN computerbat()
                 ELSE userbat()

  // Now move the bat based on batxdot and batxdotdot.
  
  //dbgwritef("Deal with bat motion*n")
  //dbgwritef("batx=%7.3d  batxdot=%7.3d  batxdotdot=%7.3d*n",
  //           batx,       batxdot,       batxdotdot)

  batxdot := batxdot + batxdotdot/cps
  
  // Limit batxdot to the range  -600_000 to +600_000
  IF batxdot> 600_000 DO batxdot :=  600_000
  IF batxdot<-600_000 DO batxdot := -600_000


  //dbgwritef("Add some attraction towards midx*n")
  //batxdot := batxdot + (midx-batx)/(3*cps)

  //dbgwritef("Add some damping to the bat motion*n")
  //batxdot := batxdot - muldiv(batxdot, 95, cps) / 100
  //dbgwritef("batxdot=%7.3d*n", batxdot)
  
  batx := batx + batxdot/cps

  // Deal with bat bounces off the left and right walls
  // The bat hiting a wall causes a push to end.
  IF batx > wall_rx-batradius DO
  { batx, batxdot := wall_rx - batradius, -batxdot
    pushaccel, pushtimelimit := 0, 0
  }
  IF batx < batradius DO
  { batx, batxdot := wall_lx + batradius, -batxdot
    pushaccel, pushtimelimit := 0, 0
  }
  //dbgwritef("=> batx=%7.3d*n", batx)

  // Ensure the bat remains at ground level.
  baty := floor_yt + batradius
}

AND computerbat() BE
{ // The computer will choose new values for
  // batxdotdot and batxdot based on the current state.

  //dbgwritef("computerbat entered*n")

  UNLESS started DO
  { //dbgwritef("Not started so move the bat towards midxby2*n")
    pushtimelimit, pushaccel := 0, 0
    batxdotdot := 0
    batxdot := muldiv((midxby2-batx)/50, -ag, 1_000) // At 15 units the
                                                     // acceleration is g.
    //dbgwritef("batx=%7.3d midxby2=%7.3d batxdot=%7.3d*n",
    //           batx,      midxby2,      batxdot)
    RETURN
  }

  // started is always TRUE when computerbat is called.

  IF finished DO
  { IF relmsecs < pushtimelimit DO
    { pushtimelimit, pushaccel := 0, 0
      dbgwritef("%n: Stop pushing since finished is TRUEd*n",
                 cyclecount)
    }
    RETURN
  }

  relmsecs := rtn_msecs!rootnode - startedms - dbgmsecs

  IF relmsecs < pushtimelimit DO
  { 
    //dbgwritef("%n: Pushing with pushaccel=%7.3d relmsecs=%7.3d  until %7.3d*n",
    //           cyclecount,      pushaccel,      relmsecs,  pushtimelimit)
    batxdotdot := pushaccel       
    RETURN
  }

  IF pushtimelimit DO
  { //dbgwritef("%n: Stop pushing*n", cyclecount)
    pushaccel, pushtimelimit := 0, 0
  }


  // Not currently pushing so select a suitable
  // acceleration for the bat.

//  dbgwritef("Not currently pushing, so try to select a ball*n")
  
  selectaball()
  // currballno will be 1, 2 or 3, pr 0 if no ball can be selected.

  IF currballno DO
  { // A ball has been selected.
    // Choose an acceleration based on its position and speed and
    // the position of the bat.
    
    LET pushms = 1_000
    // The computer action depends on whether the time for the
    // selected ball to hit the ground is greater than pushms.
    
    //dbgwritef("Ball %n selected*n", currballno)  
    extractballparams()

    //dbgwritef("selx=%7.3d sely=%7.3d ballradius=%7.3d*n",
    //           selx,      sely,      ballradius)
    //dbgwritef("selxdot=%7.3d selydot=%7.3d ag=%7.3d*n",
    //           selxdot,      selydot,      ag)

    // Set bouncems to the estimated time, since startedms
    // less dbgmsecs when the selected ball will hit the ground,
    // ignoring possible collisions. It is zero if there is no
    // selected ball.

    // Note:
    // timetozero(y, ydot, ydotdot) calculates the time for
    // a ball at height y, vertical velocity ydot and acceleration
    // ydotdot to hit the ground.

    TEST sely < ballradius + floor_yt + 2
    THEN { bouncems := relmsecs
         }
    ELSE { bouncems := relmsecs +
                   timetozero(sely-ballradius-floor_yt, selydot, ag)
           //dbgwritef("relmsecs=%7.3d timetozero(%7.3d,%7.3d,%7.3d)=%7.3d*n",
           //    relmsecs, sely-ballradius-floor_yt, selydot, ag,
	   //    bouncems-relmsecs)
           //dbgwritef("=> ")
         }
    //dbgwritef("bouncems=%7.3d*n", bouncems)
    //dbgdelay(2_000)

    // bouncems is the estimated time between now and when the
    // selected ball hits the floor.

    bouncex := selx + safemuldiv(selxdot, bouncems-relmsecs, 1_000)
    //writef("selx=%7.3d selxdot=%8.3d => bouncex=%8.3d*n",
    //        selx, selxdot,              bouncex)

    // Deal with possible wall bounces. 
    UNTIL wall_lx <= bouncex <= wall_rx DO
    { IF bouncex < xlim_lwall DO bouncex := xlim_lwall + xlim_lwall - bouncex
      IF bouncex > xlim_rwall DO bouncex := xlim_rwall + xlim_rwall - bouncex
      //dbgwritef("Deal with wall bounces, bouncex=%7.3d*n", bouncex)
    }

    // bouncex is the estimated x position of the bounce.

    // Choose pushx a distance of 4 ball diameters from bouncx.
    
    TEST bouncex < midx
    THEN pushx := bouncex + 8*ballradius
    ELSE pushx := bouncex - 8*ballradius

    //dbgwritef("Choose pushx = %7.3d pushms=%7.3d  4 ball diameters away*n",
    //                  pushx,        pushms)

    // pushx is the suggested starting position of a push
    
    //dbgabort(4415)
		    
    // A push will be started if the time from now to the bounce is less
    // pushms and the bat is at a sufficient distance from the bouncex.
    
//dbgwritef("bouncex=%7.3d batx=%7.3d bouncex-batx=%7.3d*n",
//           bouncex,      batx,      bouncex-batx)
//dbgwritef("bouncems=%7.3d pushms=%7.3d ABS(bouncex-batx)=%7.3d *
//          *4**ballradius=%7.3d*n",
//           bouncems,      pushms,      ABS(bouncex-batx),
//	   4*ballradius)
    IF bouncems < relmsecs + pushms DO
    { LET d = bouncex - batx
      TEST ABS(bouncex-batx) > 4*ballradius
      THEN { // We are not currently pushing and
             // he time to the bounce is less than pushms and
             // the bat is at a sufficient distance from bouncex, so
             // start a push towards bouncex to last for t secs where
             // t is bouncems-relmsecs with acceleration a.

             // The bat equation is: bouncex = batx + batxdot*t + a*t^2/2
	   
             // where t  = bouncems-relmsecs, time between now and the bounce
             // and   d  = bouncex - batx    Distance between the bat
	     //                              and bouncex
             // and   a  = the acceleration needed to cause
             //            the bat to reach the ball in time t.
             // so
             // a = (d - v*t) * 2 / t^2
	     // Ensure t is non zero
	     LET d  = bouncex - batx
             LET a = 0
             LET t = bouncems - relmsecs // Time from now to bounce
	     IF t < 0_500 DO
	     { dbgwritef("t=%7.3d too small so set to 0.500*n", t)
	       t := 0_500
	     }
	   
             // Calculate the acceleration needed
	     //       a = (d - batxdot*t) * 2 / (t*t)
             a := muldiv((d - muldiv(batxdot, t, 1_000)) * 2, 1_000,
                          muldiv(t, t, 1_000))
             // a = the acceleration to cause the bat to reach
             //     ball in time t

dbgwritef("bouncems is less than pushms and*n")
dbgwritef("the bat is more than 2 ball diameters from bouncex.########*n")
dbgwritef("so start a push.*n")
dbgwritef("batx=%7.3d bouncex=%7.3d*n", batx, bouncex)

dbgwritef("Calculate the push acceleration towards bouncex=%7.3d*n", bouncex)
dbgwritef("batx=%7.3d batxdot=%7.3d d=%7.3d t=time to hit=%7.3d*n",
           batx,      batxdot,      d,      t)
dbgwritef("     a = (d - batxdot**t) ** 2 / t^2*n")
dbgwritef("=> a=%7.3d*n", a)

             pushaccel     := a
             pushtimelimit := relmsecs + t + 0_150
dbgwritef("%n: Start pushing pushaccel=%7.3d relmsecs=%7.3d pushtimelimit=%7.3d*n",
           cyclecount,       pushaccel,      relmsecs,      pushtimelimit)
//dbgwritef("Time now is relmsecs=%7.3d relative to startedms*n",
//                       relmsecs)
dbgwritef("batx=%7.3d bouncex=%7.3d*n",
           batx,      bouncex)
             batxdot    := 0
             batxdotdot := 0
//sdldelay(10_000)
 	     //dbgdelay(1_000)
	     //abort(5812)
             RETURN
           }
      ELSE { 
//dbgwritef("Time to bounce is < pushms but the bat is too close to bouncex*n")
//dbgwritef("So choose batxdot to move the bat *
//          *from batx=%7.3d towards pushx=%7.3d*n",
//                batx,              pushx)
             IF pushtimelimit DO
             { dbgwritef("%n: Continue to pushing*n", cyclecount)
	       batxdotdot := pushaccel
	     }
             batxdotdot := 0
             batxdot := (pushx - batx) / 10

	     IF batxdot >  50_000 DO batxdot :=  50_000
	     IF batxdot < -50_000 DO batxdot := -50_000
//dbgwritef("Setting batxdot=%7.3d and batxdotdot=%7.3d*n",
//                   batxdot,          batxdotdot)

//IF cyclecount=147 DO abort(1000)
             RETURN
           }
    }

    // The time to the bounce is >= pushms so move the bat
    // toward pushx.
    batxdotdot := 0
//    dbgwritef("The time to bounce %7.3d is >= pushms=%7.3d, so*n",
//               bouncems-relmsecs,          pushms)
//    dbgwritef("Move the bat from %7.3d towards pushx=%7.3d*n",
//                                 batx,         pushx)
    batxdotdot := 0
    batxdot := (pushx - batx) // 10
    IF batxdot >  500_000 DO batxdot :=  500_000
    IF batxdot < -500_000 DO batxdot := -500_000
//    dbgwritef("Setting batxdot=%7.3d and batxdotdot=%7.3d*n",
//                       batxdot,          batxdotdot)
    RETURN
  }

//dbgwritef("No ball selected so move the bat from %7.3d towards midx=%7.3d*n",
//                                                 batx,         midx)
  batxdotdot := 0
  batxdot := (midx-batx) //* 50
  IF batxdot >  50_000 DO batxdot :=  50_000
  IF batxdot < -50_000 DO batxdot := -50_000
//dbgwritef("Setting batxdot = %7.3d and batxdotdot=%7.3d*n",
//                   batxdot,            batxdotdot)
  RETURN
}

AND userbat() BE
{ // The user can choose new values for batxdotdot and batxdot.
  // These changes are made by events processed by processevents.
  // All that userbat does is add a small attraction towards midx and
  // a little damping.
  TEST batx > midx THEN batxdot := batxdot - 100 
                   ELSE batxdot := batxdot + 100
  batxdot := muldiv(batxdot, 995, 1000)
  //dbgwritef("userbat: Adding a little attraction towards midx *
  //          *and some damping*n")
  //dbgwritef("batx=%7.3d batxdot=%7.3d batxdotdot=%7.3d*n",
  //           batx,      batxdot, batxdotdot)
}

AND initbucketwallsurf(surfptr) = VALOF
{ // Allocate a surface for the bucket walls
  LET width  = 2*endradius/One + 1
  LET height = (bucket_tyt - bucket_byb)/One + 2
  UNLESS mksurface(width, height, surfptr) RESULTIS FALSE
  selectsurface(surfptr, width, height)
  fillsurf(backcolour)

  // Draw the ends
  TEST debugging
  THEN setcolour(bucketendcolour)
  ELSE setcolour(bucketcolour)
  drawfillcircle(endradius/One, endradius/One, endradius/One-1)
  drawfillcircle(endradius/One, height-endradius/One, endradius/One-1)

  // Draw the wall
  setcolour(bucketcolour)
  drawfillrect(0, endradius/One, width, height-endradius/One)
  RESULTIS TRUE
}

AND initbucketbasesurf(surfptr, col) = VALOF
{ // Allocate the bucket base surface
  LET width  = (bucket_rxc - bucket_lxc)/One + 1
  LET height = 2*endradius/One + 1
  UNLESS mksurface(width, height, surfptr) RESULTIS FALSE
  selectsurface(surfptr, width, height)
  fillsurf(backcolour)
  setcolour(bucketcolour)
  drawfillrect(0, 0, width, height)
  RESULTIS TRUE
}

AND initballsurf(surfptr, col) = VALOF
{ // Allocate a ball surface
  LET height = 2*ballradius/One + 2
  LET width  = height
  LET colkey = maprgb(64,64,64)
  UNLESS mksurface(width, height, surfptr) RESULTIS FALSE

  selectsurface(surfptr, width, height)
  fillsurf(colkey)
  setcolourkey(surfptr, colkey)

  setcolour(col)
  drawfillcircle(ballradius/One, ballradius/One+1, ballradius/One)

  RESULTIS TRUE
}

AND initbatsurf(surfptr) = VALOF
{ // Allocate a bat surface
  LET height = 2*batradius/One + 2
  LET width  = height
  UNLESS mksurface(width, height, surfptr) RESULTIS FALSE
  selectsurface(surfptr, width, height)
  fillsurf(backcolour)
  setcolour(batcolour)
  drawfillcircle(batradius/One, batradius/One+1, batradius/One)
  RESULTIS TRUE
}

AND plotscreen() BE
{ selectsurface(@screen, screenxsize, screenysize)
  fillsurf(backcolour)

  // Initialise the surfaces if necessary
  UNLESS bucketwallsurfok DO
    bucketwallsurfok := initbucketwallsurf(@bucketwallsurf)
  UNLESS bucketbasesurfok DO
    bucketbasesurfok := initbucketbasesurf(@bucketbasesurf)
  UNLESS ball1surfok DO
    ball1surfok      := initballsurf(@ball1surf, ball1colour)
  UNLESS ball2surfok DO
    ball2surfok      := initballsurf(@ball2surf, ball2colour)
  UNLESS ball3surfok DO
    ball3surfok      := initballsurf(@ball3surf, ball3colour)
  UNLESS batsurfok DO
    batsurfok        := initbatsurf(@batsurf)

  // Bucket walls
  IF bucketwallsurfok DO
  { blitsurf(@bucketwallsurf, @screen, bucket_lxl/One, bucket_tyt/One)
    blitsurf(@bucketwallsurf, @screen, bucket_rxl/One, bucket_tyt/One)
  }
  
  // Bucket base
  IF bucketbasesurfok IF started DO
    blitsurf(@bucketbasesurf, @screen, bucket_lxc/One, bucket_byt/One-1)

  // The bat
  IF batsurfok DO
    blitsurf(@batsurf, @screen, (batx-batradius)/One, (baty+batradius)/One)

  // The three balls

  { IF ball1surfok DO
    { setcolour(ball1colour)
      blitsurf(@ball1surf, @screen, (cgx1-ballradius)/One, (cgy1+ballradius)/One)
    }
    IF ball2surfok DO
    { setcolour(ball2colour)
      blitsurf(@ball2surf, @screen,
                           (cgx2-ballradius)/One, (cgy2+ballradius)/One)
    }
    IF ball3surfok DO
    { setcolour(ball3colour)
      blitsurf(@ball3surf, @screen,
                           (cgx3-ballradius)/One, (cgy3+ballradius)/One)
    }
    IF showselected & activebat &  currballno DO
    { extractballparams()
      setcolour(#x00FFFFFF)
      drawfillrect(selx/One-5, sely/One-5, selx/One+5, sely/One+5)
    }
  }

  //IF FALSE DO
  UNLESS bouncex = 0 DO
  { setcolour(#xFFFFFF)

    IF FALSE DO
    IF bouncems>0 DO
    { drawf(450, 465, "bouncems=%7.3d", bouncems)
      drawf(450, 440, "bouncex    =%7.3d",     bouncex)
    }

    IF showselected & activebat & currballno DO
    { drawf(bouncex/One-6, 4, "o")
      IF bouncems<2_000 DO setcolour(#xFF00FF)
      drawf(pushx  /One-6, 4, "x")
    }
  }


  setcolour(maprgb(255,255,255))

  IF finished DO
    drawf(30, 300, "Finished -- Well Done!")

  //drawf(30, 450, "CPU usage = %i3%% cps = %i3 cyclecount=%i5",
  //                    usage,        cps,      cyclecount)

  IF FALSE DO
  IF bouncems DO
   drawf(30, 420, "time to bounce=%7.3d",
                   bouncems-relmsecs)

  IF FALSE DO
  IF pushtimelimit DO
  {  drawf(30, 390, "Pushing towards bouncex until %7.2d", pushtimelimit/10)
     drawf(30, 360, "pushaccel=%7.3d", pushaccel)
     drawf(30, 330, "Distance to go=%7.3d", bouncex-batx)
  }

  IF started DO
    drawf(30, 280, "Time %9.2d", (finished -> finishedms, relmsecs)/10)

  IF help DO
  { drawf(30, 160, "R     Reset")
    drawf(30, 140, "S     Start the game")
    drawf(30, 120, "P     Pause/Continue")
    drawf(30, 100, "H     Toggle help information")
    drawf(30,  80, "B     Toggle bat random motion")
    drawf(30,  60, "D     Toggle debugging")
    drawf(30,  40, "Q     Quit")
    drawf(30,  20, "<- -> Control the bat")
  }

  IF debugging DO
  { drawf(30, 220, "Ball1 x=%7.3d  y=%7.3d xdot=%7.3d  ydot=%7.3d",
          cgx1, cgy1, cgx1dot, cgy1dot)
    drawf(30, 205, "Ball2 x=%7.3d  y=%7.3d xdot=%7.3d  ydot=%7.3d",
          cgx2, cgy2, cgx2dot, cgy2dot)
    drawf(30, 190, "Ball3 x=%7.3d  y=%7.3d xdot=%7.3d  ydot=%7.3d",
          cgx3, cgy3, cgx3dot, cgy3dot)
    drawf(30, 175, "Bat   x=%7.3d  y=%7.3d xdot=%7.3d",
          batx, baty, batxdot)
  }
}

AND resetbucket() BE
{ // Set the initial positions
  cgx1, cgy1 := midx, bucket_byt+ballradius   + 10_000
  cgx2, cgy2 := midx, bucket_byt+3*ballradius + 20_000
  cgx3, cgy3 := midx, bucket_byt+5*ballradius + 30_000

  // Set the initial velocities
  cgx1dot, cgx2dot, cgx3dot :=  0, 0, 0
  cgy1dot, cgy2dot, cgy3dot :=  0, 0, 0

  pushaccel, pushtimelimit := 0, 0
dbgwritef("%n: Not pushing after resetting balls*n", cyclecount)

  started  := FALSE
  finished := FALSE
  finishedms := 0
  relmsecs := 0
  dbgmsecs := 0
  cyclestartms := 0
  prevcyclestartms := 0
  //dbgwritef("d:cyclestartms=%7.3d*n", cyclestartms)
  stepdonems     := 0
  //dbgwritef("d:stepdonems=%7.3d*n", stepdonems)
  lastcyclems    := 0
  //dbgwritef("d:lastcyclems=%7.3d*n", lastcyclems)
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ DEFAULT:
    LOOP

  CASE sdle_keydown:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:  LOOP

      CASE 'Q': done := TRUE
                LOOP

      CASE '?':
      CASE 'H': help := ~help
                LOOP

      CASE 'D': debugging := ~debugging
                IF bucketwallsurfok DO
                { freesurface(@bucketwallsurf)
                  bucketwallsurfok := FALSE
                }
                LOOP

      CASE 'B': activebat := ~activebat
                currballno := 0
                batxdotdot := 0
                LOOP

      CASE 'S': // Start again
                UNLESS ylim_baseb < cgy1 & bucket_lxc < cgx1 < bucket_rxc &
                       ylim_baseb < cgy2 & bucket_lxc < cgx2 < bucket_rxc &
                       ylim_baseb < cgy3 & bucket_lxc < cgx3 < bucket_rxc DO
                  resetbucket()
                started       := FALSE
                finished      := FALSE
                startedms  := 0     // Not yet started
  cyclestartms := 0
  prevcyclestartms := 0
  //dbgwritef("c:cyclestartms=%7.3d*n", cyclestartms)
  stepdonems     := 0
  //dbgwritef("c:stepdonems=%7.3d*n", stepdonems)
  lastcyclems    := 0
  //dbgwritef("c:lastcyclems=%7.3d*n", lastcyclems)
                relmsecs      := 0
		pushaccel, pushtimelimit := 0, 0
dbgwritef("%n: Not pushing after starting again*n", cyclecount)
                LOOP

      CASE 'P': // Toggle stepping
                stepping := ~stepping
                LOOP

      CASE 'V': // Show the selected ball
                showselected := ~ showselected
		LOOP

      CASE 'R': // Reset the balls
                resetbucket()
                started       := FALSE
                finished      := FALSE
                startedms  := 0      // Not yet started
  cyclestartms := 0
  prevcyclestartms := 0
  //dbgwritef("b:cyclestartms=%7.3d*n", cyclestartms)
  stepdonems     := 0
  //dbgwritef("b:stepdonems=%7.3d*n", stepdonems)
  lastcyclems    := 0
  //dbgwritef("b:lastcyclems=%7.3d*n", lastcyclems)
                relmsecs      := 0
		pushaccel, pushtimelimit := 0, 0
dbgwritef("%n: Not pushing after resetting ball*n", cyclecount)
                LOOP

      CASE sdle_arrowright:
                batxdot := batxdot + 200_000
		batxdotdot := 0
		activebat := FALSE
		//writef("Right arrow keydown, batxdot=%7.3d*n", batxdot)
		//abort(1129)
		LOOP

      CASE sdle_arrowleft:
                batxdot := batxdot - 200_000
		batxdotdot := 0
		activebat := FALSE
		//writef("Left arrow keydown, batxdot=%7.3d*n", batxdot)
		//abort(1128)
		LOOP
    }

  CASE sdle_keyup:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:  LOOP

      CASE sdle_arrowright:
		//writef("Right arrow keyup, batxdotdot=%7.3d*n", batxdotdot)
		//abort(1127)
		LOOP

      CASE sdle_arrowleft:
		//writef("Left arrow keyup, batxdotdot=%7.3d*n", batxdotdot)
		//abort(1126)
		LOOP
    }


  CASE sdle_quit:
    writef("QUIT*n");
    done := TRUE
    LOOP
}

LET sqrt(x) = VALOF
{ // x is scaled 5 digits after the decimal point
  LET FLT fx = FLOAT x / 1_000.0
  LET FLT fres = sys(Sys_flt, fl_sqrt, fx)
  LET res = FIX(fres * 1_000.0)
  RESULTIS res
}

LET timetozero(x, v, a) = VALOF
{ // x     Initial position, must be >=0
  // v     initial velocity
  // a     acceleration, must be negative
  // result is the time to reach zero
  // x, v and a are scaled fixed point with 5 digits fractioal digits
  // t is scaled fixed point with 3 digits fractioal digits
  // The equation is: s = x + vt + at^2/2
  // When s = 0
  // At^2 + Bt + C = 0
  // where A = a/2
  //       B = v
  //       C = x
  // The positive solution for t is
  // t = (-B - sqrt(B^2 - 4AC)) / 2A
  LET A   =  a/2
  LET B   =  v
  LET C   =  x>=0 -> x, 0
  LET B2  = safemuldiv(B, B, 1_000)
  LET AC4 = 4 * safemuldiv(A, C, One)
  LET t   = safemuldiv(-B - sqrt(B2 - AC4), One, 2*A)
  UNLESS a<0 DO
  { writef("timetozero: Bad argument  a=%7.3d*n", a)
    RETURN
  }
  
  //dbgwritef("x            = %12.3d*n", x)
  //dbgwritef("v            = %12.3d*n", v)
  //dbgwritef("a            = %12.3d*n", a)
  //dbgwritef("A            = %12.3d*n", A)
  //dbgwritef("B            = %12.3d*n", B)
  //dbgwritef("C            = %12.3d*n", C)
  //dbgwritef("B2           = %12.3d*n", B2)
  //dbgwritef("AC4          = %12.3d*n", AC4)
  //dbgwritef("B2-AC4       = %12.3d*n", B2-AC4)
  //dbgwritef("sqrt(B2-AC4) = %12.3d*n", sqrt(B2-AC4))
  //dbgwritef("t            = %12.3d*n", t)
  //dbgwritef("safemuldiv(-25_000, 328_968, 1_000)**4 = %12.3d*n",
  //        safemuldiv(-25_000, 328_968, 1_000)*4)
  //abort(1000)
  RESULTIS t // Scaled 3 digits after the decimal point
}

LET tst(x, v, a) BE
{ writef("x=%12.3d, v=%12.3d a=%12.3d => t = %12.3d*n",
          x,        v,       a, timetozero(x, v, a))
}

LET start() = VALOF
{ LET argv = VEC 50
  stdout := output()

  UNLESS rdargs("to/K,-m/N,-n/N,-b/S,-d/S,-v/S,-h/S", argv, 50) DO
  { writef("Bad arguments for bucket*n")
    RESULTIS 0
  }

  tofilename   := argv!0                  // to/K
  mincyclecount := 1
  IF argv!1 DO mincyclecount := !(argv!1) // -m/N
  maxcyclecount := 0
  IF argv!2 DO maxcyclecount := !(argv!2) // -n/N
  activebat    := argv!3                  // -a/S
  debugging    := argv!4                  // -d/S
  showselected := argv!5                  // -v/S
  help         := argv!6                  // -h/S

  tostream := 0
  IF tofilename DO
  { tostream := findoutput(tofilename)
    UNLESS tostream DO
    { writef("Trouble with file %s*n", tofilename)
      RESULTIS 0
    }
  }

  writef("*n*nbucketnew entered*n")

IF FALSE DO
{ writef("root 2 = %12.3d*n", sqrt(2_000))
  tst(16_000, 0, -32_000)
  tst(20_000, 0, -32_000)
  tst(64_000, 0, -32_000)
  abort(998)
}

  IF FALSE DO
  { // Code to test the cosines function
    LET e1, e2 = One, One
    FOR dy = 0 TO One BY One/100 DO
    { LET c, s, rsq = ?, ?, ?
      c := cosines(One, dy)
      s := result2
      rsq := safemuldiv(c,c,One) + safemuldiv(s,s,One)
      writef("dx=%7.3d  dy=%7.3d cos=%7.3d sin=%7.3d rsq=%7.3d*n",
              One, dy, c, s, rsq)
      IF e1 < rsq DO e1 := rsq
      IF e2 > rsq DO e2 := rsq
    }
    writef("Errors +%6.3d  -%7.3d*n", e1-One, One-e2)
    RESULTIS 0
  }

  UNLESS initgraphics() DO
  { writef("Unable toe initialiase graphics*n")
    GOTO fin
  }
  
  currballno     := 0
  
  stepping       := TRUE          // =FALSE if not stepping
  started        := FALSE
  startedchanged := TRUE
  finished       := FALSE
  
  cyclecount := 0
  bouncems, bouncex := 0, 0
  startedms     := 0
  //cyclems       := 0
  
  dbgmsecs     := 0          // msecs spent in debugging code since started
                             // the value of started was last changed.
			     // The bucket base is only open when started is
			     // FALSE.

  usage        := 0          // This is an estimate percentage CPU usage.
                             // It is only valid when non zero.

  startedms := 0       // This is only valid when started=TRUE.
                             // It is the number of msecs since midnight.

  oldcyclestartms := 0    // If non zero it is the value of startedms when
                             // was a multiple of 10. It its used to adjust cps.
  sumofstepdonems := 0    // Sum of step msecs of recent cycles. Set to zero
                             // when cyclecount is a multiple of 10.

  cyclestartms     :=  0  // msecs since startedms less dbgmsecs
  prevcyclestartms :=  0  // Time at start of previous cycle

				
  //dbgwritef("a:cyclestartms=%7.3d*n", cyclestartms)

  stepdonems     :=  0    // msecs since cyclestartms less dbgmsecs
  //dbgwritef("a:stepdonems=%7.3d*n", stepdonems)
  
  lastcyclems    :=  0    // msecs since cyclestartms less dbgmsecs

  relmsecs          :=  0    // Time relative to startedms
                             // less dbgmsecs, only valid when
			     // started is TRUE
  
  cps               := 50    // Initial setting

  mspercycle     := 0     // Only valid when non zero

  initbucketgeometry()

  initbucketstate()

  writef("Entering the cycle loop*n")

  done := FALSE
  
  UNTIL done DO
    cyclebody() // Repeatedly execute cycles
  

fin:
  writef("*nQuitting*n")
  sdldelay(1_000)

  IF bucketwallsurfok DO freesurface(@bucketwallsurf)
  IF bucketbasesurfok DO freesurface(@bucketbasesurf)
  IF ball1surfok      DO freesurface(@ball1surf)
  IF ball2surfok      DO freesurface(@ball2surf)
  IF ball3surfok      DO freesurface(@ball3surf)
  IF batsurfok        DO freesurface(@batsurf)

  closesdl()

  IF tostream DO
  { endstream(tostream)
    tostream := 0
    selectoutput(stdout)
  }
  
  RESULTIS 0
}

AND initgraphics() = VALOF
{ UNLESS sys(Sys_sdl, sdl_avail) DO
  { writef("*nThe SDL features are not available*n")
    RESULTIS FALSE
  }

  initsdl()
  mkscreen("Ball and Bucket", 800, 500)
  //mkscreen("Ball and Bucket", 600, 400)

  // Choose the colours
  
  backcolour      := maprgb(120,120,120)
  bucketcolour    := maprgb(170, 60,  30)
  bucketendcolour := maprgb(140, 30,  30)
  ball1colour     := maprgb(255,  0,   0)
  ball2colour     := maprgb(  0,255,   0)
  ball3colour     := maprgb(  0,  0, 255)
  batcolour       := maprgb( 40, 40,  40)

  bucketwallsurfok := FALSE
  bucketbasesurfok := FALSE
  ball1surfok      := FALSE
  ball2surfok      := FALSE
  ball3surfok      := FALSE
  batsurfok        := FALSE

  RESULTIS TRUE
}

AND initbucketgeometry() BE
{
/*
                        screen_xc
wall_lx                 midx                       wall_rx
|                       |                          |
|--------------------------------------------------|-- ceiling_yb   )
|                                                  |                ) ballradius
|----:----------------------------------------:----|-- ylim_ceiling )
|    :                                        :    |
|    :                                        :    |
|    :       bucket_lxl       bucket_rxl      :    |
|    :       |  bucket_lxc    |  bucket_rxc   :    |
|    :       |  |  bucket_lxr |  |  buckey_rxr:    |
|    :       |  |  |          |  |  |         :    |
|    :       |/###\|----------|/###\|---------:----|-- bucket_tyt
|    :   :   |#####|---:--:---|#####|---:-----:----|-- bucket_tyc
|    :   :   |#####|   :  :   |#####|   :     :    |
|    :   :   |#####|   :  :   |#####|   :     :    |
|    :   :   |#####|   :  :   |#####|   :     :    |
|    :   :   |#####|   :  :   |#####|   :     :    |
|    :   :   |#####|---:--:---|#####|---:-----:----|-- ylim_baset
|    :   :   |#####|##########|#####|---:-----:----|-- bucket_byt
|    :   :    \####################/----:-----:----|-- bucket_byc
|    :   :     --###############--------:-----:----|-- bucket_byb ) ballradius
|    :   :-------------:--:-------------:-----:----|-- ylim_baseb )
|    :   :             :  xlim_bucket_rl:     :    |
|    :   :             xlim_bucket_lr   :     :    |
|    :   :                              :     :    |
|    :   :                              :     :    |
|----:---:------------------------------:-----:----|-- ylim_floor )
|    :   :                              :     :    |              ) ballradius
|----:---:------------------------------:-----:----|-- floor_yb   )
|    :   :                              :     :    |
|    :   xlim_bucket_ll                 :     :    wall_rx
|    xlim_lwall                         :     xlim_rwall
wall_lx                                 xlim_bucket_rr
                                          
*/

  wall_lx := 0
  wall_rx := (screenxsize-1)*One      // Right wall

  //midx := (wall_lx+wall_rx)/2
  midx := screenxsize*One/2
  midxby2 := midx/2
  
  floor_yt   := 0                     // Floor
  ceiling_yb := (screenysize-1)*One   // Ceiling

  bucket_tyt := ceiling_yb - 6*ballradius
  bucket_tyc := bucket_tyt - endradius
  bucket_tyb := bucket_tyt - bucketthickness

  bucket_lxr := midx  - ballradius * 5 / 2
  bucket_lxc := bucket_lxr - endradius
  bucket_lxl := bucket_lxr - bucketthickness

  bucket_rxl := midx  + ballradius * 5 / 2
  bucket_rxc := bucket_rxl + endradius
  bucket_rxr := bucket_rxl + bucketthickness

  bucket_byt := bucket_tyt - 6*ballradius
  bucket_byc := bucket_byt - endradius
  bucket_byb := bucket_byt - bucketthickness

  xlim_lwall     := wall_lx    + ballradius
  xlim_rwall     := wall_rx    - ballradius
  ylim_floor     := floor_yt   + ballradius
  ylim_ceiling   := ceiling_yb - ballradius
  xlim_bucket_ll := bucket_lxl - ballradius
  xlim_bucket_lc := bucket_lxc - ballradius
  xlim_bucket_lr := bucket_lxr + ballradius
  xlim_bucket_rl := bucket_rxl - ballradius
  xlim_bucket_rc := bucket_rxc - ballradius
  xlim_bucket_rr := bucket_rxr + ballradius
  ylim_topt      := bucket_tyt + ballradius
  ylim_baseb     := bucket_byb - ballradius
  ylim_baset     := bucket_byt + ballradius
}

AND initbucketstate() BE
{ resetbucket()

  ax1, ay1 := 0, 0   // Acceleration of ball 1
  ax2, ay2 := 0, 0   // Acceleration of ball 2
  ax3, ay3 := 0, 0   // Acceleration of ball 3

  batx := midx/4  // Initial position of bat
  baty := floor_yt + batradius
  
  ylim_bat := floor_yt + batradius + ballradius

  batxdot, batydot := 150_000, 0 // Velocity of bat
  batxdotdot := 0                // Acceleration of bat

  bouncex     := midx
  pushx       := midx
  bouncems := 0
  
  pushtimelimit := 0  // Initially not pushing
  pushaccel     := 0
dbgwritef("%n: Initially not pushing*n", cyclecount)
  done := FALSE 

  startedms := 0

  // All other time are zero
  dbgmsecs   := 0
}

AND cyclebody() BE
{ // This is the body of the main loop.
  // Each execution increments cyclecount.

  IF startedchanged DO
  { // startedchanged is initially TRUE and is also set to TRUE every time
    // the value of started changes.
    startedchanged := FALSE

    // Initialise several variables because the value of started has changed.

    // Set startedms to the current number of msecs since midnight.
    startedms := rtn_msecs!rootnode
    
    // All other times are measured relative to startedms.

    dbgmsecs           := 0
    cyclestartms    := 0
    oldcyclestartms := 0
    sumofstepdonems := 0
  }

  prevcyclestartms := cyclestartms
  cyclestartms := rtn_msecs!rootnode - dbgmsecs - startedms
  // This is the time at the start of a cycle relative to startedms
  // less dbms.

  // If possible measurements made during the previous cycle are used
  // to adjust the variables usage amd cps.
//dbgwritef("%n: Start of cycle prevcyclestartms=%7.3d cyclestartms=%7.3d*n",
//           cyclecount,        prevcyclestartms, cyclestartms)

  IF stepdonems>0 & prevcyclestartms>0 DO
  { // These two variables have valid values.
    LET t10 = cyclestartms - oldcyclestartms
    // t10 is the time taken by the last 10 cycles

    // Estimate the CPU usage
    sumofstepdonems := sumofstepdonems +
                          stepdonems - prevcyclestartms

    IF cyclecount MOD 10 = 0 & t10 > 0 DO
    { LET u = 100 * sumofstepdonems / t10
      //dbgwritef("u=%i2  ", u)
      IF u <   5 DO u :=   5
      IF u > 100 DO u := 100
      IF u < usage DO usage := usage - 1
      IF u > usage DO usage := usage + 1

      // Now adjust cps based on the value of usage
      IF usage < 50 & cps < 90 DO cps := cps +1
      IF usage > 80 & cps > 10 DO cps := cps -1
      // cps will be between 10 and 90

      //dbgwritef("cyclecount=%n sumstepdonems=%7.3d  t10=%7.3d*n",
      //           cyclecount,   sumstepdonems,       t10)
      //dbgwritef("=> u=%i2  usage=%i3 cps=%i2*n", u, usage)
//dbgdelay(5000)
//abort(6991)
    }
  }
  
  IF cyclecount MOD 10 = 0 DO
  { IF oldcyclestartms DO
    { LET mspercycle = (cyclestartms - oldcyclestartms) / 10
      // Adjust cps (cycles per sec) based on the average of 10 recent
      // cycles.
      IF mspercycle DO cps := 1000 / mspercycle
      // Limit the range of possible cps values.
      IF cps <   5 DO cps :=   5
      IF cps > 100 DO cps := 100

      //dbgwritef("average mspercycle=%n => cps=%i3*n",
      //                   mspercycle,      cps)
    }
    oldcyclestartms := cyclestartms // A value when cyclecount
                                          // is a multiple of 10.
    sumofstepdonems := 0
  }

  
  // Its actions are as follows:

  // 1) It attemps to adjust cps the number of cycles per second
  //    and the CPU usage percentage.
  // 2) It calls processevent to deal with user input
  // 3) It calls step to update the positions of the balls and bat.
  // 4) It calls plotscreen to update the image.
  // 5) It calls updatescreen to cause the image to be displayed.
  // 6) It then performs a suitable delay to ensure a reasonable
  //    frame rate.

  // If started=TRUE, stepdonems will be set to the time taken
  // to perform steps 1 to 4 less dbgmsecs and mspercycle to
  // an estimate of the time to perform all 5 steps, averaged over
  // 10 cycles. The ratio of these values gives an estimate of
  // the current usage as a percentage. It is only valid when non
  // zero.

  cyclecount := cyclecount + 1

//  dbgwritef("*n%i5: ######### Start of the main loop*n", cyclecount)

  // Possibly select output to tostream.
  IF cyclecount=0 | cyclecount = mincyclecount IF tostream DO
  { //dbgwritef("*nStart writing to file %s*n", tofilename)
    selectoutput(tostream)
  }

  IF currballno DO
  { extractballparams()
    //dbgwritef("currballno=%n*n", currballno)
    //dbgwritef("selx=%7.3d    sely=%7.3d ballradius=%7.3d*n",
    //           selx,         sely,      ballradius)
    //dbgwritef("selxdot=%7.3d selydot=%7.3d ag=%7.3d*n",
    //           selxdot,      selydot,      ag)
    //dbgwritef("batx=%7.3d batxdot=%7.3d*n", batx, batxdot)
  }

  IF maxcyclecount & cyclecount > maxcyclecount DO
  { selectoutput(stdout)
    //dbgwritef("End output at cyclecount=%n and finish*n", cyclecount)
    done := TRUE
    RETURN
  }

  // mspercycle is zero or an average over 10 cycles.
  //dbgwritef("mspercycle=%i3*n", mspercycle)

  comploop(1)
  //prstate()

  processevents()       // Deal with the latest events
  //dbgwritef("After processevents(): batxdot=%7.3d batxdotdot=%7.3d*n",
  //                                  batxdot,      batxdotdot)

  IF stepping DO
  { // Execute one step either under user or computer control
    step()
    //sdldelay(1)
  }

  plotscreen()
    
  updatescreen()        // Display the current state on the screen

  // Calculate the time since the most recent cycle start
  stepdonems :=
    rtn_msecs!rootnode-dbgmsecs-startedms-cyclestartms
  //dbgwritef("Setting stepdonems=%n*n", stepdonems)
  //dbgdelay(5_000)

  sdldelay(10)         // Allow other processes to run for a bit.

}

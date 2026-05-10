/* This is a simple bat and ball game

The file bucketnew.b is used for development and replaces bucket.b
and bcplprogs/raspi/bucket.b when it works well enough.

Implemented by Martin Richards (c) February 2013

History:

23/09/2023
The computer's control of the bat is being modified.

15/09/2019
Modified to run under 32 and 64 bit BCPL on 32 and 64 bit
mchines. All fields holding machine addresses are allocated
two BCPL words even though often only the first will be used.

17/02/2013
Successfully reimplemented the first version, bucket0.b, to
make it much more efficient.
*/

SECTION "sdllib"
GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the library source code
.
SECTION "bucket"
GET "libhdr"
GET "sdl.h"

MANIFEST {
  One  =    1_00000 // The constant 1.000 scaled with 5 decimal
                    // digits after the decimal point.
  OneK = 1000_00000

  batradius       = 10_00000
  ballradius      = 25_00000
  endradius       = 15_00000
  bucketthickness = 2 * endradius

  ag = -50_00000     // Gravity acceleration
}

GLOBAL {
  done:ug

  help         // Display help information
  stepping     // =FALSE if not stepping
  stepcount    // Incremented every time step is called
  starting     // Trap door open
  started      // The balls have been released from the bucket
               // and the trap door has now been closed.
  finished     // Al ball are now back in the bucket.
  
  starttime    // Set when starting becomes FALSE
  reltime      // Time to display

  bouncetime     // Estimated bounce time
  bouncex        // Estimated bounce x position
  
  usage        // Estimated utilisation of the CPU
  displayusage
  debugging

  sps          // Estimated steps per second, adjusted automatically

  timetozero
  step
  resetbucket
   
  // All surfaces now need two BCPL words since machine
  // addresses may need 64 bits and the BCPL word length
  // may be 32 bits. All functions that used to return
  // surfaces now take an extra argument pointer to a
  // surface pair as an extra argument. These will return
  // FALSE if unsuccessful.
  // The ok variables are TRUE if the corresponding
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

  wall_lx      // Left wall
  wall_rx      // Right wall
  floor_yt     // Floor
  ceiling_yb   // Ceiling
  midx

  screen_xc

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

  pushaccel        // This is the bat accelration until
  pushtimelimit    // reltime > pushtimelimit
  
  minx; miny       // Position of the lowest ball
  minxdot; minydot // Velocity of the lowest ball

  activebat        // If TRUE the bat is given random accelerations
  currballno       // = -1, 1, 2 or 3
  selx; sely       // The position of the selected ball
  selxdot; selydot // Velocity of the selected ball

  target_t
  target_x
  target_col

  tscomploop
  tsabort
}

// The next four function allow abort and comploop
// to run without causing reltime to increment.

LET ts(f, a) BE
{ // Execute f(a) without incrementing reltime.
  // This is done by modifying starttime.
  LET t0 = rtn_msecs!rootnode
  LET ftime = 0
  f(a)
  ftime := rtn_msecs!rootnode - t0 // Time taken by f call.
  starttime := starttime - ftime
  reltime := rtn_msecs!rootnode - starttime
}

LET comploop() BE
{ LET t0 = rtn_msecs!rootnode
RETURN
  writef("*ncomploop started "); deplete(cos)
  FOR i = 0 TO 7_000_000 DO LOOP
  writef("done after %6.3d secs*n", rtn_msecs!rootnode - t0)
}

LET tscomploop() BE ts(comploop)

LET tsabort(n) BE ts(abort, n)

LET incontact(p1,p2, d) = VALOF
{ // p1 and p2 point to the coordinated of the centres of
  // two circles. It returns TRUE if they are less then
  // d units apart.
  LET x1, y1 = p1!0, p1!1
  LET x2, y2 = p2!0, p2!1
  LET dx, dy = x1-x2, y1-y2
  IF ABS dx > d | ABS dy > d RESULTIS FALSE
  IF muldiv(dx,dx,One) + muldiv(dy,dy,One) >
     muldiv(d,d,One) RESULTIS FALSE
  RESULTIS TRUE
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

    IF t1dot<=0 RETURN

    // Reverse t1dot with some loss of energy
    t1dot := rebound(t1dot)

    // Transform back to (x,y) coordinates relative to cg
    rx1dot := inprod(t1dot,w1dot,  c,-s)
    ry1dot := inprod(t1dot,w1dot,  s, c)

    // Convert to world (x,y) coordinates
    p1!2 :=  rx1dot + cgxdot
    p1!3 :=  ry1dot + cgydot
    p2!2 := -rx1dot + cgxdot
    p2!3 := -ry1dot + cgydot

    // Apply a small repulsive force between balls
    p1!0 := p1!0 - muldiv(0_40000, c, One)
    p1!1 := p1!1 - muldiv(0_40000, s, One)
    p2!0 := p2!0 + muldiv(0_40000, c, One)
    p2!1 := p2!1 + muldiv(0_40000, s, One)

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
    p1!0 := p1!0 - muldiv(0_05000, c, One)
    p1!1 := p1!1 - muldiv(0_05000, s, One)
    p2!0 := p2!0 + muldiv(0_05000, c, One)
    p2!1 := p2!1 + muldiv(0_05000, s, One)

    RETURN
  }
}

AND rebound(vel) = vel/7 - vel // Returns the rebound speed of a bounce

AND cosines(dx, dy) = VALOF
{ LET d = ABS dx + ABS dy
  LET c = muldiv(dx, One, d)  // Approximate cos and sin
  LET s = muldiv(dy, One, d)  // Direction good, length not.
  LET a = muldiv(c,c,One)+muldiv(s,s,One) // 0.5 <= a <= 1.0
  d := One // With this initial guess only 3 iterations
           // of Newton-Raphson are required.
//writef("a=%8.5d  d=%8.5d  d^2=%8.5d*n", a, d, muldiv(d,d,One))
  d := (d + muldiv(a, One, d))/2
//writef("a=%8.5d  d=%8.5d  d^2=%8.5d*n", a, d, muldiv(d,d,One))
  d := (d + muldiv(a, One, d))/2
//writef("a=%8.5d  d=%8.5d  d^2=%8.5d*n", a, d, muldiv(d,d,One))
  d := (d + muldiv(a, One, d))/2
//writef("a=%8.5d  d=%8.5d  d^2=%8.5d*n", a, d, muldiv(d,d,One))

  s := muldiv(s, One, d) // Corrected cos and sin
  c := muldiv(c, One, d)
//writef("dx=%10.5d  dy=%10.5d => cos=%8.5d sin=%8.5d*n", dx, dy, c, s)

  result2 := s
  RESULTIS c
}

AND inprod(dx, dy, c, s) = muldiv(dx, c, One) + muldiv(dy, s, One)

AND ballbounces(pv) BE 
{ // This function deals with bounces between the ball whose position
  // and velocity is specified by pv and the bat or any fixed surface.
  // It does not deal with ball on ball bounces.
  LET cx, cy, vx, vy = pv!0, pv!1, pv!2, pv!3
  TEST xlim_bucket_ll <= cx <= xlim_bucket_rr &
       ylim_baseb     <= cy <= ylim_topt
  THEN { // The ball cannot be in contact with the cieling, floor or
         // either wall so we only need to check for contact with
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
         UNLESS starting DO
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
{ newline()
  writef("currballno=%n stepcount=%i4 activebat=%n*n",
          currballno,   stepcount,    activebat)
  //writef("cgx1=%9.5d cgy1=%9.5d cgx1dot=%10.5d cgy1dot=%10.5d*n",
  //        cgx1,      cgy1,      cgx1dot,      cgy1dot)
  //writef("cgx2=%9.5d cgy2=%9.5d cgx2dot=%10.5d cgy2dot=%10.5d*n",
  //        cgx2,      cgy2,      cgx2dot,      cgy2dot)
  //writef("cgx3=%9.5d cgy3=%9.5d cgx3dot=%10.5d cgy3dot=%10.5d*n",
  //        cgx3,      cgy3,      cgx3dot,      cgy3dot)

  IF currballno DO
  writef("selx=%9.5d sely=%9.5d selxdot=%10.5d selydot=%10.5d*n",
          selx,      sely,      selxdot,      selydot)
  //writef("ax1= %9.5d ax2= %9.5d ax3=    %10.5d*n", ax1, ax2, ax3)
  //writef("ay1= %9.5d ay2= %9.5d ay3=    %10.5d*n", ay1, ay2, ay3)
  writef("batx=%9.5d baty=%9.5d batxdot=%10.5d*n", batx, baty, batxdot)
  writef("batxdotdot=%9.5d*n", batxdotdot)
  writef("reltime=%8.3d pushtimelimit=% %8.3d*n",
          reltime,      pushtimelimit)
  IF currballno DO
  writef("bouncetime= %8.3d bouncex=%9.5d*n",
          bouncetime,     bouncex)
  //delay(2_000)
  //abort(3996)
}

LET step() BE
{ stepcount := stepcount + 1

  //writef("*nstep: entered, stepcount=%n currballno=%n activebat=%n*n",
  //                       stepcount,   currballno,   activebat)
  //writef("starting=%n started=%n*n", starting, started)
  
  IF started UNLESS finished DO
    reltime := rtn_msecs!rootnode - starttime // Time since the
                                              // latest start
  //writef("reltime=%9.3d*n", reltime)
  comploop()
  
  // Check whether to close the base
  WHILE starting DO
  { IF ylim_baseb < cgy1 & bucket_lxc < cgx1 < bucket_rxc BREAK  
    IF ylim_baseb < cgy2 & bucket_lxc < cgx2 < bucket_rxc BREAK  
    IF ylim_baseb < cgy3 & bucket_lxc < cgx3 < bucket_rxc BREAK
    starting   := FALSE
    started    := TRUE
    currballno := 0 // None selected
    finished   := FALSE
    starttime  := rtn_msecs!rootnode
    reltime    := 0
    BREAK  
  }

  // Test whether all three balls are safely in the bucket.
  IF started UNLESS finished DO
    IF bucket_byt < cgy1 < bucket_tyb &
       bucket_lxc < cgx1 < bucket_rxc &
       bucket_byt < cgy2 < bucket_tyb &
       bucket_lxc < cgx2 < bucket_rxc &
       bucket_byt < cgy3 < bucket_tyb &
       bucket_lxc < cgx3 < bucket_rxc &
       ABS cgy1dot < 2_00000 &
       ABS cgy2dot < 2_00000 &
       ABS cgy3dot < 2_00000 DO finished := TRUE
       
//FOR i = 1 TO 10000000 DO i:=i+1
  // Calculate the accelerations of the balls
  // Initialise as apply gravity
  ax1, ay1 := 0, ag
  ax2, ay2 := 0, ag
  ax3, ay3 := 0, ag

  // Add a little random horizontal motion
  ax1 := ax1 + randno(2001) - 1001
  ax2 := ax2 + randno(2001) - 1001
  ax3 := ax3 + randno(2001) - 1001

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
  cgx1dot := cgx1dot + ax1/sps
  cgy1dot := cgy1dot + ay1/sps
  cgx2dot := cgx2dot + ax2/sps
  cgy2dot := cgy2dot + ay2/sps
  cgx3dot := cgx3dot + ax3/sps
  cgy3dot := cgy3dot + ay3/sps

  cgx1, cgy1 := cgx1 + cgx1dot/sps, cgy1 + cgy1dot/sps
  cgx2, cgy2 := cgx2 + cgx2dot/sps, cgy2 + cgy2dot/sps
  cgx3, cgy3 := cgx3 + cgx3dot/sps, cgy3 + cgy3dot/sps

  UNLESS activebat DO
    batxdotdot := batxdotdot + (midx-batx)/10
    
  IF activebat DO
  { // The bat is being controlled by the computer.
//writef("Bat is controlled by the computer, currballno=%n*n", currballno)

    IF reltime < pushtimelimit DO
    { // Continue with the same bat push acceleration.
      batxdotdot := pushaccel
//writef("reltime=%9.3d pushtimelimit=%9.3d pushaccel=%9.5d*n",
//        reltime,      pushtimelimit,      pushaccel)
      GOTO pushbat
    }
    
    //IF currballno DO
    //  writef("currballno=%n Testing whether it is in the bucket*n",
    //          currballno)
      
    // If the current ball is in the bucket,
    // deselected it.

IF currballno DO
{ //writef("Ball %n is currently selected.*n")
  //writef("Testing whether it is in the bucket*n")

  //writef("bucket_byc=%9.5d bucket_tyb=%9.5d cgy1=%9.5d cgy2=%9.5d cgy3=%9.5d*n",
  //        bucket_byc,      bucket_tyb,      cgy1,      cgy2,      cgy3)
  //writef("bucket_lxc=%9.5d bucket_rxc=%9.5d cgx1=%9.5d cgx2=%9.5d cgx3=%9.5d*n",
  //        bucket_lxc,      bucket_rxc,      cgx1,      cgx2,      cgx3)
}
SWITCHON currballno INTO
    { DEFAULT: currballno := 0
               ENDCASE

      CASE 1:  IF bucket_byc < cgy1 < bucket_tyb &
                  bucket_lxc < cgx1 < bucket_rxc DO
	       { //writef("Ball %n deselected*n", currballno)
	         currballno := 0
	       }
	       ENDCASE

      CASE 2:  IF bucket_byc < cgy2 < bucket_tyb &
                  bucket_lxc < cgx2 < bucket_rxc DO
	       { //writef("Ball %n deselected*n", currballno)
	         currballno := 0
	       }
	       ENDCASE

      CASE 3:  IF bucket_byc < cgy3 < bucket_tyb &
                  bucket_lxc < cgx3 < bucket_rxc DO
	       { //writef("Ball %n deselected*n", currballno)
	         currballno := 0
	       }
	       ENDCASE
    }
    
    // If there is no selected ball, try to find one that is
    // not in the bucket.
    //abort(6661)
    //UNLESS currballno DO writef("There is no selected ball so try to select one*n")
    
    UNLESS currballno UNLESS bucket_byc < cgy1 < bucket_tyb &
                             bucket_lxc < cgx1 < bucket_rxc DO currballno := 1

    UNLESS currballno UNLESS bucket_byc < cgy2 < bucket_tyb &
                             bucket_lxc < cgx2 < bucket_rxc DO currballno := 2

    UNLESS currballno UNLESS bucket_byc < cgy3 < bucket_tyb &
                             bucket_lxc < cgx3 < bucket_rxc DO currballno := 3

    //TEST currballno
    //THEN writef("Ball %n is selected*n", currballno)
    //ELSE writef("No ball selected*n")

    // If there is a current ball, extract it parameters,
    // otherwise cause the bat accelerate toward midx for
    // about 1/10 sec.
    // Unless there is a selected ball, try to select one
    SWITCHON currballno INTO
    { DEFAULT: writef("ERROR: currballno=%n*n", currballno)
               abort(999)

      CASE 0:  ENDCASE

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

    UNLESS currballno DO
    { // No ball can be selected, so choose a small bat acceleration
      // towards midx for 1/10 sec.
      pushaccel := batx>midx -> -30_00000,
                                +30_00000
      batxdotdot := pushaccel
      pushtimelimit := reltime + 0_800
     // writef("No ball selected so set pushaccel=%9.5d towards midx and pushtimelimit=%9.3d*n",
     //                                 pushaccel,                       pushtimelimit)
      GOTO pushbat
    }
    
    // There is a selected ball.
    
   // writef("Calculating the time to bounce*n")
    
    // Calculate bouncetime, the time when the selected ball will
    // hit the ground, ignoring possible collisions.

    // The equation is
    // h = sely + selydot*t + g*t^2/2
    // where h = floor_yt + ballradius
    // and   t is the time relative to the current time (reltime)

    bouncetime := 2_00000  // Default values
    bouncex    := midx
    
    FOR i = 0 TO 40 DO
    { LET t = 0_200 * i + i*i*4
      LET halfgtt = muldiv(muldiv(ag/2, t, 1000), t, 1000)
      LET h = sely + muldiv(selydot, t, 1000) + halfgtt
      //writef("t= %9.3d: height=%9.5d*n", t, h-floor_yt-ballradius)
      IF h < floor_yt+ballradius DO
      { bouncetime := t //+ reltime
        bouncex    := selx + muldiv(selxdot, t, 1000)
	BREAK
      }
    }

    bouncetime := timetozero(sely-ballradius-floor_yt, selydot, ag)
    bouncex := selx + muldiv(selxdot, bouncetime, 1_000)

    // Deal with out of possible range values 
    UNTIL wall_lx <= bouncex <= wall_rx DO
    { IF bouncex < xlim_lwall DO bouncex := xlim_lwall + xlim_lwall - bouncex
      IF bouncex > xlim_rwall DO bouncex := xlim_rwall + xlim_rwall - bouncex
    }
    
    writef("Approx time to bounce=%9.3d  approx bouncex=%9.5d*n", bouncetime, bouncex)
abort(1000)
    TEST bouncetime > 1_500
    THEN { // Try to place the bat at distance wall_rx/4 from the selected ball
           // giving it the same velocity as the ball.
           LET tx = selx < midx -> selx + wall_rx/4,
	                           selx - wall_rx/4
           LET rxdot = selxdot - batxdot
           LET rx    = tx      - batx
	   pushaccel := 10*rx + 5*rxdot
	   batxdotdot := pushaccel
           IF pushtimelimit < reltime DO
	     pushtimelimit := reltime + 0_100
         }
    ELSE { // Accelerate the bat towards the ball for one sec
           TEST batx > selx THEN pushaccel := -100_00000
	                    ELSE pushaccel :=  100_00000
	   batxdotdot := pushaccel
           IF pushtimelimit < reltime DO
	     pushtimelimit := reltime + 0_500
         }
	 
    //abort(4493)
  }

pushbat:
  // Apply forces to the bat
  //sps := 10
  //writef("applying batxdotdot=%9.5d to bat*n", batxdotdot)
  batxdot := batxdot + batxdotdot/sps
  batxdot := batxdot - batxdot/sps  // damping
  
  //writef("batx=%9.5d  batxdot=%9.5d  batxdotdot=%9.5d*n",
  //        batx, batxdot, batxdotdot)
  IF batxdot> 600_00000 DO batxdot :=  600_00000
  IF batxdot<-600_00000 DO batxdot := -600_00000

  batx := batx + batxdot/sps

  IF batx > wall_rx-batradius DO
    batx, batxdot := wall_rx - batradius, -batxdot
  IF batx < batradius DO
    batx, batxdot := batradius, -batxdot

  // Slowly correct baty
  baty := baty - (baty - batradius)/10
  //writef("Returning from step() with batx=%9.5d baty=%9.5d*n", batx, baty)
  //abort(4494)
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

  // Allocate the surfaces if necessary
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

  // Left bucket wall
  IF bucketwallsurfok DO
    blitsurf(@bucketwallsurf, @screen, bucket_lxl/One, bucket_tyt/One)

  // Right bucket wall
  IF bucketwallsurfok DO
    blitsurf(@bucketwallsurf, @screen, bucket_rxl/One, bucket_tyt/One)

  // Bucket base
  IF bucketbasesurfok UNLESS starting DO
  { //sawritef("blitsurf of bucket base*n")
    //sawritef("bucketbasesurf=(%n %n)*n", bucketbasesurf, bucketbasesurf1)
    //sawritef("screen=(%n %n)*n", screen, screen1)
    //sawritef("bucket_lxc=%9.5d bucket_byt)=%9.5d screenysize=%n*n",
    //          bucket_lxc,      bucket_byt, screenysize)
    blitsurf(@bucketbasesurf, @screen, bucket_lxc/One, bucket_byt/One-1)
  }

  // The bat
  //writef("Bat: batx=%9.5d baty=%9.5d*n", batx, baty)
  IF batsurfok DO
    blitsurf(@batsurf, @screen, (batx-batradius)/One, (baty+batradius)/One)

  // Finally, the three balls
  setcolour(ball1colour)
  IF ball1surfok DO
  { blitsurf(@ball1surf, @screen, (cgx1-ballradius)/One, (cgy1+ballradius)/One)
    IF currballno=1 DO
    { setcolour(#x007F7F7F)
      drawfillrect(cgx1/One-5, cgy1/One-5, cgx1/One+5, cgy1/One+5)
    }
  }

  setcolour(ball2colour)
  IF ball2surfok DO
  { blitsurf(@ball2surf, @screen, (cgx2-ballradius)/One, (cgy2+ballradius)/One)
    IF currballno=2 DO
    { setcolour(#x007F7F7F)
      drawfillrect(cgx2/One-5, cgy2/One-5, cgx2/One+5, cgy2/One+5)
    }
  }

  setcolour(ball3colour)
  IF ball3surfok DO
  { blitsurf(@ball3surf, @screen, (cgx3-ballradius)/One, (cgy3+ballradius)/One)
    IF currballno=3 DO
    { setcolour(#x007F7F7F)
      drawfillrect(cgx3/One-5, cgy3/One-5, cgx3/One+5, cgy3/One+5)
    }
  }


  UNLESS target_x < 0 DO
  { target_col := currballno=1 -> ball1colour,
                  currballno=2 -> ball2colour,
                  currballno=3 -> ball3colour,
                  0
    setcolour(target_col)
    IF FALSE DO
    { IF target_t>0 DO
      { drawf(20, 100, "target_t=%9.5d", target_t)
        drawf(20,  70, "target_x=%9.5d", target_x)
      }
    
      setcolour(target_col & #x7F7F7F7F)
      drawf(target_x/One-6, 2, "**")
    }
  }


  setcolour(maprgb(255,255,255))

  IF finished DO
    drawf(30, 300, "Finished -- Well Done!")
    

  IF started | finished DO
    drawf(30, 280, "Time %9.2d", reltime/10)

  IF help DO
  { drawf(30, 150, "R  -- Reset")
    drawf(30, 135, "S  -- Start the game")
    drawf(30, 120, "P  -- Pause/Continue")
    drawf(30, 105, "H  -- Toggle help information")
    drawf(30,  90, "B  -- Toggle bat random motion")
    drawf(30,  75, "D  -- Toggle debugging")
    drawf(30,  60, "U  -- Toggle usage")
    drawf(30,  45, "Left/Right arrow -- Control the bat")
  }

  IF displayusage DO
    drawf(30, 245, "CPU usage = %i3%% sps = %n", usage, sps)

  IF debugging DO
  { drawf(30, 220, "Ball1 x=%10.5d  y=%10.5d xdot=%10.5d  ydot=%10.5d",
          cgx1, cgy1, cgx1dot, cgy1dot)
    drawf(30, 205, "Ball2 x=%10.5d  y=%10.5d xdot=%10.5d  ydot=%10.5d",
          cgx2, cgy2, cgx2dot, cgy2dot)
    drawf(30, 190, "Ball3 x=%10.5d  y=%10.5d xdot=%10.5d  ydot=%10.5d",
          cgx3, cgy3, cgx3dot, cgy3dot)
    drawf(30, 175, "Bat   x=%10.5d  y=%10.5d xdot=%10.5d",
          batx, baty, batxdot)
  }
}

AND resetbucket() BE
{ // Set the initial positions
  cgx1, cgy1 := screen_xc, bucket_byt+ballradius   + 10_00000
  cgx2, cgy2 := screen_xc, bucket_byt+3*ballradius + 20_00000
  cgx3, cgy3 := screen_xc, bucket_byt+5*ballradius + 30_00000

  // Set the initial velocities
  cgx1dot, cgx2dot, cgx3dot :=  0, 0, 0
  cgy1dot, cgy2dot, cgy3dot :=  0, 0, 0

  pushaccel, pushtimelimit := 0, 0
  
  //starting := FALSE
  started  := FALSE
  finished := FALSE
  reltime  := 0
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

      CASE 'U': displayusage := ~displayusage
                LOOP

      CASE 'B': activebat := ~activebat
                currballno := 0
                batxdotdot := 0
		minx, miny, minxdot, minydot := 0, 0, 0, 0
                LOOP

      CASE 'S': // Start again
                UNLESS ylim_baseb < cgy1 & bucket_lxc < cgx1 < bucket_rxc &
                       ylim_baseb < cgy2 & bucket_lxc < cgx2 < bucket_rxc &
                       ylim_baseb < cgy3 & bucket_lxc < cgx3 < bucket_rxc DO
                  resetbucket()
                starting    := TRUE
                started     := FALSE
                finished    := FALSE
                starttime   := -1
                reltime     := 0
                LOOP

      CASE 'P': // Toggle stepping
                stepping := ~stepping
                LOOP

      CASE 'R': // Reset the balls
                resetbucket()
                finished := FALSE
                starting := FALSE
                reltime  := 0
                LOOP

      CASE sdle_arrowright:
                batxdotdot := batxdotdot + 750_00000
		activebat := FALSE
		LOOP
      CASE sdle_arrowleft:
                batxdotdot := batxdotdot - 750_00000
		activebat := FALSE
		LOOP
    }

  CASE sdle_keyup:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:  LOOP

      CASE sdle_arrowright:
                batxdotdot := batxdotdot - 750_00000
		activebat := FALSE
		LOOP
      CASE sdle_arrowleft:
                batxdotdot := batxdotdot + 750_00000
		activebat := FALSE
		LOOP
    }


  CASE sdle_quit:
    writef("QUIT*n");
    done := TRUE
    LOOP
}

LET sqrt(x) = VALOF
{ // x is scaled 5 digits after the decimal point
  LET FLT fx = FLOAT x / 1_00000.0
  LET FLT fres = sys(Sys_flt, fl_sqrt, fx)
  LET res = FIX(fres * 1_00000.0)
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
  LET C   =  x
  LET B2  = muldiv(B, B, 1_00000)
  LET AC4 = 4 * muldiv(A/10, C/10, 1_00000)
  LET t   = muldiv(-B - sqrt(B2 - AC4), 1_00000/100, 2*A)
  UNLESS x>0 & a<0 DO
  { writef("Bad arguments a=%9.5d a=%9.5d*n", x, a)
    RETURN
  }
  
  writef("x            = %12.5d*n", x)
  writef("v            = %12.5d*n", v)
  writef("a            = %12.5d*n", a)
  writef("A            = %12.5d*n", A)
  writef("B            = %12.5d*n", B)
  writef("C            = %12.5d*n", C)
  writef("B2           = %12.5d*n", B2)
  writef("AC4          = %12.5d*n", AC4)
  writef("B2-AC4       = %12.5d*n", B2-AC4)
  writef("sqrt(B2-AC4) = %12.5d*n", sqrt(B2-AC4))
  writef("t            = %12.3d*n", t)
  writef("muldiv(-25_00000/10, 328_96875, 1_00000)**40 = %12.5d*n",
          muldiv(-25_00000/10, 328_96875, 1_00000)*4)
  abort(1000)
  RESULTIS t // Scaled 3 digits after the decimal point
}

LET tst(x, v, a) BE
{ writef("x=%12.5d, v=%12.5d a=%12.5d => t = %12.3d*n",
          x,        v,       a, timetozero(x, v, a))
}

LET start() = VALOF
{ LET stepmsecs = ?
  LET comptime  = 0 // Amount of cpu time per frame
  stepcount := 0
  bouncetime, bouncex := 0, 0
  
  UNLESS sys(Sys_sdl, sdl_avail) DO
  { writef("*nThe SDL features are not available*n")
    RESULTIS 0
  }

IF FALSE DO
{ writef("root 2 = %12.5d*n", sqrt(2_00000))
  tst(16_00000, 0, -32_00000)
  tst(20_00000, 0, -32_00000)
  tst(64_00000, 0, -32_00000)
  abort(998)
}

  bucketwallsurfok := FALSE
  bucketbasesurfok := FALSE
  ball1surfok := FALSE
  ball2surfok := FALSE
  ball3surfok := FALSE
  batsurfok := FALSE

  IF FALSE DO
  { // Code to test the cosines function
    LET e1, e2 = One, One
    FOR dy = 0 TO One BY One/100 DO
    { LET c, s, rsq = ?, ?, ?
      c := cosines(One, dy)
      s := result2
      rsq := muldiv(c,c,One) + muldiv(s,s,One)
      writef("dx=%9.5d  dy=%9.5d cos=%9.5d sin=%9.5d rsq=%9.5d*n",
              One, dy, c, s, rsq)
      IF e1 < rsq DO e1 := rsq
      IF e2 > rsq DO e2 := rsq
    }
    writef("Errors +%6.5d  -%7.5d*n", e1-One, One-e2)
    RESULTIS 0
  }

  initsdl()
  mkscreen("Ball and Bucket", 800, 500)
  //mkscreen("Ball and Bucket", 600, 400)

  help := TRUE & FALSE

  activebat := TRUE
  currballno := 0
  
  stepping  := TRUE     // =FALSE if not stepping
  starting  := TRUE     // Trap door open
  started   := FALSE
  finished  := FALSE
  starttime := -1
  reltime   := 0        // Time relative to start time
  
  usage        := 0
  debugging    := FALSE
  displayusage := FALSE
  sps          := 40 // Initial setting
  stepmsecs    := 1000/sps

  backcolour      := maprgb(120,120,120)
  bucketcolour    := maprgb(170, 60,  30)
  bucketendcolour := maprgb(140, 30,  30)
  ball1colour     := maprgb(255,  0,   0)
  ball2colour     := maprgb(  0,255,   0)
  ball3colour     := maprgb(  0,  0, 255)
  batcolour       := maprgb( 40, 40,  40)

  wall_lx := 0
  wall_rx := (screenxsize-1)*One      // Right wall
  midx := (wall_lx+wall_rx)/2
  
  floor_yt   := 0                     // Floor
  ceiling_yb := (screenysize-1)*One   // Ceiling

  screen_xc := screenxsize*One/2
  bucket_tyt := ceiling_yb - 6*ballradius
  bucket_tyc := bucket_tyt - endradius
  bucket_tyb := bucket_tyt - bucketthickness

  bucket_lxr := screen_xc  - ballradius * 5 / 2
  bucket_lxc := bucket_lxr - endradius
  bucket_lxl := bucket_lxr - bucketthickness

  bucket_rxl := screen_xc  + ballradius * 5 / 2
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

  resetbucket()

  ax1, ay1 := 0, 0   // Acceleration of ball 1
  ax2, ay2 := 0, 0   // Acceleration of ball 2
  ax3, ay3 := 0, 0   // Acceleration of ball 3

  batx := screen_xc  // Position of bat
  baty := floor_yt + batradius   // Position of bat
  ylim_bat := floor_yt + batradius + ballradius

  batxdot, batydot := 150_00000, 0 // Velocity of bat
  batxdotdot := 0         // Acceleration of bat

  target_x := midx
  
  done := FALSE 

  starttime := rtn_msecs!rootnode

  UNTIL done DO
  { LET loopstarttime = rtn_msecs!rootnode

    //prstate()
    //IF stepcount> 100 & stepcount MOD 1000 < 5 DO abort(3382)
    
    reltime := loopstarttime - starttime

    processevents()       // Deal with the latest events

    IF stepping DO
      step()              // Make one step of the simulation

    usage := 100*comptime/(stepmsecs|1)

    IF usage <   4 DO usage :=   4
    IF usage > 200 DO usage := 200
    
    plotscreen()
    
    updatescreen()        // Display the current state on the screen

   UNLESS 80<usage<95 DO
    { TEST usage>90       // Modify the usage value
      THEN sps := sps-1
      ELSE sps := sps+1
    }

    IF sps<5   DO sps := 5
    IF sps>30  DO sps := 30
    stepmsecs := 1000/sps      // Estimate msecs per call of step.

//writef("sps=%i4  stepmsecs=%n usage=%n*n", sps, stepmsecs, usage)

    reltime := rtn_msecs!rootnode - starttime // Calculate the time since start
    IF FALSE DO
    IF reltime < rtn_msecs!rootnode+stepmsecs DO
    { // Delay step() if it is too soon.
      //writef("starttime=%9.3d reltime=%9.3d*n*n", starttime, reltime)
      //writef("Calling sdldelay(%n)*n", rtn_msecs!rootnode+stepmsecs - reltime)
      sdldelay(rtn_msecs!rootnode+stepmsecs - reltime)
    }
    
    //IF stepcount MOD 10 = 0 DO sdldelay(1)
    sdldelay(0_010)
  }

  writef("*nQuitting*n")
  sdldelay(0_500)

  IF bucketwallsurfok DO freesurface(@bucketwallsurf)
  IF bucketbasesurfok DO freesurface(@bucketbasesurf)
  IF ball1surfok      DO freesurface(@ball1surf)
  IF ball2surfok      DO freesurface(@ball2surf)
  IF ball3surfok      DO freesurface(@ball3surf)
  IF batsurfok        DO freesurface(@batsurf)

  closesdl()
  RESULTIS 0
}



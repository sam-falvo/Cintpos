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

  ag = 50_00000     // Gravity acceleration
}

GLOBAL {
  done:ug

  help         // Display help information
  stepping     // =FALSE if not stepping
  starting     // Trap door open
  started      // The balls have been released from the bucket
               // and the trap door has now been closed.
  finished     // Al ball are now back in the bucket.
  
  starttime    // Set when starting becomes FALSE
  displaytime  // Time to display
  usage        // Estimated utilisation of the CPU
  displayusage
  debugging

  sps          // Estimated steps per second, adjusted automatically

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
  
  minx; miny       // Position of the lowest ball
  minxdot; minydot // Velocity of the lowest ball

  activebat        // If TRUE the bat is given random accelerations
  currballno       // = -1, 1, 2 or 3
  selx; sely       // The position of the selected ball
  selxdot; selydot // Velocity of the selected ball

  target_t
  target_x
  target_col
}

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

LET step() BE
{ IF started UNLESS finished DO
    displaytime := sdlmsecs() - starttime

  // Check whether to close the base
  WHILE starting DO
  { IF ylim_baseb < cgy1 & bucket_lxc < cgx1 < bucket_rxc BREAK  
    IF ylim_baseb < cgy2 & bucket_lxc < cgx2 < bucket_rxc BREAK  
    IF ylim_baseb < cgy3 & bucket_lxc < cgx3 < bucket_rxc BREAK
    starting := FALSE
    started := TRUE
    currballno := -1 // None selected
    finished := FALSE
    starttime := sdlmsecs()
    displaytime := 0
    BREAK  
  }

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
  ax1, ay1 := 0, -ag
  ax2, ay2 := 0, -ag
  ax3, ay3 := 0, -ag

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
  { // The bat is controlled by the computer
    //LET aval = 700_00000  // Bat accelaration to use
    
    //writef("*nbatx=%8.5d batxdot=%8.5d*n", batx, batxdot)

    // bucket_byc       // Centre level of the bucket base
    // activebat        // If TRUE the bat is given random accelerations
    // currballno       // = -1, 1, 2 or 3
    // selx, sely       // The position of the selected ball
    // selxdot, selydot // Velocity of the selected bal1
    // target_x          Estimate bounce position of the current ball
    
    // Unset currballno if it has moved above the bucket base level.
    IF currballno=1 & cgy1>bucket_byc DO currballno := -1
    IF currballno=2 & cgy2>bucket_byc DO currballno := -1
    IF currballno=3 & cgy3>bucket_byc DO currballno := -1
    //writef("2 currballno=%n*n", currballno)
    
    // If currballno is unset try to select a ball.
    IF currballno<0 & cgy1<bucket_byc & cgy1dot<=0 DO currballno := 1
    IF currballno<0 & cgy2<bucket_byc & cgy2dot<=0 DO currballno := 2
    IF currballno<0 & cgy3<bucket_byc & cgy3dot<=0 DO currballno := 3
    //writef("currballno=%n*n", currballno)

    // Choose a target for the bat and its acceleation.
    
    TEST currballno<=0
    THEN {
      // There is no current ball so select midx as the target
      // and choose the ball's acceleration.
      target_x := midx
      batxdotdot := chooseaccel(target_x,
                                batx, batxdot,
                                0, // Desired speed when
			           // reaching target_x
			        10_00000 // Time to reach target_x
			       )
    }
    ELSE {
      // There is a selected ball so get the ball's parameters
      IF currballno=1 DO
        selx, sely, selxdot, selydot := cgx1, cgy1, cgx1dot, cgy1dot
      IF currballno=2 DO
        selx, sely, selxdot, selydot := cgx2, cgy2, cgx2dot, cgy2dot
      IF currballno=3 DO
        selx, sely, selxdot, selydot := cgx3, cgy3, cgx3dot, cgy3dot

      // If the selected ball is not travelling down fast enough,
      // select selx as the target and select the bat's acceleration.

      TEST selydot > -1_00000
      THEN {
        // The ball is not moving down fast enough so
        // select selx as the target and choose the bat/s
	// acceleration
	target_x := selx
        batxdotdot := chooseaccel(target_x,
                                  batx, batxdot,
                                  0, // Desired speed when
		                  // reaching target_x
		                  10_00000 // Time to reach target_x
		                 )
      }
      ELSE {
        // The selected ball is falling fast enough so
        // estimate the time until it hits the floor and
        // its position at that time.
        target_t := muldiv(sely-ballradius, One, -selydot)
        target_x := selx + muldiv(selxdot, target_t, One)

        // Possible bouncing off the left wall.
        IF target_x <= xlim_lwall DO target_x := wall_lx - target_x
        // Possible bouncing off the right wall.
        IF target_x >= xlim_rwall DO target_x := wall_rx - target_x

        // Choose an acceleration for the bat.
        batxdotdot := chooseaccel(target_x,
                                  batx, batxdot,
	 		          50_00000, // Desired speed when
			                    // reaching target_x
			          target_t  // Time to reach tagetx
			         )
      }
    }
  }

  // Apply forces to the bat
  //sps := 10
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
}

AND chooseaccel(targx, batx, batxdot, speed, t) = VALOF
TEST speed {
  // Equation of motion:
  //   s = s0 + v0*t + a * t^2 / 2
  // so
  //   target_x = batx + batxdot*t + a * t^2 / 2
  // so
  //   a = (targtx - batx - batxdot*t) * 2 / t^2
  LET a = 2 * muldiv(targx-batx-muldiv(batxdot,t,One),
                     One,
		     muldiv(t,t,One))
  // v = v0 + a*t
  LET v = batxdot + muldiv(a, t, One)
  IF FALSE DO
  { writef("*ncurrballno=%n*n", currballno)
    writef("selx=%9.5d selxdot=%0.5d*n", selx, selxdot)
    writef("sely=%9.5d selydot=%0.5d*n", sely, selydot)
    writef("t=%9.5d targx=%0.5d*n", t, targx)
    writef("batx=%9.5d batxdot=%9.5d*n", batx, batxdot)
    writef("speed=%9.5d*n", speed)
    writef("a=%0.5d v=%9.5d*n", a, v)
  }
  TEST batx<targx
  THEN a := a + ( v<speed->+5_00000,-5_00000)
  ELSE a := a + (-v<speed->-5_00000,+5_00000)
  IF FALSE DO
  { writef("=> a=%0.5d*n", a)
    abort(5678)
  }
  RESULTIS a
}
ELSE {
  // speed is zero, so try to set batxdot to about
  // 2*(targx-batx)/t hoping to reach targx with a low speed.
  LET v = 2 * muldiv(targx-batx, One, t)
  TEST v>0
  THEN  batxdotdot := batxdot>+v -> -5_00000, +5_00000
  ELSE  batxdotdot := batxdot<-v -> +5_00000, -5_00000
  //writef("batxdotdot=%9.5d*n", batxdotdot)
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
    blitsurf(@bucketbasesurf, @screen, bucket_lxc/One, bucket_byt/One-1)
  }
  // The bat
  IF batsurfok DO
    blitsurf(@batsurf, @screen, (batx-batradius)/One, (baty+batradius)/One)

  // Finally, the three balls
  setcolour(ball1colour)
  IF ball1surfok DO
    blitsurf(@ball1surf, @screen, (cgx1-ballradius)/One, (cgy1+ballradius)/One)
  setcolour(ball2colour)
  IF ball2surfok DO
    blitsurf(@ball2surf, @screen, (cgx2-ballradius)/One, (cgy2+ballradius)/One)
  setcolour(ball3colour)
  IF ball3surfok DO
    blitsurf(@ball3surf, @screen, (cgx3-ballradius)/One, (cgy3+ballradius)/One)

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
    drawf(30, 280, "Time %9.2d", displaytime/10)

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

AND resetballs() BE
{ cgy1 := bucket_byt+ballradius   + 10_00000
  cgy2 := bucket_byt+3*ballradius + 20_00000
  cgy3 := bucket_byt+5*ballradius + 30_00000
  cgx1, cgx2, cgx3 := screen_xc, screen_xc, screen_xc 
  cgx1dot, cgx2dot, cgx3dot :=  0, 0, 0
  cgy1dot, cgy2dot, cgy3dot :=  0, 0, 0

  starting    := FALSE
  started     := FALSE
  finished    := FALSE
  displaytime := -1
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
                currballno := -1
                batxdotdot := 0
		minx, miny, minxdot, minydot := 0, 0, 0, 0
                LOOP

      CASE 'S': // Start again
                UNLESS ylim_baseb < cgy1 & bucket_lxc < cgx1 < bucket_rxc &
                       ylim_baseb < cgy2 & bucket_lxc < cgx2 < bucket_rxc &
                       ylim_baseb < cgy3 & bucket_lxc < cgx3 < bucket_rxc DO
                  resetballs()
                starting := TRUE
                started := FALSE
                finished := FALSE
                starttime := -1
                displaytime := -1
                LOOP

      CASE 'P': // Toggle stepping
                stepping := ~stepping
                LOOP

      CASE 'R': // Reset the balls
                resetballs()
                finished := FALSE
                starting := FALSE
                displaytime := -1
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

LET start() = VALOF
{ LET stepmsecs = ?
  LET comptime  = 0 // Amount of cpu time per frame

  UNLESS sys(Sys_sdl, sdl_avail) DO
  { writef("*nThe SDL features are not available*n")
    RESULTIS 0
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

  help := TRUE

  activebat := TRUE
  currballno := -1
  
  stepping := TRUE     // =FALSE if not stepping
  starting := TRUE     // Trap door open
  started := FALSE
  finished := FALSE
  starttime := -1
  displaytime := -1
  usage := 0
  debugging := FALSE
  displayusage := FALSE
  sps := 40 // Initial setting
  stepmsecs := 1000/sps

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

  resetballs()

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

  UNTIL done DO
  { LET t0 = sdlmsecs()
    LET t1 = ?

    processevents()

    IF stepping DO step()

    usage := 100*comptime/(stepmsecs|1)
    plotscreen()
    updatescreen()
    UNLESS 80<usage<95 DO
    { TEST usage>90
      THEN sps := sps-1
      ELSE sps := sps+1
      IF sps<5 DO sps := 5
      stepmsecs := 1000/sps
    }

    t1 := sdlmsecs()

    comptime := t1 - t0
    IF t0+stepmsecs > t1 DO sdldelay(t0+stepmsecs-t1)
    //sdldelay(0_100)
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



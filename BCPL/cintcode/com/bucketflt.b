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

GLOBAL {
  done:ug

  FLT One           // The constant    1.0
  FLT OneK          // The constant 1000.0

  FLT batradius
  FLT ballradius
  FLT endradius
  FLT bucketthickness
  FLT pushlevel       // Level below which the bat pushes

  FLT ag         // Gravity acceleration

  help           // Display help information
  stepping       // =FALSE if not stepping
  singlestepping // Single stepping
  starting       // Trap door open
  started        // The balls have been released from the bucket
                 // and the trap door has now been closed.
  finished       // All balls are now back in the bucket.
  
  starttime      // in msecs Set when starting becomes FALSE
  displaytime    // In msecs Time to display
  
  ballpushtime   // End time of ball push
  ballpushxdot   // Desired ball push speed
  
  usage          // Estimated utilisation of the CPU
  displayusage
  debugging

  FLT sps        // Estimated steps per second, adjusted automatically

  // All surfaces now need two BCPL words since machine
  // addresses may need 64 bits and the BCPL word length
  // may be 32 bits. All functions that used to return
  // surfaces now take an extra argument pointer to a
  // surface pair as an extra argument. These will return
  // FALSE if unsuccessful.
  // The ok variables are TRUE if the corresponding
  // surface has been created.

  bucketwallsurf  // Surface for the bucket walls
  bucketwallsurfok
  
  bucketbasesurf  // Surface for the bucket base
  bucketbasesurfok
  
  ball1surf       // Surfaces for the three balls
  ball1surfok
  
  ball2surf
  ball2surfok
  
  ball3surf
  ball3surfok
  
  batsurf         // Surface for the bat
  batsurfok

  backcolour      // Background colour
  bucketcolour
  bucketendcolour
  ball1colour
  ball2colour
  ball3colour
  batcolour

  FLT wall_lx      // Left wall
  FLT wall_rx      // Right wall
  FLT floor_yt     // Floor
  FLT ceiling_yb   // Ceiling
  FLT midx

  FLT screen_xc

  FLT bucket_lxl; FLT bucket_lxc; FLT bucket_lxr // Bucket left wall
  FLT bucket_rxl; FLT bucket_rxc; FLT bucket_rxr // Bucket right wall
  FLT bucket_tyb; FLT bucket_tyc; FLT bucket_tyt // Bucket top
  FLT bucket_byb; FLT bucket_byc; FLT bucket_byt // Bucket base

  // Ball bounce limits allowing for the radius of the balls.
  FLT xlim_lwall;     FLT xlim_rwall
  FLT ylim_floor;     FLT ylim_ceiling
  FLT xlim_bucket_ll; FLT xlim_bucket_lc; FLT xlim_bucket_lr 
  FLT xlim_bucket_rl; FLT xlim_bucket_rc; FLT xlim_bucket_rr
  FLT ylim_topt
  FLT ylim_baseb;     FLT ylim_baset 
  FLT ylim_bat

  // Positions, velocities and accelerations of each ball
  FLT cgx1; cgy1; FLT cgx1dot; FLT cgy1dot; FLT ax1; FLT ay1
  FLT cgx2; cgy2; FLT cgx2dot; FLT cgy2dot; FLT ax2; FLT ay2
  FLT cgx3; cgy3; FLT cgx3dot; FLT cgy3dot; FLT ax3; FLT ay3

   // Position, velocity and acceleration of the bat
   // These must all be defined and live in consective globals
   // for the bouncing mechanism to work
   
  FLT batx; FLT baty; FLT batxdot; FLT batydot; FLT batxdotdot
  
  activebat        // If TRUE the bat is given random accelerations
  currballno       // = -1, 1, 2 or 3
  prevballno       // = -1, 1, 2 or 3
  FLT selx; FLT sely       // The position of the selected ball
  FLT selxdot; FLT selydot // Velocity of the selected ball

  ballselecttime     // In msecs, if displaytime > ballselecttime
                     // currballno is set to -1 to cause a new
		     // ball to be selected.

  batpushtime        // In msecs, end time of ball push
  FLT batpushxdotdot // Desired ball push acceleration

  ballbounces
}

LET incontact(p1, p2, FLT d) = VALOF
{ // p1 and p2 point to the coordinated of the centres of
  // two circles. It returns TRUE if they are less then
  // distance d apart.
  LET FLT x1, FLT y1 = p1!0, p1!1
  LET FLT x2, FLT y2 = p2!0, p2!1
  LET FLT dx, FLT dy = x1-x2, y1-y2
  IF ABS dx > d | ABS dy > d RESULTIS FALSE
  IF dx*dx + dy*dy > d*d RESULTIS FALSE
  RESULTIS TRUE
}

AND cbounce(p1, p2, FLT m1, FLT m2) BE
{ // p1 -> [x1, y1, x1dot, y1dot] The position and velecity of a ball, bat or bucket end.
  // p2 -> [x2, y2, x2dot, y2dot] The position and velecity of a ball.
  // The two objects are both circular and in contact.
  
  // m1 and m2 are the masses of the two objects in arbitrary units
  // m2 = 0 if p1 is a bucket end and p2 is a ball.
  // m1=m2  if the collition is between two balls
  // m1=5.0 and m2=1.0 is for collisions between the bat and ball assuming the bat
  // has five times the mass of the ball.

  LET FLT x1    = p1!0
  LET FLT y1    = p1!1
  LET FLT x1dot = p1!2
  LET FLT y1dot = p1!3
  LET FLT x2    = p2!0
  LET FLT y2    = p2!1
  LET FLT x2dot = p2!2
  LET FLT y2dot = p2!3

  LET FLT c = cosines(x2-x1, y2-y1) // Direction from p1 to p2 as cosines
  LET FLT s = result2

  IF m2=0.0 DO
  { // Object 1 is a bucket corner
    // Object2 is a ball
    // Transform to (t,w) coordinates
    // where t is in the direction from the corner to the ball.
    LET FLT tdot =  x2dot*c + y2dot*s
    LET FLT wdot = -x2dot*s + y2dot*c

    IF tdot>0 RETURN

    // Object 2 is getting closer so reverse tdot (but not wdot)
    // and transform back to world (x,y) coordinates.
    tdot := rebound(tdot) // Reverse tdot with some loss of energy
    // Transform back to real world (x,y) coordinates
    p2!2 := tdot*c - wdot*s
    p2!3 := tdot*s + wdot*c

    RETURN
  }

  IF m1=m2 DO
  { // Objects 1 and 2 are both balls of equal mass
    // Find the velocity of the centre of gravity
    LET FLT cgxdot = (x1dot+x2dot)/2.0
    LET FLT cgydot = (y1dot+y2dot)/2.0
    // Calculate the velocity of object 1
    // relative to the centre of gravity
    LET FLT rx1dot = x1dot - cgxdot
    LET FLT ry1dot = y1dot - cgydot
    // Transform to (t,w) coordinates
    LET FLT t1dot = finprod(rx1dot,ry1dot,  c,s)
    LET FLT w1dot = finprod(rx1dot,ry1dot, -s,c)

    IF t1dot<=0 RETURN

    // Reverse t1dot with some loss of energy
    t1dot := rebound(t1dot)

    // Transform back to (x,y) coordinates relative to cg
    rx1dot := finprod(t1dot,w1dot,  c,-s)
    ry1dot := finprod(t1dot,w1dot,  s, c)

    // Convert to world (x,y) coordinates
    p1!2 :=  rx1dot + cgxdot
    p1!3 :=  ry1dot + cgydot
    p2!2 := -rx1dot + cgxdot
    p2!3 := -ry1dot + cgydot

    // Apply a small repulsive force between balls
    p1!0 := p1!0 - 0.4 * c
    p1!1 := p1!1 - 0.4 * s
    p2!0 := p2!0 + 0.4 * c
    p2!1 := p2!1 + 0.4 * s

    RETURN
  }

  { // Object 1 is the bat
    // Object 2 is a ball
    // Find the velocity of the centre of gravity
    LET FLT cgxdot = (x1dot*m1+x2dot*m2)/(m1+m2)
    LET FLT cgydot = (y1dot*m1+y2dot*m2)/(m1+m2)
    // Calculate the velocities of the two objects
    // relative to the centre of gravity
    LET FLT rx1dot = p1!2 - cgxdot
    LET FLT ry1dot = p1!3 - cgydot
    LET FLT rx2dot = p2!2 - cgxdot
    LET FLT ry2dot = p2!3 - cgydot
    // Transform to (t,w) coordinates
    LET FLT t1dot = finprod(rx1dot,ry1dot,  c,s)
    LET FLT w1dot = finprod(rx1dot,ry1dot, -s,c)
    LET FLT t2dot = finprod(rx2dot,ry2dot,  c,s)
    LET FLT w2dot = finprod(rx2dot,ry2dot, -s,c)

    IF t1dot<=0 RETURN

    // Reverse t1dot and t2dot with some loss of energy
    t1dot := rebound(t1dot)
    t2dot := rebound(t2dot)

    // Transform back to (x,y) coordinates relative to cg
    rx1dot := finprod(t1dot,w1dot,  c,-s)
    ry1dot := finprod(t1dot,w1dot,  s, c)
    rx2dot := finprod(t2dot,w2dot,  c,-s)
    ry2dot := finprod(t2dot,w2dot,  s, c)

    // Convert to world (x,y) coordinates
    p1!2 := rx1dot + cgxdot
    p1!3 := ry1dot + cgydot // The bat cannot move vertically
    p2!2 := rx2dot + cgxdot
    p2!3 := ry2dot + cgydot

    // Apply a very small repulsive force
    p1!0 := p1!0 - 0.05 * c
    p1!1 := p1!1 - 0.05 * s
    p2!0 := p2!0 + 0.05 * c
    p2!1 := p2!1 + 0.05 * s

    RETURN
  }
}

AND rebound(FLT vel) = vel/7.0 - vel // Returns the rebound speed of a bounce

AND cosines(FLT dx, FLT dy) = VALOF
{ LET FLT d = ABS dx + ABS dy
  LET FLT c = dx / d  // Approximate cos and sin
  LET FLT s = dy / d  // Direction good, length not.
  LET FLT a = c*c + s*s // 0.5 <= a <= 1.0
  d := One // With this initial guess only 3 iterations
           // of Newton-Raphson are required.
//writef("a=%8.5f  d=%8.5f  d^2=%8.5f*n", a, d, d*d)
  d := (d + a/d)/2.0
//writef("a=%8.5d  d=%8.5f  d^2=%8.5f*n", a, d, d*d)
  d := (d + a/d)/2.0
//writef("a=%8.5d  d=%8.5f  d^2=%8.5f*n", a, d, d*d)
  d := (d + a/d)/2.0
//writef("a=%8.5d  d=%8.5f  d^2=%8.5f*n", a, d, d*d)

  s := s/d // Corrected cos and sin
  c := c/d
//writef("dx=%10.5f  dy=%10.5f => cos=%8.5f sin=%8.5f*n", dx, dy, c, s)

  result2 := s
  RESULTIS c
}

AND finprod(FLT dx, FLT dy, FLT c, FLT s) = dx*c + dy*s

AND ballbounces(p) BE 
{ // p -> [ x, y, xdot, ydot, ax, ay ]
  // ie the position and velocity and acceleration of the ball.
  // This function deals with bounces between the ball 
  // and any fixed objects or the bat.
  // It does not deal the ball bouncing off other balls.
  LET FLT x, FLT y, FLT xdot, FLT ydot = p!0, p!1, p!2, p!3
  TEST xlim_bucket_ll < x < xlim_bucket_rr &
       ylim_baseb     < y < ylim_topt
  THEN { // The ball is inside or touching the bucket and
         // cannot be in contact with the ceiling, floor or
         // either wall so we only need to check for contact with
         // the bucket

         IF y > bucket_tyc DO
         { LET FLT x1, FLT y1, FLT x1dot, FLT y1dot = bucket_lxc, bucket_tyc, 0.0, 0.0
	   // Test whether in contact with the bucket left circular top.
           IF incontact(@x1, p, endradius+ballradius) DO
           { cbounce(@x1, p, 1.0, 0.0)
             // No other bounces possible
             RETURN
           }
           x1 := bucket_rxc
	   // Test whether in contact with the bucket right circular top.
           IF incontact(@x1, p, endradius+ballradius) DO
           { cbounce(@x1, p, 1.0, 0.0)
             // No other bounces possible
             RETURN
           }
           // No other bounces possible
           RETURN
         }

         IF y >= bucket_byc DO
         { // The ball is at the same level as the bucket walls

           IF x <= bucket_lxc DO
           { //The ball is in contact with the outside of the  bucket left wall
             p!0 := xlim_bucket_ll
             IF xdot>0.0 DO p!2 := rebound(xdot)
           }
           IF bucket_lxc < x <= xlim_bucket_lr DO
           { // The ball is in contact with the inside of the bucket left wall
             p!0 := xlim_bucket_lr
             IF xdot<0.0 DO p!2 := rebound(xdot)
           }
           IF xlim_bucket_rl <= x < bucket_rxc DO
           { // The ball is in contact with the inside of the bucket right wall
             p!0 := xlim_bucket_rl
             IF xdot>0.0 DO p!2 := rebound(xdot)
           }
           IF bucket_rxc < x DO
           { // The ball is in contact with the outside of the bucket right wall
             p!0 := xlim_bucket_rr
             IF xdot<0.0 DO p!2 := rebound(xdot)
           }
         }

         // The ball may be in contact with the base of the bucket
	 
         UNLESS starting DO
         { // The bucket base is present
           IF bucket_lxc <= x <= bucket_rxc DO
           {
             IF y < bucket_byc DO
             { // Bounce on the outside of the base
               p!1 := ylim_baseb
               IF ydot>0.0 DO p!3 := rebound(ydot)
               // No other bounces are possible
               RETURN
             }
             IF bucket_byc <= y <= ylim_baset DO
             { // Bounce on the top of the base
               p!1 := ylim_baset
               IF ydot<0.0 DO p!3 := rebound(ydot)
               // No other bounces are possible
               RETURN
             }
           }
         }

         // Bounces with the bottom corners
         IF y < bucket_byc DO
         { LET FLT x1, FLT y1, FLT x1dot, FLT y1dot = bucket_lxc, bucket_byc, 0, 0
           IF incontact(@x1, p, endradius+ballradius) DO
           { // Bounce with bottom left corner
             cbounce(@x1, p, 1.0, 0.0)
             // No other bounces are possible
             RETURN
           }
           x1 := bucket_rxc
           IF incontact(@x1, p, endradius+ballradius) DO
           { // Bounce with bottom right corner
             cbounce(@x1, p, 1.9, 0.0)
             // No other bounces are possible
             RETURN
           }
         }
       }
  ELSE { // The ball can only be in contact with the bat, side walls,
         // ceiling or floor

         // Bouncing with the bat
         IF incontact(@batx, p, batradius+ballradius) DO
         { p!4, p!5 := 0, 0
           cbounce(@batx, p, 5.0, 1.0)
           batydot := 0.0 // Immediately damp out the bat's vertical motion
         }

         // Left wall bouncing
         IF x <= xlim_lwall DO
         { p!0 := xlim_lwall
           IF xdot<0.0 DO p!2 := rebound(xdot)
         }

         // Right wall bouncing
         IF x >= xlim_rwall DO
         { p!0 := xlim_rwall
           IF xdot>0.0 DO p!2 := rebound(xdot)
         }

         // Ceiling bouncing
         IF y >= ylim_ceiling DO
         { p!1 := ylim_ceiling
           IF ydot>0.0 DO p!3 := rebound(ydot)
           // No other bounces are possible
           RETURN
         }

         // Floor bouncing
         IF y <= ylim_floor DO
         { p!1 := ylim_floor
	 //sawritef("bounces: ball in contact with the floor*n")
           IF ydot<0.0 DO p!3 := rebound(ydot)
         }

         // No other bounces are possible
         RETURN
       }
}

AND prstate() BE
{ writef("%9.3f currballno=%n*n", displaytime, currballno)
  writef("cgx1=%9.5f cgy1=%9.5f cgx1dot=%9.5f cgy1dot=%9.5f*n",
          cgx1,      cgy1,      cgx1dot,      cgy1dot)
  writef("cgx2=%9.5f cgy2=%9.5f cgx2dot=%9.5f cgy2dot=%9.5f*n",
          cgx2,      cgy2,      cgx2dot,      cgy2dot)
  writef("cgx3=%9.5f cgy3=%9.5f cgx3dot=%9.5f cgy3dot=%9.5f*n",
          cgx3,      cgy3,      cgx3dot,      cgy3dot)
  writef("ax1=%9.5f ax2=%9.5f ax3=%9.5f*n", ax1, ax2, ax3)
  writef("batx=%9.5f baty=%9.5f batxdot=%9.5f*n", batx, baty, batxdot)
}

LET step() BE
{
//  writef("step: entered*n")
  prstate()
  
  IF started UNLESS finished DO
  { displaytime := sdlmsecs() - starttime
    IF displaytime > ballselecttime DO
    { currballno := -1
      ballselecttime := displaytime + 5_000 // msecs
      writef("%8.3d Unselecting a ball*n", displaytime)
    }
  }

  // Check whether to close the base
  WHILE starting DO
  { IF ylim_baseb < cgy1 & bucket_lxc < cgx1 < bucket_rxc BREAK  
    IF ylim_baseb < cgy2 & bucket_lxc < cgx2 < bucket_rxc BREAK  
    IF ylim_baseb < cgy3 & bucket_lxc < cgx3 < bucket_rxc BREAK
    starting    := FALSE
    started     := TRUE
    finished    := FALSE
    currballno  := -1 // None selected
    starttime   := sdlmsecs()
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
       ABS cgy1dot < 2.0 &
       ABS cgy2dot < 2.0 &
       ABS cgy3dot < 2.0 DO finished := TRUE

//FOR i = 1 TO 10000000 DO i:=i+1  // A CPU bound delay

  // Calculate the accelerations of the balls
  // Initialise as apply gravity
  ax1, ay1 := 0.0, -ag
  ax2, ay2 := 0.0, -ag
  ax3, ay3 := 0.0, -ag

  // Add a little random horizontal motion
  ax1 := ax1 + FLOAT(randno(2001) - 1001) / 1_00000.0
  ax2 := ax2 + FLOAT(randno(2001) - 1001) / 1_00000.0
  ax3 := ax3 + FLOAT(randno(2001) - 1001) / 1_00000.0

  //writef("ax1=%9.5f ax2=%9.5f ax3=%9.5f*n", ax1, ax2, ax3)
  //abort(7123)
  
  ballbounces(@cgx1)
  ballbounces(@cgx2)
  ballbounces(@cgx3)

  // Ball on ball bounce
  //IF FALSE DO
  IF incontact(@cgx1, @cgx2, ballradius+ballradius) DO
  { ay1, ay2 := 0.0, 0.0
    //writef("Ball 1 and 2 in contact*n")
    //abort(9912)
    cbounce(@cgx1, @cgx2, 1.0, 1.0)
  }

  //IF FALSE DO
  IF incontact(@cgx1, @cgx3, ballradius+ballradius) DO
  { ay1, ay3 := 0.0, 0.0
    //writef("Ball 1 and 3 in contact*n")
    //abort(9913)
    cbounce(@cgx1, @cgx3, 1.0, 1.0)
  }

  //IF FALSE DO
  IF incontact(@cgx2, @cgx3, ballradius+ballradius) DO
  { ay2, ay3 := 0.0, 0.0
    //writef("Ball 2 and 3 in contact*n")
    //abort(9923)
    cbounce(@cgx2, @cgx3, 1.0, 1.0)
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

//writef("cgy1=%9.5f cgy1dot=%9.5f ay1=%9.5f sps=%9.5f*n", cgy1, cgy1dot, ay1, sps)

  //UNLESS activebat DO
  //  batxdotdot := batxdotdot + (midx-batx)/10
    
  IF FALSE DO
  IF activebat DO
  { // The bat is controlled by the computer.
    // It selects a ball, if possible.
    // If no ball is selected it chooses an acceleration towards
    // midx and maintains this acceleration for a short time.
    // If there is a selected ball and it is some distance above
    // the ground, it tries to move to an x position some distance
    // away from the selected ball. This position is on the midx
    // side of the ball, and sets batxdotdot towards this position.
    // If there is a selected ball not covered by the previous
    // statement, it chooses an acceleration towards the ball
    // which continues with this value for some time.
    
    //writef("*nbatx=%8.5d batxdot=%8.5d*n", batx, batxdot)

    // bucket_byc       // Centre level of the bucket base
    // bucket_lxc       // Bucket left side centre
    // bucket_rxc       // Bucket right side centre
    // activebat        // If TRUE the bat is given random accelerations
    // currballno       // = -1, 1, 2 or 3
    // selx, sely       // The position of the selected ball
    // selxdot, selydot // Velocity of the selected bal1
    // batpusttine     // End rel time os bat push
    // batpushxdotdot   // The desired bat speed
    // pushlevel        // Level below which the bat will push.
    
    // If currballno is unset try to select a ball that is not between
    // the bucket sides and above the bucket base.
    //abort(6661)
    
    IF currballno<0 DO
    { sely := -1 // Try to select the highest ball that is not above
                 // the bucket base and between its sides.
      IF cgy1>sely UNLESS cgy1>bucket_byc &
                          bucket_lxc<cgx1<bucket_rxc DO
 			    sely, currballno := cgy1, 1
      IF cgy2>sely UNLESS cgy2>bucket_byc &
                          bucket_lxc<cgx2<bucket_rxc DO
 			    sely, currballno := cgy2, 2
      IF cgy3>sely UNLESS cgy3>bucket_byc &
                          bucket_lxc<cgx3<bucket_rxc DO
 			    sely, currballno := cgy3, 3
    }

    // Choose a target for the bat and its acceleation.
    
    TEST currballno<=0
    THEN { // There is no selected ball, if the bat's acceleration
           // timeout has passed, an acceleration is chosen towards midx
	   // and sets it to remain for some time.
           IF displaytime > batpushtime DO
	   { batpushxdotdot := (midx - batx) * 3 / 4
	     batpushtime := displaytime + 0_200 // + 2 secs
	     writef("Displaytime=%10.3d batpushtime=%10.3d batpushxdotdot=%10.5d*n",
	             displaytime,       batpushtime,       batpushxdotdot)
	   }
	   batxdotdot := batpushxdotdot
         }
    ELSE { // There is a selected ball, find its parameters.
           IF currballno=1 DO
             selx, sely, selxdot, selydot := cgx1, cgy1, cgx1dot, cgy1dot
           IF currballno=2 DO
             selx, sely, selxdot, selydot := cgx2, cgy2, cgx2dot, cgy2dot
           IF currballno=3 DO
             selx, sely, selxdot, selydot := cgx3, cgy3, cgx3dot, cgy3dot

           { LET FLT xbounce = bouncepos(selx, sely, selxdot, selydot)
	     LET FLT tbounce = result2
             writef("displaytime=%9.3f selx=%9.5f sely=%9.5f selxdot=%9.5f selydot=%9.5f*n",
	             displaytime,      selx,      sely,      selxdot,      selydot)
             writef("xbounce x pos = %9.5f  bounce time = %7.3f*n", xbounce, tbounce)
	   }
	   
           // Test the height of the selected ball
           TEST sely < pushlevel
           THEN { // The selected ball is near the gound.
	          // If the ball push time has expired choose a suitable
 	 	  // acceleration.
	          IF displaytime > batpushtime DO
		  { // Setup another push
		    { batpushxdotdot :=  batx < selx -> +502_00000,
		                                        -502_00000
		      batpushtime := displaytime + 0_500 // + 2 secs
		      //writef("displaytime=%10.4d batpushtime=%10.3d batxdotdot=%10.5d*n",
		      //        displaytime,       batpushtime,       batxdotdot)
		    }
		  }
		  batxdotdot := batpushxdotdot
	        }
           ELSE { // There is a selected ball and it is some distance
	          // above the ground.
	          LET tx = ?       // To hold the target x position
    	          TEST selx > midx
	          THEN tx := selx - 8 * ballradius
	          ELSE tx := selx + 8 * ballradius
//writef("selx=%10.5d tx=%10.5d*n", selx, tx)
                  batxdotdot := chooseaccel( tx,   selxdot,
                                             batx, batxdot,
                                             0, // Desired speed when
		                                // reaching tx
		                             4_000 // Time to reach tx
		                           )
	          //abort(7653)
                }
         }
  }

  IF batxdotdot> 550_00000 DO batxdotdot :=  550_00000
  IF batxdotdot<-550_00000 DO batxdotdot := -550_00000

  // Apply forces to the bat
  //sps := 10.0
  batxdot := batxdot + batxdotdot/sps
  batxdot := batxdot - batxdot/sps  // damping
  
  //writef("batx=%9.5d  batxdot=%9.5d  batxdotdot=%9.5d*n",
  //        batx, batxdot, batxdotdot)
  IF batxdot> 600_00000 DO batxdot :=  600_00000
  IF batxdot<-600_00000 DO batxdot := -600_00000

  //IF FALSE DO
  batx := batx + batxdot/sps

  IF batx > wall_rx-batradius DO
    batx, batxdot := wall_rx - batradius, -batxdot
  IF batx < batradius DO
    batx, batxdot := batradius, -batxdot

  // Slowly correct baty
  baty := baty - (baty - batradius)/10.0
}

AND FLT chooseaccel(tx, txdot,
                    bx, bxdot,
		    speed, t) = VALOF
{ // tx   is the current target x position
  // txdot is the current target speed
  // bx       is the current bat x position
  // bxdot    is the cureent bat speed
  // speed    is the desired speed on reaching the target
  // t        is the time suggested to reach the target

  // We need to calculate an acceleration a that is suitable.
  
  // Equations of motion:
  
  // targ_x = tx + txdot*t     // Motion of the target
  // bat_x  = bx + bxdot*t + a * t^2 / 2 // bat with accel a
  // so at time t from now we need
  // targ_x = bat_x
  // ie
  // tx + txdot*t = bx + bxdot*t + a * t^2 / 2
  // ie
  // (tx-bx) + (txdot-bxdot)*t = a * t^2 / 2
  // so
  // a = ((tx-bx) + (txdot-bxdot) * t) * 2 / t^2

  // We will do this calculation using floating point arithmetic.

  //LET FLT scalefactor = 1.0

  LET FLT ftx    = FLOAT tx
  LET FLT ftxdot = FLOAT txdot
  LET FLT fbx    = FLOAT bx
  LET FLT fbxdot = FLOAT bxdot
  LET FLT ft     = FLOAT t     / 1_000.0 // Time in secs
  LET FLT fa = ((ftx-fbx) + (ftxdot-fbxdot)*ft) * 2.0 / (ft*ft)

  //writef("*ndisplaytime=%9.3d*n", displaytime)
IF FALSE DO
  IF displaytime > 10_000 DO // About 10 secs since the latest start
  { writef("*ndisplaytime=%9.3d*n", displaytime)
    writef("*ncurrballno=%n*n", currballno)
    writef("ftx    = %9.5f*n", ftx)
    writef("ftxdot = %9.5f*n", ftxdot)
    writef("fbx    = %9.5f*n", fbx)
    writef("fbxdot = %9.5f*n", fbxdot)
    writef("ft     = %9.5f*n", ft)
    writef("(ftx-fbx)=%9.5f*n", ftx-fbx)
    writef("(ftxdot-fbxdot)=%9.5f*n", ftxdot-fbxdot)
    writef("(ftxdot-fbxdot)**ft=%9.5f*n", (ftxdot-fbxdot)*ft)
    writef("((ftx-fbx)+(ftxdot-fbxdot)**ft)**2.0/(ft**ft)=%9.5f*n",
            ((ftx-fbx)+(ftxdot-fbxdot)*ft)*2.0/(ft*ft))
    writef("fa     = %9.5f*n", fa)

    FOR dt = 0 TO t BY 0_500 DO
    { LET FLT fdt = (FLOAT dt) / 1_000.0
      LET FLT tpos = ftx + ftxdot*fdt
      LET FLT bpos = fbx + fbxdot*fdt + fa * fdt*fdt / 2.0
      writef("fdt=%10.5f tpos=%10.5f bpos=%10.5f*n", fdt, tpos, bpos)
    }
    abort(5676)
  }

//  writef("fa     = %9.5f*n", fa)

  RESULTIS FIX (fa * 300.0)
}

AND FLT bouncepos(FLT x,    FLT y,
                  FLT xdot, FLT ydot) = VALOF
{ // x, y   is the current position of a ball.
  // xdot, ydot is the velocity of the ball.
  // Return the x position of the bounce.
  // Result2 is the time of the bounce.
  
  // The ball moves in a parabola whose equation is

  // h = y + ydot*t - g * t^2 / 2

  // We need to find t when h = ballradius

  // The quadratic equation for t is thus

  // g * t^2 / 2 - ydot*t - y + ballradius = 0

  // ie At^2 + Bt + C = 0
  // where A = g/2
  //       B = -ydot
  //       C = ballradius - y
  // We must choose the larger root so
  
  // tbounce = (-B + sqrt(B%2 - 4AC)) / 2A
  // and
  // xbounce = x + xdot * tbounce
  // This is then corrected approximatly taking account of wall bounces
  // but ignoring bucket collisions.

  LET FLT A = ag / 2.0
  LET FLT B = - ydot
  LET FLT C = ballradius - y
  LET FLT tbounce = ( sys(Sys_flt, fl_sqrt, B*B - 4.0*A*C)) / (2.0*A)
  LET FLT xbounce = x + xdot * tbounce

  // Deal with wall bounces ignoring the bucket

  { LET FLT d = xbounce - (wall_lx+ballradius)
    IF d < 0.0 DO
    { xbounce := wall_lx+ballradius - d
      LOOP
    }
    d := xbounce - (wall_rx-ballradius)
    IF d > 0.0 DO
    { xbounce := wall_rx-ballradius - d
      LOOP
    }
  } REPEAT

  writef("bounce pos =%9.5f  bounce time = %9.5f*n", xbounce, tbounce+displaytime)

  result2 := displaytime + tbounce
  RESULTIS xbounce
}

AND initbucketwallsurf(surfptr) = VALOF
{ // Allocate a surface for the bucket walls
  LET FLT width  = 2.0*endradius + 1.0
  LET FLT height = bucket_tyt - bucket_byb + 2.0
  writef("Calling mksurface width=%10.5f height=%10.5f*n", width, height)
  UNLESS mksurface(FIX width, FIX height, surfptr) RESULTIS FALSE
  selectsurface(surfptr, FIX width, FIX height)
  fillsurf(backcolour)
RESULTIS TRUE
  // Draw the ends
  TEST debugging
  THEN setcolour(bucketendcolour)
  ELSE setcolour(bucketcolour)
  drawfillcircle(FIX endradius, FIX endradius, FIX endradius-1)
  drawfillcircle(FIX endradius, FIX(height-endradius), FIX endradius - 1)

  // Draw the wall
  setcolour(bucketcolour)
  drawfillrect(0, FIX endradius, FIX width, FIX(height-endradius))
  //writef("Return TRUE from initbucketwallsurf*n")
  RESULTIS TRUE
}

AND initbucketbasesurf(surfptr, col) = VALOF
{ // Allocate the bucket base surface
  LET FLT width  = bucket_rxc - bucket_lxc + 1.0
  LET FLT height = 2*endradius + 1.0
  UNLESS mksurface(FIX width, FIX height, surfptr) RESULTIS FALSE
  selectsurface(surfptr, FIX width, FIX height)
  fillsurf(backcolour)
  setcolour(bucketcolour)
  drawfillrect(0, 0, FIX width, FIX height)
  RESULTIS TRUE
}

AND initballsurf(surfptr, col) = VALOF
{ // Allocate a ball surface
  LET FLT height = 2.0*ballradius + 2.0
  LET FLT width  = height
  LET colkey = maprgb(64,64,64)
  
  //UNLESS mksurface(FIX width, FIX height, surfptr) RESULTIS FALSE

  selectsurface(surfptr, FIX width, FIX height)
  //fillsurf(colkey)
  //setcolourkey(surfptr, colkey)

  //setcolour(col)
  //drawfillcircle(FIX ballradius, FIX(ballradius+1.0), FIX ballradius)

  RESULTIS TRUE & FALSE
}

AND initbatsurf(surfptr) = VALOF
{ // Allocate a bat surface
  LET FLT height = 2.0*batradius + 2.9
  LET FLT width  = height
  UNLESS mksurface(FIX width, FIX height, surfptr) RESULTIS FALSE
  selectsurface(surfptr, FIX width, FIX height)
  fillsurf(backcolour)
  setcolour(batcolour)
  drawfillcircle(FIX batradius, FIX(batradius+1.0), FIX batradius)
  RESULTIS TRUE
}

AND plotscreen() BE
{ selectsurface(@screen, screenxsize, screenysize)
  fillsurf(backcolour)
  // Allocate the surfaces if necessary
  abort(7661)
  UNLESS bucketwallsurfok DO
    bucketwallsurfok := initbucketwallsurf(@bucketwallsurf)
  abort(7662)
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
//abort(7660)

  // Left bucket wall
  IF bucketwallsurfok DO
  { writef("calling blitsurf(@bucketwallsurf, @screen, %n, %n)*n",
            FIX bucket_lxl, FIX bucket_tyt)
    abort(1236)
    blitsurf(@bucketwallsurf, @screen, FIX bucket_lxl, FIX bucket_tyt)
  }
abort(7661)

  // Right bucket wall
  IF bucketwallsurfok DO
    blitsurf(@bucketwallsurf, @screen, FIX bucket_rxl, FIX bucket_tyt)
abort(7662)

  // Bucket base
  IF bucketbasesurfok UNLESS starting DO
  { //sawritef("blitsurf of bucket base*n")
    //sawritef("bucketbasesurf=(%n %n)*n", bucketbasesurf, bucketbasesurf1)
    //sawritef("screen=(%n %n)*n", screen, screen1)
    blitsurf(@bucketbasesurf, @screen, FIX bucket_lxc, FIX(bucket_byt-1.0))
  }

abort(7663)

  // The bat
  IF batsurfok DO
  { writef("Calling blitsurf(%n %n %n %n)*n",
            @batsurf, @screen, FIX(batx-batradius), FIX(baty+batradius))
    //abort(7811)
    blitsurf(@batsurf, @screen, FIX(batx-batradius), FIX(baty+batradius))
  }
abort(7664)

  // Finally, the three balls
  setcolour(ball1colour)
  IF ball1surfok DO
  { blitsurf(@ball1surf, @screen, FIX(cgx1-ballradius), FIX(cgy1+ballradius))
    IF currballno=1 DO
    { setcolour(#x00FFFFFF)
//      writef("calling drawrect(%n, %n, %n, %n)*n",
//              FIX cgx1-5, FIX cgy1-5, FIX cgx1+5, FIX cgy1+5)
      drawfillrect(FIX cgx1-5, FIX cgy1-5, FIX cgx1+5, FIX cgy1+5)
    }
  }
abort(7665)

  setcolour(ball2colour)
  IF ball2surfok DO
  { blitsurf(@ball2surf, @screen, FIX(cgx2-ballradius), FIX(cgy2+ballradius))
    IF currballno=2 DO
    { setcolour(#x00FFFFFF)
//      writef("calling drawrect(%n, %n, %n, %n)*n",
//              FIX cgx2-5, FIX cgy2-5, FIX cgx2+5, FIX cgy2+5)
      drawfillrect(FIX cgx2-5, FIX cgy2-5, FIX cgx2+5, FIX cgy2+5)
    }
  }
abort(7666)

  setcolour(ball3colour)
  IF ball3surfok DO
  { blitsurf(@ball3surf, @screen, FIX(cgx3-ballradius), FIX(cgy3+ballradius))
    IF currballno=3 DO
    { setcolour(#x00FFFFFF)
//      writef("calling drawrect(%n, %n, %n, %n)*n",
//              FIX cgx3-5, FIX cgy3-5, FIX cgx3+5, FIX cgy3+5)
      drawfillrect(FIX cgx3-5, FIX cgy3-5, FIX cgx3+5, FIX cgy3+5)
    }
  }
abort(7667)

  setcolour(maprgb(255,255,255))
  

  IF finished DO
  { drawf(30, 370, "Finished -- Well Done!")
    currballno := -1
    done := TRUE
  }
    

  IF started | finished DO
    drawf(30, 350, "Time %9.2d", displaytime/10)

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
    drawf(30, 245, "CPU usage = %i3%% sps = %8.5f", usage, sps)

  IF debugging DO
  { drawf(30, 220, "Ball1 x=%10.5f  y=%10.5f xdot=%10.5f  ydot=%10.5f",
          cgx1, cgy1, cgx1dot, cgy1dot)
    drawf(30, 205, "Ball2 x=%10.5f  y=%10.5f xdot=%10.5f  ydot=%10.5f",
          cgx2, cgy2, cgx2dot, cgy2dot)
    drawf(30, 190, "Ball3 x=%10.5f  y=%10.5f xdot=%10.5f  ydot=%10.5f",
          cgx3, cgy3, cgx3dot, cgy3dot)
    drawf(30, 175, "Bat   x=%10.5f  y=%10.5f xdot=%10.5f",
          batx, baty, batxdot)
  }
//writef("plotscreen: returning*n")
abort(7668)
}

AND resetballs() BE
{ cgy1 := bucket_byt+ballradius   + 10.0
  cgy2 := bucket_byt+3*ballradius + 20.0
  cgy3 := bucket_byt+5*ballradius + 30.0
  cgx1 := screen_xc
  cgx2 := screen_xc
  cgx3 := screen_xc
  cgx1dot, cgx2dot, cgx3dot :=  0.0, 0.0, 0.0
  cgy1dot, cgy2dot, cgy3dot :=  0.0, 0.0, 0.0

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
		batpushtime := 0
                LOOP

      CASE 'P': // Toggle stepping
                stepping := ~stepping
                LOOP

      CASE '\': // Single steo
                stepping := TRUE
                singlestepping := TRUE


      CASE 'T': // Print the current state.
                newline()
                writef("cgx1=%8.5f cgx1dot=%8.5f*n", cgx1, cgx1dot)
                writef("cgx2=%8.5f cgx2dot=%8.5f*n", cgx2, cgx2dot)
                writef("cgx3=%8.5f cgx3dot=%8.5f*n", cgx1, cgx3dot)
                writef("currbalno=%n selx=%8.5f sely=%8.5f*n", currballno, selx, sely)
	        writef("pushlevel=%8.5f*n", pushlevel)	
                writef("selxdot=%8.5f selydot=%8.5f batpushxdotdot=%8.5f*n",
		        selxdot,      selydot,      batpushxdotdot)
                writef("displaytime=%9.3d batpushtime=%9.2d*n", displaytime, batpushtime)
		LOOP

      CASE 'R': // Reset the balls
                resetballs()
                finished := FALSE
                starting := FALSE
                displaytime := -1
                LOOP
		
      CASE sdle_arrowright:
                batxdotdot := batxdotdot + 750.0
		activebat := FALSE
		LOOP
      CASE sdle_arrowleft:
                batxdotdot := batxdotdot - 750.0
		activebat := FALSE
		LOOP
    }

  CASE sdle_keyup:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:  LOOP

      CASE sdle_arrowright:
                batxdotdot := batxdotdot - 750.0
		activebat := FALSE
		LOOP
      CASE sdle_arrowleft:
                batxdotdot := batxdotdot + 750.0
		activebat := FALSE
		LOOP
    }


  CASE sdle_quit:
    writef("QUIT*n");
    done := TRUE
    LOOP
}

LET start() = VALOF
{ LET FLT stepmsecs = ?
  LET comptime  = 0 // Amount of cpu time per frame

  randseed := 12345
  
  One :=    1-0     // The constant 1.0 scaled with 5 decimal
                    // digits after the decimal point.
  OneK := 1000.0

  batradius       := 10.0
  ballradius      := 25.0
  endradius       := 15.0
  bucketthickness := 2.0 * endradius
  pushlevel       := ballradius*6  // Level below which th bat pushes
  ag              := 50.0          // Gravity acceleration

  UNLESS sys(Sys_sdl, sdl_avail) DO
  { writef("*nThe SDL features are not available*n")
    RESULTIS 0
  }

  ballselecttime  := 0
  batpushtime     := 0

  batpushxdotdot  := 0.0 // Ball accelerationd until displaytime > ballpushtime
  
  bucketwallsurfok := FALSE
  bucketbasesurfok := FALSE
  ball1surfok      := FALSE
  ball2surfok      := FALSE
  ball3surfok      := FALSE
  batsurfok        := FALSE

  IF FALSE DO
  { // Code to test the cosines function
    LET e1, e2 = 1.0, 1.0
    FOR i = 0 TO 100 DO
    { LET FLT dy = FLOAT i / 100.0
      LET FLT c, FLT s, FLT rsq = ?, ?, ?
      c := cosines(1.0, dy)
      s := result2
      rsq := c*c + s*s
      writef("dx=%9.5f  dy=%9.5f cos=%9.5f sin=%9.5f rsq=%9.5f*n",
              1.0, dy, c, s, rsq)
      IF e1 < rsq DO e1 := rsq
      IF e2 > rsq DO e2 := rsq
    }
    writef("Errors +%6.5f  -%7.5f*n", e1-1.0, 1.0-e2)
    RESULTIS 0
  }

  initsdl()
  mkscreen("Ball and Bucket", 800, 500)
  //mkscreen("Ball and Bucket", 600, 400)

  help := TRUE & FALSE

  activebat := TRUE & FALSE
  currballno := -1
  
  stepping       := TRUE     // =FALSE if not stepping
  singlestepping := FALSE    // =FALSE if not single stepping
  starting       := TRUE     // Trap door open
  started        := FALSE
  finished       := FALSE
  debugging      := FALSE
  
  starttime    := -1
  displaytime  := -1

  usage        := 0
  displayusage := FALSE
  sps          := 40.0 // Initial setting
  stepmsecs    := 1000.0/sps

  backcolour      := maprgb(120, 120, 120)
  bucketcolour    := maprgb(170,  60,  30)
  bucketendcolour := maprgb(140,  30,  30)
  ball1colour     := maprgb(255,   0,   0)
  ball2colour     := maprgb(  0, 255,   0)
  ball3colour     := maprgb(  0,   0, 255)
  batcolour       := maprgb( 40,  40,  40)

/*
                        screen_xc
wall_lx                 midx                       wall_rx
|                       |                          |
|--------------------------------------------------|-- ceiling_yb   ) ballradius
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
|    :   :-------------:--:-------------:-----:----|-- ylim_basec )
|    :   :             :  xlim_bucket-rl:     :    |
|    :   :             xlim_bucket_rl   :     :    |
|    :   :                              :     :    |
|    :   :                              :     :    |
|----:---:------------------------------:-----:----|-- ylim_floor ) ballradius
|----:---:------------------------------:-----:----|-- floor_yb   )
|    :   :                              :     :    |
|    :   xlim_bucket_ll                 :     :    wakk_rx
|    xlim_lwall                         :     xlim_rwall
wall_lx                                 xlim_bucket_rr
                                          
*/

  wall_lx := 0.0
  wall_rx := FLOAT(screenxsize-1)     // Right wall
  midx := (wall_lx+wall_rx)/2.0
  
  floor_yt   := 0.0                   // Floor
  ceiling_yb := FLOAT(screenysize-1)  // Ceiling

  screen_xc  := FLOAT(screenxsize / 2)

  bucket_lxr := screen_xc  - ballradius * 2.5
  bucket_lxc := bucket_lxr - endradius
  bucket_lxl := bucket_lxr - bucketthickness

  bucket_rxl := screen_xc  + ballradius * 2.5
  bucket_rxc := bucket_rxl + endradius
  bucket_rxr := bucket_rxl + bucketthickness

  bucket_tyt := ceiling_yb - 6.0*ballradius
  bucket_tyc := bucket_tyt - endradius
  bucket_tyb := bucket_tyt - bucketthickness

  bucket_byt := bucket_tyt - 6.0*ballradius
  bucket_byc := bucket_byt - endradius
  bucket_byb := bucket_byt - bucketthickness

  xlim_lwall     := wall_lx    + ballradius
  xlim_rwall     := wall_rx    - ballradius
  ylim_floor     := floor_yt   + ballradius
  ylim_ceiling   := ceiling_yb - ballradius
  xlim_bucket_ll := bucket_lxl - ballradius
  xlim_bucket_lr := bucket_lxr + ballradius
  xlim_bucket_rl := bucket_rxl - ballradius
  xlim_bucket_rr := bucket_rxr + ballradius

  ylim_topt      := bucket_tyt + ballradius
  ylim_baseb     := bucket_byb - ballradius
  ylim_baset     := bucket_byt + ballradius

  resetballs()

  ax1, ay1 := 0.0, 0.0   // Acceleration of ball 1
  ax2, ay2 := 0.0, 0.0   // Acceleration of ball 2
  ax3, ay3 := 0.0, 0.0   // Acceleration of ball 3

  batx := screen_xc  // Position of bat
  baty := floor_yt + batradius   // Position of bat
  ylim_bat := floor_yt + batradius + ballradius

  batxdot    := 150.0    // Velocity of bat
  batydot    := 0.0      // Velocity of bat
  batxdotdot := 0.0      // Acceleration of bat

  done := FALSE
  
  UNTIL done DO
  { LET t0 = sdlmsecs()
    LET t1 = ?
writef("About to call processevents()*n")
//stepping := FALSE
    processevents()

    IF FALSE DO
    IF currballno<0 DO
    { batxdotdot := (midx - batx) / 4
      batxdot := batxdot + batxdotdot/sps
      batx := batx + batxdot/sps
    }
    
    IF stepping DO step()
    
    IF singlestepping DO
    { stepping       := FALSE
      singlestepping := FALSE
    }

    usage := 100*comptime/(stepmsecs|1)
    writef("About to call plotscreen()*n")
    abort(1234)
    plotscreen()
    abort(1235)
    //writef("returned from plotscreen()*n")
    writef("About to call updatescreen()*n*n")
    updatescreen()
    //writef("Returned from updatescreen()*n")
    //writef("usage=%n currballno=%n*n", usage, currballno)
    UNLESS 80<usage<95 DO
    { TEST usage>90
      THEN sps := sps-1
      ELSE sps := sps+1
      IF sps<5.0   DO sps :=   5.0
      IF sps>100.0 DO sps := 100.0
      stepmsecs := 1000.0/sps
    }

    t1 := sdlmsecs()
    //writef("Returned from sdlmsecs()*n")

    comptime := t1 - t0
    //IF t0+stepmsecs > t1 DO sdldelay(t0+stepmsecs-t1)
    //writef("Calling sdldelay(100)*n")
//sdldelay(100)
//writef("End of 500msec delay*n")
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



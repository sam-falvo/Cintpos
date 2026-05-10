/*
This is a program that displays some robots attempting to
pick up bottles with their grabbers and deposit them in a pit.

Implemented by Martin Richards (c) February 2015

History:

08/12/2016
Trying a new algorithm for robot-robot collision avoidance.

27/11/2016
Currently teaching the robots to catch and dispose of the bottles.

02/02/2015
Initial implementation started based on bucket.b.

*/

SECTION "sdllib"
GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the library source code
.
SECTION "robots"
GET "libhdr"
GET "sdl.h"

MANIFEST {
  // Most arithmetic uses scaled numbers with 3 digits
  // after the decimal point.

  One  =    1_000 // The constant 1.000 scaled with 3 decimal
                  // digits after the decimal point.
  OneK = 1000 * One

  spacevupb = 100000

  pitradius       = 50_000
  bottleradius    =  5_000
  robotradius     = 18_000
  shoulderradius  =  4_000
  tipradius       =  2_000   // Tip of grabber arm
  armthickness    = 2*tipradius
  grablen         = 12_000
  edgesize        = 60_000

  grabbedpos      = bottleradius /     // (Typically = 0_500)
                    ((robotradius - shoulderradius - 2*tipradius)/One)

  //#########################    Robot geometry    #########################
  //
  //                   Y
  //                   ^                 shoulder
  //                   |                /
  //               + + + + +     + + + /               tip
  //          +        |       +   |   +              /
  //        +          |      +  + o----a + + + + + d/      ------------- y = r1-r2
  //      +            |       +   ++  ++           p +     |d1 = 2 x r3
  //    +              r1        + + ++ b + + + + + c       ------------- y = r1-r2-d1
  //   +    left       |           +   ++       ^
  //  +                |           +    +       d2 = (r1-r2-d1) x grabpos/One
  //  +                |           +    +       v              0.1<=grabpos<=1.0
  //  +------r1--------O-----------+----+-----q------------> X
  //  +                |           +    +     q = (r1+2xr4,0) centre of grabbed bottle
  //  +                |           +    +              
  //   +    right      |           +   ++                 r1 = robotradius    = 18.0
  //    +              |         + + ++ b + + + + + c     r2 = shoulderradius =  4.0
  //      +            |       +   ++  ++           p +   r3 = tipradius      =  2.0
  //        +          |      +  + o----a + + + + + d     r4 = bottleradius   =  5.0
  //          +        |       +   |   +
  //               + + + + +     + + +  |<---d3---->|     d3 = grablen        = 12.0

  // Bottle field selectors
  b_cgx=0; b_cgy         // The first four must be in positions 0 to 3
  b_cgxdot; b_cgydot
  b_grabbed   // If grabbed, b_robot is the grabbing robot
  b_robot     // 0 or the robot that selected this bottle
  b_dropped   // If true, the bottle has fallen into the pit
  b_id        // The bottle number

  b_upb=b_id
  b_size      // Number of elements in a bottle node

  // robot selectors
  r_cgx=0;     r_cgy       // The first four must be in positions 0 to 3
  r_cgxdot;    r_cgydot
  r_grabpos;   r_grabposdot
  r_bottle                 // =0 or the selected bottle
  r_inarea                 // =TRUE if the bottle in the grabber area
                           // and the grabber is closing
                           // if another bottle is found to be in the
                           // grabber area, the grabber opens and if the
                           // selected bottle was grabbed it is released.

  // Coordinates of the robot shoulders
  r_lex;  r_ley;   r_rex;  r_rey  //    le       re
  r_lcx;  r_lcy;   r_rcx;  r_rcy  //    lc       rc

  // Coords of the robot arms
  r_ltax; r_ltay;  r_rtax; r_rtay //  ltd ltp ltc   rtc rtp rtd
  r_ltbx; r_ltby;  r_rtbx; r_rtby //
  r_ltcx; r_ltcy;  r_rtcx; r_rtcy //
  r_ltdx; r_ltdy;  r_rtdx; r_rtdy //
  r_ltpx; r_ltpy;  r_rtpx; r_rtpy //  lta     ltb   rtb     rta

  r_bcx;  r_bcy  // Centre of the grabber.

  r_id           // The robot number

  r_upb=r_id
  r_size        // Number of elements in a robot node
}

GLOBAL {
  done:ug
  debugging
  help         // Display help information
  stepping     // =FALSE if not stepping
  finished

  usage
  displayusage
  debugging

  sps             // Steps per second, adjusted automatically
  
  bottles         // Number of bottles
  bottlev         // Vector of bottles
                  // bottlev!0 holds the current number of bottles
  robots          // Number of robots
  robotv          // Vector of robots
                  // robotv!0 holds the current number of robots

  // coords of the pit centre
  pit_x; pit_y; pit_xdot; pit_ydot
  thepit // -> [ pitx, pity, pit_xdot, pit_ydot]
  xsize  // Window size in pixels
  ysize
  seed

  spacev; spacep; spacet
  mkvec

  bottlecount     // Number of bottles not yet in the pit
  freebottles     // Number of free bottles -- not selected or dropped

  bottlesurfR     // Surface for a red bottle
  bottlesurfR1
  bottlesurfRok
  
  bottlesurfDR    // Surface for a dark red grabbed bottle
  bottlesurfDR1
  bottlesurfDRok

  bottlesurfK     // Surface for a black bottle (number 1)
  bottlesurfK1
  bottlesurfKok

  bottlesurfB     // Surface for a brown bottle (grabbed)
  bottlesurfB1
  bottlesurfBok
  
  pitsurf         // Surface for the bucket base
  pitsurf1
  pitsurfok
  
  backcolour      // Background colour
  col_red; col_black; col_brown
  col_darkred; col_darkblue; col_darkgreen
  col_gray1; col_gray2; col_gray3; col_gray4

  pitcolour
  robotcolour
  robot1colour
  grabbercolour

  wall_wx         // West wall x coordinate
  wall_ex         // East wall x coordinate
  wall_sy         // South wall y coordinate
  wall_ny         // North wall y coordinate

  priq        // Heap structure for the time queue
  priqn       // Number of items in priq
  priqupb     // Upb of priq

  msecsnow    // Updated by step, possibly releasing
              // events in the priority queue
  msecs0      // Starting time since midnight
}

LET mkvec(upb) = VALOF
{ LET p = spacep
  spacep := spacep+upb+1
  IF spacep>spacet DO
  { writef("Insufficient space*n")
    abort(999)
    RESULTIS 0
  }
  //writef("mkvec(%n) => %n*n", upb, p)
  RESULTIS p
}

AND mk2(a, b) = VALOF
{ LET p = mkvec(1)
  p!0, p!1 := a, b
  RESULTIS p
}

AND incontact(p1, p2, dist) = VALOF
{ // This returns TRUE if points p1 and p2 are no more than dist apart.
  LET dx = ABS(p1!0-p2!0)
  LET dy = ABS(p1!1-p2!1)

//writef("incontact: x1=%9.3d  y1=%9.3d*n", p1!0, p1!1)
//writef("incontact: x2=%9.3d  y2=%9.3d*n", p2!0, p2!1)
//writef("incontact: dx=%9.3d  dy=%9.3d dist=%9.3d*n", dx, dy, dist)
  IF dx > dist | dy > dist DO
  { //writef("=> FALSE*n");
    //abort(9104)
    RESULTIS FALSE
  }
//writef("dx^2   =%12.3d*n", muldiv(dx,dx,One))
//writef("dy^2   =%12.3d*n", muldiv(dy,dy,One))
//writef("dist^2 =%12.3d*n", muldiv(dist,dist,One))
//abort(9102)
  IF muldiv(dx,dx,One) + muldiv(dy,dy,One) >
     muldiv(dist,dist,One) DO
  { //writef("=> FALSE*n")
    //abort(9105)
    RESULTIS FALSE
  }
//writef("=> TRUE*n")
//abort(9103)
  RESULTIS TRUE
}

AND cbounce(p1, p2, m1, m2) BE
{ // p1!0 and p1!1 are the x and y coordinates of a circular object.
  // p1!2 and p1!3 are the corresponding velocities
  // p1!4 and p1!5 are the corresponding direction cosines
  // p2!0 and p2!1 are the x and y coordinates of the other circular object.
  // p2!2 and p2!3 are the corresponding velocities
  // p2!4 and p3!5 are the corresponding direction cosines
  // m1 and m2 are the masses of the two objects in arbitrary units
  // m1=m2  if the collition is between two bottles or two robots.
  // m1=5 and m2=1 then p1 is a robot and p2 is a bottle.
  // m1=1 and m2=0 then p1 is an infinitely heavy robot or grabbed bottle
  //                and p2 is a bottle.

  LET c = cosines(p2!0-p1!0, p2!1-p1!1) // Direction from p1 to p2
  LET s = result2

  IF m2=0 DO
  { // Object 1 is a robot or a grabbed bottle and object 2 is a bottle.
    // The robots or grabbed bottle is treated as infinitely heavy.
    LET xdot = p2!2 - p1!r_cgxdot
    LET ydot = p2!3 - p1!r_cgydot
    // Transform to (t,w) coordinates
    // where t is in the direction from the robot to the bottle
    LET tdot = inprod(xdot,ydot,  c, s)
    LET wdot = inprod(xdot,ydot, -s, c)

//writef("robot-bottle bounce tdot=%9.3d wdot=%9.3d*n", tdot, wdot)
    IF tdot>0 RETURN // The robot and bottle are moving apart

    // The bottle is getting closer so reverse tdot (but not wdot)
    // and transform back to world (x,y) coordinates.
    tdot := rebound(tdot) // Reverse tdot with some loss of energy
    // Transform back to real world (x,y) coordinates
    p2!2 := inprod(tdot, wdot, c, -s) + p1!r_cgxdot
    p2!3 := inprod(tdot, wdot, s,  c) + p1!r_cgydot
    // Note that the robot or grabbed bottle motion is not changed.
    RETURN
  }

  IF m1=m2 DO
  { // This deals with bottle-bottle and robot-robot bounces.
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

    IF t1dot<=0 RETURN // The objects are moving apart

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

    // Apply a small repulsive force between the objects.

    p1!0 := p1!0 - muldiv(0_400, c, One)
    p1!1 := p1!1 - muldiv(0_400, s, One)
    p2!0 := p2!0 + muldiv(0_400, c, One)
    p2!1 := p2!1 + muldiv(0_400, s, One)

    RETURN
  }

  { // m1~=m2 and neither are zero.
    // Object 1 is a robot and object 2 is a bottle
    // and the robot is not infinitely heavy.
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

IF FALSE DO
{ 
  writef("dir  =(%10.3d,%10.3d)*n", c, s)
  writef("p1   =(%10.3d,%10.3d)*n", p1!0, p1!1)
  writef("p2   =(%10.3d,%10.3d)*n", p2!0, p2!1)
  writef("p1dot=(%10.3d,%10.3d) m1=%n*n", p1!2, p1!3, m1)
  writef("p2dot=(%10.3d,%10.3d) m2=%n*n", p2!2, p2!3, m2)
  writef("cgdot=(%10.3d,%10.3d)*n", cgxdot, cgydot)
  writef("r1dot=(%10.3d,%10.3d)*n", rx1dot, ry1dot)
  writef("r2dot=(%10.3d,%10.3d)*n", rx2dot, ry2dot)
  writef("t1dot=(%10.3d,%10.3d)*n", t1dot, w1dot)
  writef("t2dot=(%10.3d,%10.3d)*n", t2dot, w2dot)
  writef("t1dot=%10.3d is the speed towards the centre of gravity*n", t1dot)
  abort(1000)
}
    IF t1dot<=0 RETURN // The robot and bottle are moving apart

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
    p1!3 := ry1dot + cgydot

    p2!2 := rx2dot + cgxdot
    p2!3 := ry2dot + cgydot
  }
}

AND rebound(vel) = vel/8 - vel // Returns the rebound speed of a bounce

AND cosines(x, y) = VALOF
{ // This function returns the cosine and sine of the angle between
  // the line from (0,0) to (x, y) and the x axis.
  // The result is the cosine and result2 is the sine.
  LET c, s, a = ?, ?, ? 
  LET d = ABS x + ABS y
  UNLESS d DO
  { result2 := 0
    RESULTIS One
  }
  c := muldiv(x, One, d)  // Approximate cos and sin
  s := muldiv(y, One, d)  // Direction good, length not.
  a := muldiv(c,c,One)+muldiv(s,s,One) // 0.5 <= a <= 1.0
  d := 1_000 // With this initial guess only 3 iterations
             // of Newton-Raphson are required.
//writef("a=%8.3d  d=%8.3d  d^2=%8.3d*n", a, d, muldiv(d,d,One))
  d := (d + muldiv(a, One, d))/2
//writef("a=%8.3d  d=%8.3d  d^2=%8.3d*n", a, d, muldiv(d,d,One))
  d := (d + muldiv(a, One, d))/2
//writef("a=%8.3d  d=%8.3d  d^2=%8.3d*n", a, d, muldiv(d,d,One))
  d := (d + muldiv(a, One, d))/2
//writef("a=%8.3d  d=%8.3d  d^2=%8.3d*n", a, d, muldiv(d,d,One))

  s := muldiv(s, One, d) // Corrected cos and sin
  c := muldiv(c, One, d)
//writef("x=%8.3d  y=%8.3d => cos=%8.3d sin=%8.3d*n", x, y, c, s)
//abort(3589)
  result2 := s
  RESULTIS c
}

AND inprod(dx, dy, c, s) = muldiv(dx, c, One) + muldiv(dy, s, One)

LET step() BE
{ // This function deals with the motion of all the robots and bottles
  // and their interractions with each other and the wall and the pit.

  msecsnow := sdlmsecs() - msecs0
  // Deal with crossing midnight assuming now is no more than
  // 24 hours since the start of the run.
  IF msecsnow<0 DO msecsnow := msecsnow + (24*60*60*1000)

  //writef("step: entered*n")
  //IF bottlecount=0 DO finished := TRUE

  // Robots always point in their directions of motion given by
  // cgxdot and cgydot. A robot with a selected bottle will rotate
  // towards its bottle and be given sufficient speed it to catch
  // it up.  Interaction between robots and the walls, the pit,
  // and other robots affect cgxdot and cgydot.

  //abort(9001)

  // (1) Deal with robot bounces and collisions with the walls and
  //     pit slope.

  FOR rib = 1 TO robotv!0 DO
  { LET r = robotv!rib
    LET x, y = r!r_cgx, r!r_cgy

    LET dw = x - wall_wx  // Distance from west wall
    AND dn = wall_ny - y  // Distance from north wall
    AND de = wall_ex - x  // Distance from east wall
    AND ds = y - wall_sy  // Distance from south wall

    // Limit the speed of the robot

    IF ABS r!r_cgxdot > 40_000 | ABS r!r_cgydot > 40_000 DO
      r!r_cgxdot, r!r_cgydot := r!r_cgxdot*97/100, r!r_cgydot*97/100

    // Ensure the robot is always moving.

    WHILE ABS r!r_cgxdot + ABS r!r_cgydot < 1_000 DO
    { //sawritef("R%i2: Random nudge: xdot=%8.3d ydot=%8.3d*n",
      //          r!r_id, r!r_cgxdot, r!r_cgydot)
      r!r_cgxdot := r!r_cgxdot + randno(201) - 100
      r!r_cgydot := r!r_cgydot + randno(201) - 100
    }

    // Test if the robot is closest to the west wall
    IF dw<edgesize & dw<=dn & dw<=ds DO
    { // (x,y) is closest to the west wall
      TEST dw < robotradius
      THEN r!r_cgxdot, r!r_cgx := -r!r_cgxdot, wall_wx + robotradius
      ELSE r!r_cgxdot := r!r_cgxdot + 4_000
    }

    // Test if the robot is closest to the north wall
    IF dn<edgesize & dn<=de & dn<=dw DO
    { // (x,y) is closest to the north wall
      TEST dn < robotradius
      THEN r!r_cgydot, r!r_cgy := -r!r_cgydot, wall_ny - robotradius
      ELSE r!r_cgydot := r!r_cgydot - 4_000
    }

    // Test if the robot is closest to the east wall
    IF de<edgesize & de<=ds & de<=dn DO
    { // (x,y) is closest to the east wall
      TEST de < robotradius
      THEN r!r_cgxdot, r!r_cgx := -r!r_cgxdot, wall_ex - robotradius
      ELSE r!r_cgxdot := r!r_cgxdot - 4_000
    }

    // Test if the robot is closest to the south wall
    IF ds<edgesize & ds<=de & ds<=dw DO
    { // (x,y) is closest to the south wall
      TEST ds < robotradius
      THEN r!r_cgydot, r!r_cgy := -r!r_cgydot, wall_sy + robotradius
      ELSE r!r_cgydot := r!r_cgydot + 4_000
    }

    IF incontact(r, thepit, pitradius+edgesize) DO
    { // If the robot is on the pit slope.
      LET c = cosines(x-pit_x, y-pit_y)
      LET s = result2
      r!r_cgxdot := r!r_cgxdot + inprod(1_000,0, c,-s)
      r!r_cgydot := r!r_cgydot + inprod(1_000,0, s, c)
    }
  }

  // (2) Deal with bottle bounces and collisions with the walls,
  //     pit slope. Drop bottles that are above the pit and
  //     decrement bottlecount and freebottles appropriately.
  //     If the bottle was owned start opening its start
  //     opening owner's grabber, if not fully open.

  FOR bid = 1 TO bottlev!0 DO
  { LET b = bottlev!bid

    IF b!b_dropped LOOP

    // Limit the speed of the bottle

    IF ABS b!b_cgxdot > 35_000 | ABS b!b_cgydot > 35_000 DO
      b!b_cgxdot, b!b_cgydot := b!b_cgxdot*97/100, b!b_cgydot*97/100

    // Test if the bottle is within the pit slope circle.

    IF incontact(b, thepit, pitradius+edgesize) DO
    { // The bottle is within the pit slope circle. 

      IF incontact(b, thepit, pitradius-bottleradius) DO
      { // The bottle is actually above the pit so must be dropped.

        // Note that freebottles is the count of how many bottles are
        // neither selected nor dropped.
        // bottlecount is the number of bottles that have not yet dropped.

        LET owner = b!b_robot // Find the owner, if any.

        IF owner DO
        { owner!r_bottle := 0              // Deselect the bottle.
          owner!r_inarea := FALSE          // Only TRUE if the selected bottle
                                           // is in the area.
          UNLESS owner!r_grabpos= 1_000 DO // Start opening the owner's
            owner!r_grabposdot := +0_600   // grabber if necessary.
          b!b_grabbed := FALSE             // Ensure the bottle is not grabbed.
          b!b_robot := 0                   // The bottle has no owner.
          freebottles := freebottles + 1   // The bottle is no longer owned.
        }

        // The bottle had no owner and is being dropped into the pit
        // so decrement freebottles and bottlecount
        freebottles := freebottles - 1
        bottlecount := bottlecount - 1
        b!b_dropped := TRUE

        LOOP // This bottle has gone, so consider another bottle, if any.
      }

      // The bottle is not above the pit but is on the pit slope.

      { // Deal with bottle-pit slope interactions
        // Calculate the direction from the pit centre to the bottle.
        LET dx = cosines(b!b_cgx-pit_x, b!b_cgy-pit_y)
        LET dy = result2
//writef("B%i2: dx=%10.3d dy=%10.3d  dx=%10.3d dy=%10.3d*n",
//        bid, b!b_cgx-pit_x, b!b_cgy-pit_y, dx, dy)

        // Apply a constant force away from the pit centre.
        b!b_cgxdot := b!b_cgxdot + muldiv(10_000, dx, One)
        b!b_cgydot := b!b_cgydot + muldiv(10_000, dy, One)
      }

      // This bottle is within the pit slope circle so cannot be
      // on a wall edge.
      LOOP
    }

    // This bottle may be near a wall edge.

    { LET x = b!b_cgx
      LET y = b!b_cgy

      // Bottle interaction with the walls
      LET dw = x - wall_wx  // Distance from west wall
      AND dn = wall_ny - y  // Distance from north wall
      AND de = wall_ex - x  // Distance from east wall
      AND ds = y - wall_sy  // Distance from south wall

      // Test if the bottle closest to the west wall.
      IF dw<edgesize & dw<=dn & dw<=ds DO
      { // (x,y) is closest to the west wall
        TEST dw < bottleradius
        THEN b!b_cgxdot, b!b_cgx := -b!b_cgxdot, wall_wx + bottleradius
        ELSE b!b_cgxdot := b!b_cgxdot + 20_000
      }

      // Test if the bottle closest to the north wall.
      IF dn<edgesize & dn<=de & dn<=dw DO
      { // (x,y) is closest to the north wall
        TEST dn < bottleradius
        THEN b!b_cgydot, b!b_cgy := -b!b_cgydot, wall_ny - bottleradius
        ELSE b!b_cgydot := b!b_cgydot - 20_000
      }

      // Test if the bottle closest to the east wall.
      IF de<edgesize & de<=ds & de<=dn DO
      { // (x,y) is closest to the east wall
        TEST de < bottleradius
        THEN b!b_cgxdot, b!b_cgx := -b!b_cgxdot, wall_ex - bottleradius
        ELSE b!b_cgxdot := b!b_cgxdot - 20_000
      }

      // Test if the bottle closest to the south wall.
      IF ds<edgesize & ds<=de & ds<=dw DO
      { // (x,y) is closest to the south wall
        TEST ds < bottleradius
        THEN b!b_cgydot, b!b_cgy := -b!b_cgydot, wall_sy + bottleradius
        ELSE b!b_cgydot := b!b_cgydot + 20_000
      }
    }
    // Consider another bottle, if any.
  }

  // (3) Deal with robot-robot bounces and collision avoidance.

  FOR rid1 = 1 TO robotv!0 DO
  { // Test for robot-robot interaction -- collision avoidance and bouncing.
    LET r1 = robotv!rid1
    LET x1, y1 = r1!r_cgx, r1!r_cgy

    FOR rid2 = rid1+1 TO robotv!0 DO
    { LET r2 = robotv!rid2  // Another robot
      LET x2, y2 = r2!r_cgx, r2!r_cgy

      IF incontact(r1, r2, 12*robotradius) DO
      { // These two robots are close enough for collision avoidance
        // to be applied, or possibly perform a simple bounce.
        //sawritef("R%i2 is in avoidance range with R%i2*n", rid1, rid2)

        // But if they are touching perform a simple bounce.
        TEST incontact(r1, r2, 2*robotradius)
        THEN { // The robots are in contact so perform a simple bounce.
               //sawritef("R%i2 is bouncing off R%i2*n", rid1, rid2)
               cbounce(r1, r2, 1, 1)
               // cbounce does not move the robots
//abort(9109)
             }
        ELSE { // The robots are in range and not touching
               // so perform collision avoidance adjustment,
               // if necessary.

               LET dx = x2-x1  // Position of r2 relative to r1
               LET dy = y2-y1

               // Subtract the velocity of r2 from both r1 and r2
               // effectively make r2 stationary.
               LET relvx = r1!r_cgxdot - r2!r_cgxdot
               LET relvy = r1!r_cgydot - r2!r_cgydot

               // Compute the direction cosines of the relative velocity
               LET c = cosines(relvx, relvy)
               LET s = result2

               // Rotate about r to make the relative velocity lie in
               // the X axis, and calculate where this will leave r2.
               LET sepx = muldiv(c, dx, One) + muldiv(s, dy, One)
               AND sepy = muldiv(c, dy, One) - muldiv(s, dx, One)

               // sepx is the distance to travel before reaching the closest
               //      approach
               // sepy is the closest approach distance.

               IF rid1=-1 DO
               { writef("R%n: is avoidance range with R%n*n", rid1, rid2)
                 writef("R%n:  cg (%8.3d,%8.3d)      velocity = (%8.3d,%8.3d)*n",
                         rid1, x1, y1,  r1!r_cgxdot, r1!r_cgydot)
                 writef("R%n:  cg (%8.3d,%8.3d)      velocity = (%8.3d,%8.3d)*n",
                         rid2, x2, y2, r2!r_cgxdot, r2!r_cgydot)
                 writef("(dx,dy)=(%8.3d,%8.3d)  Rel velocity = (%8.3d,%8.3d)*n",
                         dx, dy, relvx, relvy)
                 writef("Rel velocity direction cosines         (%8.3d,%8.3d)*n",c,s)
                 writef("sepx = %8.3d  sepy = %8.3d minsep = %8.3d*n",
                         sepx, sepy, 6*robotradius)

                 abort(9100)
               }

               IF rid1=-1 & sepx>0 DO
                 writef("R%i2 and R%i2: ABS sepy = %9.3d  6**robotradius=%9.3d*n",
                         rid1, rid2, ABS sepy, 6*robotradius)

               IF sepx>0 & ABS sepy < 6*robotradius DO
               { // The robots are getting closer and will get too close
                 // so an adjustment must be made
                 // The forces depend on the robot's speed
                 LET f1 = (ABS r1!r_cgxdot + ABS r1!r_cgydot)*12/100
                 LET f2 = (ABS r2!r_cgxdot + ABS r2!r_cgydot)*12/100

                 LET fx1 = +muldiv(f1, s, One)
                 AND fy1 = -muldiv(f1, c, One)
                 LET fx2 = +muldiv(f2, s, One)
                 AND fy2 = -muldiv(f2, c, One)

                 IF sepy<0 DO
                 { fx1, fy1 := -fx1, -fy1 // Apply forces in the right direction
                   fx2, fy2 := -fx2, -fy2
                 }
                 // Apply force (fx1,fy1) to robot r1. Note that the direction of
                 // (fx1,fy1) is (-s,c)
                 // Robot r2 receives its force in the opposite direction.

                 r1!r_cgxdot, r1!r_cgydot := r1!r_cgxdot+fx1, r1!r_cgydot+fy1
                 r2!r_cgxdot, r2!r_cgydot := r2!r_cgxdot-fx2, r2!r_cgydot-fy2

                 // This changes the velocities of both robots but not their
                 // positions.

                 IF rid1=-1 DO
                 { //writef("R%i2 and %i2: ABS sepy = %9.3d  6**robotradius=%9.3d*n",
                   //        rid1, rid2, ABS sepy, 6*robotradius)
                   writef("Applying fx1=%9.3d  fy1=%9.3d to R%n*n", fx1, fy1, rid1)
                   writef("Applying fx2=%9.3d  fy2=%9.3d to R%n*n", fx2, fy2, rid2)
                   //abort(631)
                 }

                 // Do not move the robots yet.
               }
             }
      }
    }
  }



  // (4) For each robot, set inarea=false then look at every bottle.
  //     Deal with its bounces off the robot body, shoulders,
  //     and grabber.
  //     If a bottle is in the grabber area and the grabber is fully
  //     open and inarea=false, cause it to become the robot's selected
  //     bottle, set inarea=true and start closing the grabber, but if
  //     inarea was true there are two or more bottles in the grabber
  //     area so start opening the grabber to let one or more escape.
  //     If inarea=true and grabposdot<0 and grappos<=grabbedpos set
  //     grabbed to true and set grabposdot=0.

  FOR rid = 1 TO robotv!0 DO
  { LET r = robotv!rid
    LET b = r!r_bottle  // The currently selected bottle
    LET inareacount = 0 // Count of the number of bottle in the grabber area
                        // If >0 b will be a bottle in the grabber area

    UNLESS b IF freebottles & r!r_grabpos=1_000 DO
    { // This robot can select a bottle
//sawritef("R%n: has no selected bottle, freebottles=%n and grabpos=%6.3*n",
//          rid, freebottles, r!r_grabpos)

      FOR bid = 1 TO bottlev!0 DO
      { b := bottlev!bid

        UNLESS b!b_dropped | b!b_robot DO
        { // Bottle b is neither dropped nor owned by another
          // robot, so select it.
          r!r_bottle := b
          r!r_inarea := FALSE  // This should not be necessary.
          b!b_robot := r
          freebottles := freebottles - 1
//sawritef("R%n: selects B%n, freebottles=%n*n", rid, bid, freebottles)
          BREAK
        }
      }
    }
    
    // This robot has a selected if one was available.

    // Now deal with robot-bottle interraction.
    // This requires the robots coordinates to be calculated.
    robotcoords(r)

    FOR bid = 1 TO bottlev!0 DO
    { LET b = bottlev!bid

      IF b!b_dropped LOOP // Ignore dropped bottles

       // Ignore this bottle unless it is close to the robot.
      UNLESS incontact(r, b, 3*robotradius) LOOP

      IF rid=-1 DO
      { writef("R%n is close to B%n*n", rid, bid)
        abort(3002)
      }

      // Test if the bottle has hit the body of the robot.
      IF incontact(r, b, robotradius+bottleradius) DO
      { // If so make the bottle bounce off.
        IF rid=-1 DO
          writef("R%n body bounce with B%n*n", rid, bid)
        cbounce(r, b, 1, 0) // The robot is infinitely heavy
      }

      // Test for left shoulder-bottle bounce
      { LET sx, sy, sxdot, sydot =
            r!r_lcx, r!r_lcy,          // Left shoulder centre
            r!r_cgxdot, r!r_cgydot     // Motion ignoring rate of rotation.
        LET s = @sx                    // Centre of left choulder.
        IF incontact(s, b, shoulderradius+bottleradius) DO
        { // They are in contact so make the bottle bounce off
          IF rid=-1 DO
            writef("R%n left shoulder contact with B%n*n", rid, bid)
          cbounce(s, b, 1, 0) // Robot is inifinitely heavy
        }
      }

      // Test for right shoulder-bottle bounce
      { LET sx, sy, sxdot, sydot =
            r!r_rcx, r!r_rcy,
            r!r_cgxdot, r!r_cgydot
        LET s = @sx 
        IF incontact(s, b, shoulderradius+bottleradius) DO
        { // They are in contact so make the bottle bounce off
          IF rid=-1 DO
            writef("R%n right shoulder contact with B%n*n", rid, bid)
          cbounce(s, b, 1, 0) // Robot is infinitely heavy
        }
      }

      // Test for robot left tip bounce
      { LET sx, sy, sxdot, sydot =
            r!r_ltcx, r!r_ltcy,
            r!r_cgxdot, r!r_cgydot
        LET s = @sx 
        IF incontact(s, b, tipradius+bottleradius) DO
        { // They are in contact so make the bottle bounce off
          IF rid=-1 DO
            writef("R%n left tip contact with B%n*n", rid, bid)
          cbounce(s, b, 1, 0) // Robot is heavy
        }
      }

      // Test for robot right tip bounce
      { LET sx, sy, sxdot, sydot =
            r!r_rtcx, r!r_rtcy,
            r!r_cgxdot, r!r_cgydot
        LET s = @sx 
        IF incontact(s, b, tipradius+bottleradius) DO
        { // They are in contact so make the bottle bounce off
          IF rid=-1 DO
            writef("R%n right tip contact with B%n*n", rid, bid)
          cbounce(s, b, 1, 0) // Robot is heavy
        }
      }

      // Test for robot grabber bounces

      { // Make the robot's centre the origin
        LET bx = b!b_cgx - r!r_cgx
        LET by = b!b_cgy - r!r_cgy

        LET c = cosines(r!r_cgxdot, r!r_cgydot) // Direction cosines of the robot
        LET s = result2

        // Rotate clockwise the bottle position about the new origin
        LET tx = inprod(bx, by,  c,  s)
        LET ty = inprod(bx, by, -s,  c)

        // Deal with bounces of the arm edges

        // Calculate the y positions of the arm edges.
        LET y3 = muldiv(robotradius-shoulderradius-armthickness,
                        r!r_grabpos, One) // Right edge of the left  arm
        LET y4 = y3 + armthickness        // Left  edge of the left  arm
        LET y2 = -y3                      // Left  edge of the right arm
        LET y1 = -y4                      // Right edge of the right arm

IF rid=-1 DO // Debugging aid
{ writef("R%n:  cg=(%8.3d %8.3d)*n", rid, r!r_cgx, r!r_cgy)
  writef("B%n:  cg=(%8.3d %8.3d)*n", bid, b!b_cgx, b!b_cgy)
  writef("bx=%8.3d by=%8.3d*n", bx, by)
  writef("tx=%8.3d ty=%h.3d grablen=%8.3d*n", tx, ty, grablen)
  writef("x1=%8.3d x2=%8.3d*n", robotradius, robotradius+grablen)
  writef("y1=%8.3d y2=%8.3d y3=%8.3d y4=%8.3d*n", y1, y2, y3, y4)
abort(1234)
}

        IF robotradius <= tx <= robotradius+grablen DO
        { // Bounces and grabbing are both possible

IF rid=-1 DO
{ sawritef("R%n: has B%n parallel to grabbers*n", rid, bid)
  abort(1235)
}
          IF y1 - bottleradius <= ty <= y1 DO
          { // Bottle bounce with outside edge of right arm
            //LET rtdot = inprod(r!r_cgxdot, r!r_cgydot, c, s)
            LET rwdot = inprod(r!r_cgxdot, r!r_cgydot,-s, c)
            LET btdot = inprod(b!b_cgxdot, b!b_cgydot, c, s)
            LET bwdot = inprod(b!b_cgxdot, b!b_cgydot,-s, c)
            LET v = bwdot-rwdot
IF rid=-1 DO
{ sawritef("B%n: in contact with outside edge of right grabber arm*n", bid)
  abort(1236)
}
            IF v>0 DO
            { bwdot := rebound(v) + rwdot
              // Transform bottle velocity to world coords
              b!b_cgxdot := inprod(btdot,bwdot, c, -s)
              b!b_cgydot := inprod(btdot,bwdot, s,  c)
            }
          }

          IF y2 <= ty <= y2 + bottleradius DO
          { // Bottle bounce with the inside edge of right arm
            LET rtdot = inprod(r!r_cgxdot, r!r_cgydot, c, s)
            LET rwdot = inprod(r!r_cgxdot, r!r_cgydot,-s, c)
            LET btdot = inprod(b!b_cgxdot, b!b_cgydot, c, s)
            LET bwdot = inprod(b!b_cgxdot, b!b_cgydot,-s, c)
            LET v = bwdot-rwdot // Speed of bottle away from the right arm
IF rid=-1 DO
{ sawritef("B%n: in contact with inside edge of right grabber arm*n", bid)
  sawritef("rxdot=%8.3d rydot=%8.3d*n", r!r_cgxdot, r!r_cgydot)
  sawritef("bxdot=%8.3d bydot=%8.3d*n", b!b_cgxdot, b!b_cgydot)
  sawritef("c=    %8.3d s=    %8.3d*n", c, s)
  sawritef("rtdot=%8.3d rwdot=%8.3d*n", rtdot, rwdot)
  sawritef("btdot=%8.3d bwdot=%8.3d*n", btdot, bwdot)
  sawritef("v=    %8.3d*n", v)
  abort(1236)
}
            IF v<0 DO
            { bwdot := rebound(v) + rwdot
              // Transform bottle velocity to world coords
IF rid=-1 DO
  sawritef("bxdot=%8.3d bydot=%8.3d*n", b!b_cgxdot, b!b_cgydot)
              b!b_cgxdot := inprod(btdot,bwdot, c, -s)
              b!b_cgydot := inprod(btdot,bwdot, s,  c)
IF rid=-1 DO
{ sawritef("bxdot=%8.3d bydot=%8.3d*n", b!b_cgxdot, b!b_cgydot)
  abort(1239)
}
            }
            //IF tydot>0 DO tydot := rebound(tydot)
          }

          IF y3 - bottleradius <= ty <= y3 DO
          { // Bottle collision with right edge of left arm
            //LET rtdot = inprod(r!r_cgxdot, r!r_cgydot, c, s)
            LET rwdot = inprod(r!r_cgxdot, r!r_cgydot,-s, c)
            LET btdot = inprod(b!b_cgxdot, b!b_cgydot, c, s)
            LET bwdot = inprod(b!b_cgxdot, b!b_cgydot,-s, c)
            LET v = bwdot-rwdot
IF rid=-1 DO
{ sawritef("B%n: in contact with right edge of left grabber*n", bid)
  abort(1237)
}
            IF v>0 DO
            { bwdot := rebound(v) + rwdot
              // Transform bottle velocity to world coords
              b!b_cgxdot := inprod(btdot,bwdot, c, -s)
              b!b_cgydot := inprod(btdot,bwdot, s,  c)
            }
          }

          IF y4 <= ty <= y4 + bottleradius DO
          { // Bottle collision with left edge of left arm
            //LET rtdot = inprod(r!r_cgxdot, r!r_cgydot, c, s)
            LET rwdot = inprod(r!r_cgxdot, r!r_cgydot,-s, c)
            LET btdot = inprod(b!b_cgxdot, b!b_cgydot, c, s)
            LET bwdot = inprod(b!b_cgxdot, b!b_cgydot,-s, c)
            LET v = bwdot-rwdot
IF rid=-1 DO
{ sawritef("B%n in contact with left edge of left grabber*n", bid)
  abort(1236)
}

            IF v<0 DO
            { bwdot := rebound(v) + rwdot
              // Transform bottle velocity to world coords
              b!b_cgxdot := inprod(btdot,bwdot, c, -s)
              b!b_cgydot := inprod(btdot,bwdot, s,  c)
            }
          }

          IF y2 <= ty <= y3 DO
          { // Bottle b is the grabber area

IF rid=-1 DO
{ sawritef("B%n: is in R%n's grabber area*n", bid, rid)
  abort(2233)
}
            inareacount := inareacount + 1

            UNLESS b!b_robot DO
            { // The bottle is not dropped and does not have
              // an owner, select it.
            
              IF r!r_bottle DO
              { // De-select this robot's current bottle
                LET sb = r!r_bottle
                sb!b_robot := 0
                sb!b_grabbed := FALSE
                r!r_bottle := 0
                r!r_inarea := FALSE
                freebottles := freebottles + 1
              }

              r!r_bottle := b
              r!r_inarea := TRUE
              b!b_robot := r
              b!b_grabbed := FALSE
              freebottles := freebottles - 1
            }


            // Test for a bounce off the grabber base
            IF robotradius <= tx <= robotradius+bottleradius DO
            { LET rtdot = inprod(r!r_cgxdot, r!r_cgydot, c, s)
              LET btdot = inprod(b!b_cgxdot, b!b_cgydot, c, s)
              LET bwdot = inprod(b!b_cgxdot, b!b_cgydot,-s, c)
              LET v = btdot-rtdot
IF rid=-1 DO
{ sawritef("B%n is in contact R%n,s grabber base*n", bid, rid)
  sawritef("grabbedpos = %8.3d*n", grabbedpos)
  abort(2235)
}
              IF v<0 DO
              { btdot := rebound(v) + rtdot
                // Transform bottle velocity to world coords
                b!b_cgxdot := inprod(btdot,bwdot, c, -s)
                b!b_cgydot := inprod(btdot,bwdot, s,  c)
              }
            }
          }
        }
      }
    }    // End of bottle loop

    // If the selected bottle is the only bottle in this robot's
    // grabber area set inarea to TRUE.
    r!r_inarea := inareacount=1
  }

  // (5) Deal with all the bottle-bottle bounces.

  FOR bid1 = 1 TO bottlev!0 DO
  { LET b1 = bottlev!bid1  // b1 -> [cgx, cgy, cgxdot, cgydot]

    UNLESS b1!b_dropped DO
    { // Test for bottle-bottle bounces
      FOR bid2 = bid1+1 TO bottlev!0 DO
      { LET b2 = bottlev!bid2  // b2 -> [cgx, cgy, cgxdot, cgydot]
        IF b2!b_dropped LOOP
        IF incontact(b1, b2, bottleradius+bottleradius) DO
          cbounce(b1, b2, 1, 1)
      }
    }
  }
  //abort(9002)

  // Move the robots and their grabber arms.
  // All bottles have been seen.
  FOR rid = 1 TO robotv!0 DO
  { LET r = robotv!rid
    LET b = r!r_bottle
    LET inarea = r!r_inarea // =TRUE if b is the only bottle in
                            // this robot's grabber area
    LET grabpos, grabposdot = r!r_grabpos, r!r_grabposdot

    UNLESS inarea | b!b_grabbed IF grabposdot=0 & grabpos<1_000 DO
    {  grabposdot := +0_600
       r!r_grabposdot := grabposdot 
    }

    IF grabposdot DO
    { grabpos := grabpos+grabposdot/sps
      r!r_grabpos := grabpos

      TEST grabposdot > 0
      THEN { IF grabpos >= 1_000 DO r!r_grabpos, r!r_grabposdot := 1_000, 0
           }
      ELSE { IF grabpos <= grabbedpos & inarea DO
             { // The grabber has just captured the selected bottle
               grabpos        := grabbedpos
               r!r_grabpos    := grabpos
               r!r_grabposdot :=  0
               b!b_grabbed    := TRUE
             }
           }

      // If the grabber is fully closed start opening it.
      IF grabpos <= 0_100 DO r!r_grabpos, r!r_grabposdot := 0_100, 0_600
    }

    IF inarea & r!r_grabpos=1_000 & r!r_grabposdot=0 DO
      r!r_grabposdot := -0_600

    // Encourage the robot to move towards its selected bottle, if any.
    IF r!r_bottle &
       edgesize < r!r_cgx < screenxsize*One-edgesize &
       edgesize < r!r_cgy < screenysize*One-edgesize DO
    { LET b = r!r_bottle // The possibly grabbed selected bottle

      UNLESS b!b_grabbed | b!b_dropped DO
      { // The bottle is selected, not grabbed and not dropped
        // so make the robot move towards it
        LET dx = b!b_cgx - r!r_bcx
        LET dy = b!b_cgy - r!r_bcy
        // Calculate the direction from the robot to the bottle
        LET ct = cosines(dx, dy)
        LET st = result2

        // Calculate the speed of the bottle
        LET vx, vy = b!b_cgxdot, b!b_cgydot

        // Calculate the direction of motion
        LET bcv = cosines(vx, vy)
        LET bsv = result2
        LET speed = vx=0=vy -> 0,
                    ABS vx > ABS vy -> muldiv(vx, One, bcv),
                                       muldiv(vy, One, bsv)

        // Increase the speed depending on the distance from the bottle
        speed := speed + 15_000

        // Increase the speed if the robot is not close to the bottle
        UNLESS incontact(r, b, 2*robotradius) DO speed := speed + 56_000

        // Make the robot move towards the bottle
        r!r_cgxdot := (29 * r!r_cgxdot + muldiv(speed, ct, One)) / 30
        r!r_cgydot := (29 * r!r_cgydot + muldiv(speed, st, One)) / 30
      }

      IF b!b_grabbed DO
      { // Cause the robot to move towards the pit
        LET dx = pit_x - r!r_cgx
        LET dy = pit_y - r!r_cgy
        LET cp = cosines(dx, dy)
        LET sp = result2

        // Make the robot move towards the pit
        r!r_cgxdot := (29 * r!r_cgxdot + muldiv(60_000, cp, One)) / 30
        r!r_cgydot := (29 * r!r_cgydot + muldiv(60_000, sp, One)) / 30
        b!b_cgxdot, b!b_cgxdot := r!r_cgxdot, r!r_cgxdot
      }
    }    

    r!r_cgx := r!r_cgx + r!r_cgxdot/sps
    r!r_cgy := r!r_cgy + r!r_cgydot/sps
  }

  // Move the bottles
  FOR bid = 1 TO bottlev!0 DO
  { LET b = bottlev!bid

    IF b!b_dropped LOOP

    b!b_cgx := b!b_cgx + b!b_cgxdot/sps
    b!b_cgy := b!b_cgy + b!b_cgydot/sps
  }
}

AND initpitsurf(col, surfptr) = VALOF
{ // Allocate the pit surface
  LET r1 = pitradius/One
  LET r2 = r1 + edgesize/One
  LET height = 2*r2 + 2
  LET width  = height
  LET colkey = maprgb(1,1,1)
  LET surfok = mksurface(width, height, surfptr)

  UNLESS surfok RESULTIS FALSE
  
  selectsurface(surfptr, width, height)
  fillsurf(colkey)
  setcolourkey(surfptr, colkey)

  setcolour(col_gray1)
  drawfillcircle(r2, r2+1, r2)

  setcolour(col)
  drawfillcircle(r2, r2+1, r1)

  RESULTIS TRUE
}

AND initbottlesurf(col, surfptr) = VALOF
{ // Allocate a bottle surface
  LET height = 2*bottleradius/One + 2
  LET width  = height
  LET colkey = maprgb(1,1,1)
  LET surfok = mksurface(width, height, surfptr)

  selectsurface(surfptr, width, height)
  fillsurf(colkey)
  setcolourkey(surfptr, colkey)

  setcolour(col)
  drawfillcircle(bottleradius/One, bottleradius/One+1, bottleradius/One)

  RESULTIS TRUE
}

AND sine(theta) = VALOF
// theta =  0_000 for  0 degrees
//       = 64_000 for 90 degrees
// Returns a value in range -1_000 to +1_000
{ LET a = theta  /  1_000
  LET r = theta MOD 1_000
  LET s = rawsine(a)
  RESULTIS s + (rawsine(a+1)-s)*r/1000
}

AND cosine(x) = sine(x+64_000)

AND rawsine(x) = VALOF
{ // x is scaled d.ddd with 64.000 representing 90 degrees
  // The result is scaled d.ddd, ie 1_000 represents 1.000
  LET t = TABLE   0,   25,   49,   74,   98,  122,  147,  171,
                195,  219,  243,  267,  290,  314,  337,  360,
                383,  405,  428,  450,  471,  493,  514,  535,
                556,  576,  596,  615,  634,  653,  672,  690,
                707,  724,  741,  757,  773,  788,  803,  818,
                831,  845,  858,  870,  882,  893,  904,  914,
                924,  933,  942,  950,  957,  964,  970,  976,
                981,  985,  989,  992,  995,  997,  999, 1000,
               1000

  LET a = x&63
  UNLESS (x&64)=0  DO a := 64-a
  a := t!a
  UNLESS (x&128)=0 DO a := -a
  RESULTIS a
}

AND robotcoords(r) BE
{ // This function calculates the orientation of the robot
  // and the coordinates of all its key points
  LET x, y = r!r_cgx, r!r_cgy
  LET r1 = robotradius
  LET r2 = shoulderradius
  LET r3 = tipradius
  LET d1 = 2*r3
  LET d2 = muldiv(r!r_grabpos, r1-r2-d1, One)
  LET d3 = grablen
  LET c  = cosines(r!r_cgxdot, r!r_cgydot)
  LET s  = result2
  LET ns = -s

  r!r_lcx  := x + inprod( c,ns, r1-r2, r1-r2) // Left side
  r!r_lcy  := y + inprod( s, c, r1-r2, r1-r2)
  r!r_lex  := x + inprod( c,ns,    r1, r1-r2)
  r!r_ley  := y + inprod( s, c,    r1, r1-r2)

  r!r_rcx  := x + inprod( c,ns, r1-r2, r2-r1) // Right side
  r!r_rcy  := y + inprod( s, c, r1-r2, r2-r1)
  r!r_rex  := x + inprod( c,ns,    r1, r2-r1)
  r!r_rey  := y + inprod( s, c,    r1, r2-r1)

  r!r_ltax := x + inprod( c,ns,    r1, d1+d2) // Left arm
  r!r_ltay := y + inprod( s, c,    r1, d1+d2)
  r!r_ltbx := x + inprod( c,ns,    r1,    d2)
  r!r_ltby := y + inprod( s, c,    r1,    d2)
  r!r_ltcx := x + inprod( c,ns, r1+d3,    d2)
  r!r_ltcy := y + inprod( s, c, r1+d3,    d2)
  r!r_ltdx := x + inprod( c,ns, r1+d3, d1+d2)
  r!r_ltdy := y + inprod( s, c, r1+d3, d1+d2)
  r!r_ltpx := x + inprod( c,ns, r1+d3, d2+r3)
  r!r_ltpy := y + inprod( s, c, r1+d3, d2+r3)

  r!r_rtax := x + inprod( c,ns,    r1,-d1-d2) // Right arm
  r!r_rtay := y + inprod( s, c,    r1,-d1-d2)
  r!r_rtbx := x + inprod( c,ns,    r1,   -d2)
  r!r_rtby := y + inprod( s, c,    r1,   -d2)
  r!r_rtcx := x + inprod( c,ns, r1+d3,   -d2)
  r!r_rtcy := y + inprod( s, c, r1+d3,   -d2)
  r!r_rtdx := x + inprod( c,ns, r1+d3,-d1-d2)
  r!r_rtdy := y + inprod( s, c, r1+d3,-d1-d2)
  r!r_rtpx := x + inprod( c,ns, r1+d3,-d2-r3)
  r!r_rtpy := y + inprod( s, c, r1+d3,-d2-r3)

  // Centre of grabbed bottle
  r!r_bcx  := x + inprod( c,ns, robotradius+2*bottleradius, 0)
  r!r_bcy  := y + inprod( s, c, robotradius+2*bottleradius, 0)
}

AND drawrobot(r) BE
{ LET b = r!r_bottle

  robotcoords(r)

  setcolour(r!r_id=1 -> robot1colour, robotcolour)

  // Body
  drawfillcircle(r!r_cgx/One, r!r_cgy/One, robotradius/One)
  // Left shoulder
  drawfillcircle(r!r_lcx/One, r!r_lcy/One, shoulderradius/One)
  // Right shoulder
  drawfillcircle(r!r_rcx/One, r!r_rcy/One, shoulderradius/One)

  IF debugging DO
  { // Plot the robot number centred in the robot
    setcolour(col_black)
    drawf(r!r_cgx/One-(r!r_id>=10->9,3), r!r_cgy/One-6, "%n", r!r_id)
  }

  setcolour(grabbercolour)
  // Grabber base
  drawquad(r!r_lcx/One, r!r_lcy/One,    //   lc--le
           r!r_lex/One, r!r_ley/One,    //   |    |
           r!r_rex/One, r!r_rey/One,    //   |    |
           r!r_rcx/One, r!r_rcy/One)    //   rc--re
  // Left arm
  drawquad(r!r_ltax/One, r!r_ltay/One,  //   lta--------ltd
           r!r_ltbx/One, r!r_ltby/One,  //   |            |
           r!r_ltcx/One, r!r_ltcy/One,  //   ltb--------ltc
           r!r_ltdx/One, r!r_ltdy/One)
  drawfillcircle(r!r_ltpx/One, r!r_ltpy/One, tipradius/One)
  // Right arm
  drawquad(r!r_rtax/One, r!r_rtay/One,  //   rta--------rtd
           r!r_rtbx/One, r!r_rtby/One,  //   |            |
           r!r_rtcx/One, r!r_rtcy/One,  //   rtb--------rtc
           r!r_rtdx/One, r!r_rtdy/One)
  drawfillcircle(r!r_rtpx/One, r!r_rtpy/One, tipradius/One)

//sawritef("debugging=%n b=%n grabbed=%n dropped=%n*n",
//          debugging, b, b!b_grabbed, b!b_dropped)
 IF debugging UNLESS b!b_grabbed | b!b_dropped DO
 { setcolour(col_red)
   moveto(r!r_bcx/One, r!r_bcy/One)
   drawto(b!b_cgx/One, b!b_cgy/One)
//updatescreen()
//abort(1000)
 }
}

AND drawbottle(b) BE UNLESS b!b_dropped DO
{ LET r = b!b_robot // Owning robot,if any
  LET surf, surf1 = bottlesurfR, bottlesurfR1

  IF b!b_id=1  DO surf, surf1 := bottlesurfK, bottlesurfK1
  IF b!b_robot DO surf, surf1 := bottlesurfDR, bottlesurfDR1

  IF b!b_grabbed DO
  { surf, surf1 := bottlesurfB, bottlesurfB1
    b!b_cgx := r!r_bcx // If grabbed the bottle is at the centre
    b!b_cgy := r!r_bcy // of the robot's grabber.
  }

  blitsurf(@surf, @screen, (b!b_cgx-bottleradius)/One,
                           (b!b_cgy+bottleradius)/One)

  IF debugging DO
  { // Plot the bottle number near the bottle
    setcolour(col_black)
    drawf(b!b_cgx/One+10, b!b_cgy/One-6, "%n", b!b_id)
  }
}

AND plotscreen() BE
{ LET d = edgesize/One
  selectsurface(@screen, screenxsize, screenysize)
  fillsurf(backcolour)
  selectsurface(@screen, xsize, ysize)

  setcolour(col_gray1)
  drawquad(0,0, d,d, d,screenysize-d, 0,screenysize)
  setcolour(col_gray2)
  drawquad(0, screenysize,
           d, screenysize-d,
           screenxsize-d, screenysize-d,
           screenxsize, screenysize)
  setcolour(col_gray3)
  drawquad(screenxsize, screenysize,
           screenxsize-d, screenysize-d,
           screenxsize-d, d,
           screenxsize, 0)
  setcolour(col_gray4)
  drawquad(0,0, d,d, screenxsize-d,d, screenxsize,0)

  // The pit
  blitsurf(@pitsurf, @screen,
           (pit_x-pitradius-edgesize)/One, (pit_y+pitradius+edgesize)/One)
//sawritef("pit_x=%n pit_y=%n pitradius=%n*n", pit_x, pit_y, pitradius)
//updatescreen()
//abort(1000)
  selectsurface(@screen, xsize, ysize)
//abort(800)

  FOR i = 1 TO robotv!0  DO drawrobot(robotv!i)
//updatescreen()
//abort(1000)
  FOR i = 1 TO bottlev!0 DO drawbottle(bottlev!i)
//updatescreen()
//abort(1000)
//abort(802)

  setcolour(maprgb(255,255,255))
  
  IF debugging DO
  { //drawf(30, 380, "sps         = %i2", sps)
    drawf(80, 365, "freebottles = %i2", freebottles)
    drawf(80, 350, "bottlecount = %i2", bottlecount)
  }

  IF help DO
  { drawf(30, 165, "H -- Toggle help information")
    drawf(30, 150, "Q -- Quit")
    drawf(30, 135, "X -- Enter the debugger")
    drawf(30, 120, "P -- Pause/Continue")
    drawf(30, 105, "G -- Close the grabber of the Dark green robot")
    drawf(30,  90, "R -- Open the grabber of the Dark green robot")
    drawf(30,  75, "D -- Toggle debugging")
    drawf(30,  60, "U -- Toggle usage")
    drawf(30,  45, "W -- Write debugging info")
    drawf(30,  30, "Arrow keys -- Control the dark green robot")
  }

  setcolour(maprgb(255,255,255))
  
  IF displayusage DO
    drawf(30, 345, "CPU usage = %i3%% sps = %n", usage, sps)
//updatescreen()
//abort(803)
  IF debugging DO
  { LET r = robotv!1
    LET sb = r!r_bottle
    LET b = bottlev!0 -> bottlev!1, 0
    drawf(80, 120, "R1: x=%8.3d y=%8.3d xdot=%8.3d ydot=%8.3d",
          r!r_cgx, r!r_cgy, r!r_cgxdot, r!r_cgydot)
    IF b DO
      drawf(80, 105, "B1: x=%8.3d y=%8.3d xdot=%8.3d ydot=%8.3d",
            b!b_cgx, b!b_cgy, b!b_cgxdot, b!b_cgydot)
    //drawf(80, 85, "    grabpos=%8.3d       grabposdot=%8.3d",
    //      r!r_grabpos, r!r_grabposdot)
    //IF sb DO
    //  drawf(80, 45, "Selected B%i2 grabbed=%n",
    //        (sb -> sb!b_id, 0), (sb -> sb!b_grabbed, FALSE))
//abort(5678)
  }
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ DEFAULT:
    LOOP

  CASE sdle_keydown:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:
      CASE 'H': help := ~help
                LOOP


      CASE 'Q': done := TRUE
                LOOP

      CASE 'D': debugging := ~debugging
                LOOP

      CASE 'X': sawritef("User requested entry to the debugger*n")
                sawritef("robotv=%n bottlev=%n*n", robotv, bottlev)
                abort(9999)
                LOOP

      CASE 'U': displayusage := ~displayusage
                LOOP

      CASE 'W': sawritef("Bottles      = %n*n", bottles)
                sawritef("Free bottles = %n*n", freebottles)
                sawritef("Robots       = %n*n", robots)
                FOR i = 1 TO bottlev!0 DO
                { LET b = bottlev!i
                  UNLESS b LOOP
                  sawritef("Bottle %i2: ", b!b_id)
                  IF b!b_robot DO sawritef(" robot  %i2", b!b_robot!r_id)
                  IF b!b_grabbed DO sawritef(" grabbed")
                  sawritef("*n")
                }

                FOR i = 1 TO robotv!0 DO
                { LET r = robotv!i
                  UNLESS r LOOP
                  sawritef("Robot  %i2: ", r!r_id)
                  IF r!r_bottle DO sawritef(" bottle %i2", r!r_bottle!b_id)
                  IF r!r_inarea DO sawritef(" bottle in area")
                  sawritef("*n")
                }

                abort(1000)
                LOOP

      CASE 'G': // Grab
              { LET r = robotv!1
                LET b = r!r_bottle
                // Start closing unless a bottle is already grabbed
                UNLESS b & b!b_grabbed DO r!r_grabposdot := -0_600
                LOOP
              }

      CASE 'R': // Release
              { LET r = robotv!1
                LET b = r!r_bottle
                r!r_grabposdot := +0_300
                IF b & b!b_grabbed DO b!b_grabbed := FALSE
                LOOP
              }

      CASE 'S': // Start again
                LOOP

      CASE 'P': // Toggle stepping
                stepping := ~stepping
                LOOP

      CASE sdle_arrowup:
              { LET r = robotv!1
                LET c = cosines(r!r_cgxdot, r!r_cgydot)
                LET s = result2
                r!r_cgxdot := r!r_cgxdot + muldiv(5_000, c, One)
                r!r_cgydot := r!r_cgydot + muldiv(5_000, s, One)
                LOOP
              }

      CASE sdle_arrowdown:
              { LET r = robotv!1
                LET c = cosines(r!r_cgxdot, r!r_cgydot)
                LET s = result2
                r!r_cgxdot := r!r_cgxdot - muldiv(4_000, c, One)
                r!r_cgydot := r!r_cgydot - muldiv(4_000, s, One)
                LOOP
              }

      CASE sdle_arrowright:
              { LET r = robotv!1
                LET xdot = r!r_cgxdot
                LET ydot = r!r_cgydot
                LET dc  = cosine(4_000)
                LET ds  = sine(4_000)
                r!r_cgxdot := inprod(xdot,ydot, dc, ds)
                r!r_cgydot := inprod(xdot,ydot,-ds, dc)
                LOOP
              }

      CASE sdle_arrowleft:
              { LET r = robotv!1
                LET xdot = r!r_cgxdot
                LET ydot = r!r_cgydot
                LET dc  = cosine(4_000)
                LET ds  = - sine(4_000)
                r!r_cgxdot := inprod(xdot,ydot, dc, ds)
                r!r_cgydot := inprod(xdot,ydot,-ds, dc)
                LOOP
              }
    }

  CASE sdle_quit:
    writef("QUIT*n");
    done := TRUE
    LOOP
}

AND nearedge(x, y, dist) = VALOF
{ //writef("nearedge: x=%n y=%n dist=%n xsize=%n ysize=%n*n",
  //        x/One, y/One, dist/One, xsize, ysize)
//abort(2000)
  UNLESS dist < x < xsize*One - dist  RESULTIS TRUE
  UNLESS dist < y < ysize*One - dist  RESULTIS TRUE
//writef("=> FALSE*n")
//abort(2001)
  RESULTIS FALSE
}

AND nearthepit(x, y, dist) = VALOF
{ LET cx = pit_x
  LET cy = pit_y
  LET dx = ABS(x - cx)
  LET dy = ABS(y - cy)
//writef("nearthepit: x=%n y=%n cx=%n cy=%n dist=%n*n",
//        x/One, y/One, cx/One, cy/One, dist/One)
//abort(3000)
  IF dx < dist & dy < dist RESULTIS TRUE
//writef("=> FALSE*n")
//abort(3001)
  RESULTIS FALSE
}

AND nearbottle(x, y, b, dist) = VALOF
{ // Return TRUE if (x,y) is near bottle b.
  // (x,y) is the position of either a robot or a bottle.
  LET bx = b!b_cgx
  LET by = b!b_cgy
  LET dx = ABS(x - bx)
  LET dy = ABS(y - by)
//writef("nearbottle: bid=%n x=%n y=%n bx=%n by=%n dx+dy=%n dist=%n*n",
//        b!b_id, x/One, y/One, bx/One, by/One, (dx+dy)/One, dist/One)
//abort(4000)
  IF dx < dist & dy < dist RESULTIS TRUE

//writef("=>FALSE*n")
//abort(4001)
  RESULTIS FALSE

}

AND nearanybottle(bid, x, y, dist) = VALOF
{ // Return TRUE if (x,y) near a bottle other than bottle bid
  // If bid=0, (x,y) is the position of a robot.
  FOR i = 1 TO bottlev!0 UNLESS i=bid IF nearbottle(x, y, bottlev!i, dist)
     RESULTIS TRUE
  RESULTIS FALSE
}

AND nearrobot(x, y, r, dist) = VALOF
{ // Return TRUE if (x,y) is near robot r.
  // (x,y) is the position of either a robot or a bottle.
  LET rx = r!r_cgx
  LET ry = r!r_cgy
  LET dx = ABS(x - rx)
  LET dy = ABS(y - ry)
//sawritef("nearrobot: rib=%i2 x=%n y=%n rx=%n ry=%n dx+dy=%n dist=%n*n",
//          r!r_id, x/One, y/One, rx/One, ry/One, (dx+dy)/One, dist/One)
//abort(5000)
  IF dx < dist & dy < dist RESULTIS TRUE

//sawritef("=>FALSE*n")
//abort(5001)
  RESULTIS FALSE
}

AND nearanyrobot(rid, x, y, dist) = VALOF
{ // Return TRUE if (x,y) near a robot other than robot rid
  // If rid=0, (x,y) is the position of a bottle.
  FOR i = 1 TO robotv!0 DO
    UNLESS i=rid IF nearrobot(x, y, robotv!i, dist) RESULTIS TRUE
  RESULTIS FALSE
}


LET start() = VALOF
{ LET argv = VEC 50
  LET stepmsecs = ?
  LET comptime  = 0 // Amount of cpu time per frame
  LET day, msecs, filler = 0, 0, 0
  //datstamp(@day)
  seed := 5 //msecs       // Set seed based on time of day
  //msecs0 := msecs   // Set the starting time
  //msecsnow := 0

  UNLESS rdargs("-b/n,-r/n,-sx/n,-sy/n,-s/n,-d/s",
                argv, 50) DO
  { writef("Bad arguments for robots*n")
    RESULTIS 0
  }

  bottles := 35
  robots  :=  7
  //bottles := 20
  //robots  :=  6
  //bottles := 1
  //robots  := 1

  xsize   := 700
  ysize   := 500

  IF argv!0 DO bottles := !(argv!0) // -b/n
  IF argv!1 DO robots  := !(argv!1) // -r/n
  IF argv!2 DO xsize   := !(argv!2) // -sx/n
  IF argv!3 DO ysize   := !(argv!3) // -sy/n
  IF argv!4 DO seed    := !(argv!4) // -s/n
  debugging := argv!5               // -d/s

  help := FALSE

  IF bottles <   0 DO bottles :=   0
  IF bottles > 100 DO bottles := 100
  IF robots  <   1 DO robots  :=   1
  IF robots  >  30 DO robots  :=  30

  freebottles := bottles
  bottlecount := bottles
  setseed(seed)

  UNLESS sys(Sys_sdl, sdl_avail) DO
  { writef("*nThe SDL features are not available*n")
    RESULTIS 0
  }

  spacev := getvec(spacevupb)

  UNLESS spacev DO
  { writef("Insufficient space available*n")
    RESULTIS 0
  }

  spacep, spacet := spacev, spacev+spacevupb


  IF FALSE DO
  { // Code to test the cosines function
    LET e1, e2, rsq = One, One, One
    LET x, y, xdot, ydot, c, s = 0, 0, One, 0, One, 0
    LET p = @x
    FOR dy = 0 TO One BY One/100 DO
    { ydot := dy
      c := cosines(xdot, ydot)
      s := result2
      rsq := inprod(c,s, c,s)
      writef("dx=%8.3d  dy=%8.3d cos=%8.3d sin=%8.3d rsq=%8.3d*n",
              One, dy, c, s, rsq)
      IF e1 < rsq DO e1 := rsq
      IF e2 > rsq DO e2 := rsq
    }
    writef("Errors +%7.3d  -%7.3d*n", e1-One, One-e2)
abort(1000)
    RESULTIS 0
  }

  // Initialise the priority queue
  priq := mkvec(200)
  priqn, priqupb := 0, 200

  initsdl()
  mkscreen("Robots -- Press H for Help", xsize, ysize)

  backcolour      := maprgb(100,100,100)
  col_red         := maprgb(255,  0,  0)
  col_darkred     := maprgb(196,  0,  0)
  col_black       := maprgb(  0,  0,  0)
  col_brown       := maprgb(100, 50, 20)
  col_gray1       := maprgb(110,110,110)
  col_gray2       := maprgb(120,120,120)
  col_gray3       := maprgb(130,130,130)
  col_gray4       := maprgb(140,140,140)
  pitcolour       := maprgb( 20, 20,100)
  robotcolour     := maprgb(  0,255,  0)
  robot1colour    := maprgb(  0,120, 40)
  grabbercolour   := maprgb(200,200, 40)
//abort(1000)
  bottlesurfRok  := initbottlesurf(col_red,     @bottlesurfR)
  bottlesurfDRok := initbottlesurf(col_darkred, @bottlesurfDR)
  bottlesurfKok  := initbottlesurf(col_black,   @bottlesurfK)
  bottlesurfBok  := initbottlesurf(col_brown,   @bottlesurfB)
  pitsurfok      := initpitsurf(pitcolour,      @pitsurf)
//abort(1000)

  pit_x, pit_y := xsize*One/2, ysize*One/2
  pit_xdot, pit_ydot := 0, 0
  thepit := @pit_x

  // Initialise robotv
  robotv := mkvec(robots)
  robotv!0 := 0
  FOR i = 1 TO robots DO
  { LET r = mkvec(r_upb)
    LET x = ?
    LET y = ?

    UNLESS r DO
    { sawritef("More space needed*n")
      abort(999)
    }

    FOR j = 0 TO r_upb DO r!j := 0

    FOR k = 1 TO 200 DO
    { x := randno(xsize*One)
      y := randno(ysize*One)
      UNLESS nearedge (x, y, robotradius) |
             nearthepit  (x, y, pitradius+2*robotradius) |
             nearanyrobot(i, x, y, 3*robotradius) BREAK
//writef("R%i2: x=%8.3d y=%8.3d no good*n", i, x, y)
//abort(1000)
      IF k>150 DO
      { writef("Too many robots to place*n")
        abort(999)
      }
    }

writef("R%i2: x=%8.3d y=%8.3d good*n", i, x, y)
//abort(1005)

    robotv!0 := i
    robotv!i := r

    // Position
    r!r_cgx        := x
    r!r_cgy        := y

    // Motion
    r!r_cgxdot     := randno(40_000) - 20_000
    r!r_cgydot     := randno(40_000) - 20_000

    // grabber
    r!r_grabpos    := 1_000     // The grabber is fully open
    r!r_grabposdot := 0_000
    r!r_bottle     := 0         // No grabbed bottle
    r!r_id := i
    robotcoords(r)
  }
//abort(1001)
  // Initialise bottlev
  bottlev := mkvec(bottles)
  bottlev!0 := 0
  FOR i = 1 TO bottles DO
  { LET b = mkvec(b_upb)
    LET x = ?
    LET y = ?

    UNLESS b DO
    { sawritef("More space needed*n")
      abort(999)
    }

    FOR j = 0 TO b_upb DO b!j := 0

    //FOR k = 1 TO 1000 DO
    { // Choose a random position for the next bottle
      x := randno(xsize*One)
      y := randno(ysize*One)
//sawritef("Calling nearedge*n")
      UNLESS nearedge  (x, y, 4*bottleradius) |
             nearthepit(x, y, 4*bottleradius) |
             nearanyrobot (0, x, y, 2*robotradius)  |
             nearanybottle(i, x, y, 4*bottleradius) BREAK
      //IF k > 200 DO
      //{ writef("Too many bottles to place*n")
      //  abort(999)
      //  BREAK
      //}
    } REPEAT

    bottlev!0   := i
    bottlev!i   := b
    b!b_cgx     := x
    b!b_cgy     := y
    b!b_cgxdot  := randno(50_000) - 25_000
    b!b_cgydot  := randno(50_000) - 25_000
    b!b_grabbed := FALSE
    b!b_robot   := 0         // No grabbing robot
    b!b_dropped := FALSE
    b!b_id      := i
  }
//abort(1002)

  stepping := TRUE     // =FALSE if not stepping
  usage := 0
  //debugging := FALSE
  displayusage := FALSE
  sps := 10 // Initial setting
  stepmsecs := 1000/sps

  wall_wx := 0
  wall_ex := (screenxsize-1)*One      // East wall

  wall_sy    := 0                     // South wall
  wall_ny := (screenysize-1)*One      // North wall

  done := FALSE

//{ LET r1, r2 = robotv!1, robotv!2
//r1!r_cgx, r1!r_cgy := 400_000,               100_000
//r2!r_cgx, r2!r_cgy := 400_000+robotradius*5, 100_000+00_000
//r1!r_cgxdot, r1!r_cgydot := 10_000,            0_000
//r2!r_cgxdot, r2!r_cgydot := 0,                -1_000
//}

//abort(1003)
  UNTIL done DO
  { LET t0 = sdlmsecs()
    LET t1 = ?

    processevents()

    IF stepping DO step()
//abort(922)
    usage := 100*comptime/stepmsecs

    plotscreen()
    updatescreen()

    UNLESS 80<usage<95 DO
    { TEST usage>90
      THEN sps := sps-1
      ELSE sps := sps+1
      IF sps<1 DO sps := 1 // To stop division by zero
      stepmsecs := 1000/sps
    }

    t1 := sdlmsecs()

    comptime := t1 - t0
    IF t0+stepmsecs > t1 DO sdldelay(t0+stepmsecs-t1)
  }

  writef("*nQuitting*n")
  sdldelay(0_200)

  IF bottlesurfR  DO freesurface(@bottlesurfR)
  IF bottlesurfDR DO freesurface(@bottlesurfDR)
  IF bottlesurfK  DO freesurface(@bottlesurfK)
  IF bottlesurfB  DO freesurface(@bottlesurfB)
  IF pitsurf      DO freesurface(@pitsurf)

  closesdl()

fin:
  IF spacev DO freevec(spacev)
  RESULTIS 0
}

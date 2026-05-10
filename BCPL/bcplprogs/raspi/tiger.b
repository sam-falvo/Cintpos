/*
########### THIS IS UNDER DEVELOPMENT ###############################

This is a flight simulator based on Jumbo that ran interactively on a
PDP 11 generating the pilots view on a Vector General Display.

Originally implemented by Martin Richards in mid 1970s.

Substantially modified my Martin Richards (c) October 2012.

It has been extended to use 32 rather than 16 bit arithmetic.

It is planned that this will simulate the flying characterists of
a De Havilland D.H.82A Tiger Moth which I learnt to fly as a teenager.

This version uses the SDL Graphics Library. The version of this
program that uses the OpenGL graphics library is called gltiger.b.

Change history

08/08/2020
This progran need revision since the recent changes to sdl.b have stopped
it from working. Corrections will be made in due course.

22/03/2018
Made extensive modifications based on the recent changes to draw3d.b.
It now makes use of floating point and the new FLT feature.

25/01/2013
Name changed to tiger.b

Controls

Either use a USB Joystick for elevator, ailerons and throttle, or
use the keyboard as follows:

Up arrow      Trim joystick forward a bit
Down arrow    Trim joystick backward a bit
Left arrow    Trim joystick left a bit
Right arrow   Trim joystick right a bit

, or <        Trim rudder left
. or >        Trim rudder right
x             More throttle
z             Less throttle

0             Display the pilot's view
1,2,3,4,5,6,7,8 Display the aircraft viewed from various angles

f             View aircraft from a greater distance
n             View aircraft from a closer position

p             pause/unpause the simulation

g             Reset the aircraft on the glide path

t             Reset the aircraft ready for take off -- default
              ie stationary on the ground at the end of the runway

u             Plot usage

a, b, c       Rotate the aircraft anti-clockwise about axes t, w, l
A, B, C       Rotate the aircraft clockwise about axes t, w, l

q             Quit

There are joystick buttons equivalent to Up arrow, Down arrow, Left
Arrow and Right arrow. There are also joystick buttons to trim the
rudder left and right, useful for steering on the runway.

The display shows various beacons on the ground including the lights
on the sides and the ends of the runway.

The display also shows various flight instruments including the
artificial horizon, the height and speed and various navigational aids
to help the pilot find the runway.

Note that a real tigermoth does not actually have an artificial horizon
so cannot be flown in cloud or at night.
*/

GET "libhdr"
GET "sdl.h"
GET "sdl.b"
.
GET "libhdr"
GET "sdl.h"

MANIFEST {
  FLT D45 = 0.707107  // cosine of pi/4
  FLT Sps = 20.0      // Steps per second

  // Most measurements are in feet held as floating point numbers.

  FLT k_g = 32.0      // Acceleration due to gravity, 32 ft per sec per sec
                      // Scaled with 3 digits after the decimal point.
  FLT k_drag = k_g/15 // Acceleration due to drag as 100 ft per sec
                      // The drag is proportional to the square of the speed.

  // Conversion factors
  FLT mph2fps   = 5280.0/(60*60)
  FLT mph2knots = 128.0/147
}

GLOBAL {
  done:ug

  FLT One      // Set to 1.0  Loading globals are cheaper than
  FLT Zro      // Set to 0.0  loading 32-bit constants.

  stepping     // =FALSE if not stepping the simulation
  stepcount
  msecs1
  FLT steprate

  crashed      // =TRUE if crashed
  debugging    // Toggled by the D command.
  plotusage    // Toggled by the U command.
  

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

  FLT c_throttle; FLT c_trimthrottle; FLT throttle  //   0.0 to +1.0
  FLT c_aileron;  FLT c_trimaileron;  FLT aileron   //  -1.0 to +1.0
  FLT c_elevator; FLT c_trimelevator; FLT elevator  //  -1.0 to +1.0
  FLT c_rudder;   FLT c_trimrudder;   FLT rudder    //  -1.0 to +1.0

  enginestarted     // = TRUE or FALSE
  FLT rpm; FLT targetrpm
  FLT thrust

  FLT rateofclimb  // In ft/min

  FLT ctn; FLT ctw; FLT cth    // Direction cosines of direction t (forward)
  FLT cwn; FLT cww; FLT cwh    // Direction cosines of direction w (left)
  FLT cln; FLT clw; FLT clh    // Direction cosines of direction l (lift)

  FLT cetn; FLT cetw; FLT ceth // Eye direction cosines of direction t (forward)
  FLT cewn; FLT ceww; FLT cewh // Eye direction cosines of direction w (left)
  FLT celn; FLT celw; FLT celh // Eye direction cosines of direction l (lift)

  FLT eyen; FLT eyew; FLT eyeh // Position of the eye relative to the aircraft.
  FLT eyedist                  // Eye distance from aircraft

  FLT rtdot; FLT rwdot; FLT rldot // Anti-clockwise rotation rates about the
                                  // t, w and l axes in radian per second.

  // Rotational forces about the aircraft axes.
  FLT rft; FLT rft1     // Rotational force about t axis in ft poundals
  FLT rfw; FLT rfw1     // Rotational force about w axis
  FLT rfl; FLT rfl1     // Rotational force about l axis

  cdrawquad3d       // (x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4)
  cdrawtriangle3d   // (x1,y1,z1, x2,y2,z2, x3,y3,z3)
                    // All floating point values.

  FLT cockpitl      // Height of the pilots eye in ft.

  FLT posn; FLT posw; FLT posh // World coordinates of the aircraft origin.
  FLT cgn;  FLT cgw;  FLT cgh  // World coordinates of the aircraft CG.
                               // about 0.6 ft in negative t direction
                               // from the origin.

  FLT cgndot; FLT cgwdot; FLT cghdot // Speeds in ft/s

  hatdir           // Hat direction
  hatmsecs         // msecs of last hat change
  eyedir           // Eye direction
                   // 0 = cockpit view
                   // 1,...,8 view from behind, behind-left, etc

  // Speed in various directions is measured in ft/s.
  FLT tdot; FLT wdot; FLT ldot // Speed in t, w and l directions
  FLT tdotsq; FLT wdotsq; FLT ldotsq // Speed squared in t, w and l directions

  FLT mass                  // Mass of the aircraft, typically 2000 lbs

  FLT mit; FLT miw; FLT mil // Moment of inertia about t, w and l axes
                            // 1 corresponds to a mass of 1 lb 1 ft from the axis.

  FLT rtdot; FLT rwdot; FLT rldot // Rotation rates about t, w and l axes
                                  // in radians per second.
  FLT rdt;   FLT rdw;   FLT rdl   // Rotational damping moment about
                                  // the t, w and l axes

  // Linear forces in the three directions
  // These are in poundals. 1 poundal will accelerate a mass of 1 lb
  // at a rate of 1 ft/s/s. The force of gravity on a mass of 1 lb is
  // g poundals, giving an acceleration of g ft/s/s.
  FLT ft; FLT ft1       // Force and previous force in t direction
  FLT fw; FLT fw1       // Force and previous force in w direction
  FLT fl; FLT fl1       // Force and previous force in l direction

  // Rotational forces about the three axes.
  FLT rft; FLT rft1     // Current and previous moments about t axis
  FLT rfw; FLT rfw1     // Current and previous moments about w axis
  FLT rfl; FLT rfl1     // Current and previous moments about l axis

  FLT atl // Angle of attack

  // Tables interpolated by rdtab(a, tab) giving the  lift
  // and drag coefficients for a given angle a in degrees.
  lifttab   // Tables giving the lift coefficient.
  dragtab   // Tables giving the drag coefficient.

  // For instance the table lifttab give the lift coeffient causes by air
  // coming towards the aircraft at an a is the angle between O and (ldot,tdot)
  // and the direction t. If a>=0 the airflow is coming towards the upper
  // of the wing. If a<0 the airflow is coming from the more more normal
  // direction toward the underside of the wing. If a=170 the air flow is
  // from behind and 10 degrees above. If a=-170 the air flow is from behind
  // and 10 degrees below.

  //                      l   *(l,t)
  //         _            ^  / \        Positive angle airflow
  //        /  \          | / a )       toward upper surface of
  //       | -------------*-------> t   the wing.
  //        \|            
  //

  // The lift coefficient is greatest when a is abort -15 degrees and
  // falls of rapidly as a becomes more negative. The wings have a
  // built-in angle of attack so even when a=0 the lift coefficient
  // is positive.

  // dragtab give the drag coefficient based on the airflow angle
  // in the tl plane.

  FLT usage         // 0 to 100 percentage cpu usage
}

// Insert the definition of drawtigermoth()
GET "drawtigermoth.b"

LET inprod(FLT a, FLT b, FLT c,
           FLT x, FLT y, FLT z) =
  // Return the cosine of the angle between two unit vectors.
  a*x + b*y + c*z

AND rotate(FLT rt, FLT rw, FLT rl) BE
{ // Rotate the aircraft about axes t, w and l.
  // rt, rw and rl are assumed to be small rotational angles
  // causing rotation about axes t, w, l. Positive values cause
  // anti-clockwise rotations about their axes.

  LET FLT tx =       ctn - rl * cwn + rw * cln
  LET FLT wx =  rl * ctn +      cwn - rt * cln
  LET FLT lx = -rw * ctn + rt * cwn +      cln

  LET FLT ty =       ctw - rl * cww + rw * clw
  LET FLT wy =  rl * ctw +      cww - rt * clw
  LET FLT ly = -rw * ctw + rt * cww +      clw

  LET FLT tz =       cth - rl * cwh + rw * clh
  LET FLT wz =  rl * cth +      cwh - rt * clh
  LET FLT lz = -rw * cth + rt * cwh +      clh

  ctn, ctw, cth := tx, ty, tz
  cwn, cww, cwh := wx, wy, wz
  cln, clw, clh := lx, ly, lz

  adjustlength(@ctn);      adjustlength(@cwn);      adjustlength(@cln) 
  adjustortho(@ctn, @cwn); adjustortho(@ctn, @cln); adjustortho(@cwn, @cln)
}

AND radius2(FLT x, FLT y) = VALOF
{ LET FLT rsq = x*x + y*y
  RESULTIS sys(Sys_flt, fl_sqrt, rsq)
}

AND radius3(FLT x, FLT y, FLT z) = VALOF
{ LET FLT rsq = x*x + y*y + z*z
  RESULTIS sys(Sys_flt, fl_sqrt, rsq)
}

AND adjustlength(v) BE
{ // This helps to keep vector v of unit length
  LET FLT x, FLT y, FLT z = v!0, v!1, v!2
  LET FLT r = radius3(x,y,z)
  v!0 := x / r
  v!1 := y / r
  v!2 := z / r
}

AND adjustortho(a, b) BE
{ // This helps to keep the unit vector b orthogonal to a
  LET FLT a0, FLT a1, FLT a2 = a!0, a!1, a!2
  LET FLT b0, FLT b1, FLT b2 = b!0, b!1, b!2
  LET FLT corr = inprod(a0,a1,a2, b0,b1,b2)
  b!0 := b0 - a0 * corr
  b!1 := b1 - a1 * corr
  b!2 := b2 - a2 * corr
}

AND rdtab(FLT a, tab) = VALOF
{ // A "curve" is represented by a sequence line segments. The vertices
  // have floating point coordinates (a,r) stored in tab. The number of
  // vertices n is held in tab!0. This function computes the value of the
  // curve at any given point a. This is done using linear interpolation
  // on the two points enclosing a. If a is less than the a value of the
  // first coordinate its r value is returned. Similarly, if a is greater
  // than the a value of the last point, the r value of the last point is
  // returned.
  
  // tab -> [n, a0,r0, ..., a(n-1), r(n-1)]

  LET n  = tab!0              // The number of points
  LET p  = @tab!1             // Pointer to first entry
  LET q  = p + 2*(n-1)        // Pointer to the last entry
  
  IF n=0      RESULTIS 0.0    // No entries, return zero
  IF a <= p!0 RESULTIS p!1    // a is too small, take the first value
  IF a >= q!0 RESULTIS q!1    // a is too large, take the last  value

  // p!0 < a and a < q!0,  so there is a pair of points enclosing a.
  
  UNTIL p!0 <= a <= p!2 DO p := p+2  // Find the enclosing line segment
  
  { LET FLT x0, FLT r0 = p!0, p!1  // x0 <= a <= x1
    LET FLT x1, FLT r1 = p!2, p!3
    LET FLT dx = x1-x0
    IF dx=0 RESULTIS r0
    // Use linear interpolation between these two entries.
    RESULTIS r0 + (r1-r0) * (a-x0) / dx
  }
}

AND angle(FLT x, FLT y) = x=0 & y=0 -> 0.0, VALOF
{ // Calculate the angle in degrees between point (x,y) and the
  // x axis using atan2.
  // If (x,y) is above the x-axis the result is betweem 0 and +180
  // If (x,y) is below the x-axis the result is betweem 0 and -180
  LET FLT a = sys(Sys_flt, fl_atan2, y, x) * 180.0 / 3.14159
//drawf(20,  30, "angle: x=%13.3f y=%13.3f => angle = %8.3f*n",
//        x, y, a)
  RESULTIS a
}

LET step() BE
{ // Update the aircraft position, orientation and motion.

  // On entry,
  // the position of the aircraft is at cgn, cgw and cgh in
  // real word coordinates.
  // Its speed in directions n, w and h are cgndot, cgwdot and cghdot.
  // The orientation of the aircraft is given by the direction
  // cosines ctn, ctw, cth   direction of thrust
  //         cwn, cww, cwh   direction of the left wing
  //         cln, clw, clh   direction of lift
  // These three directions are orthogonal.

  // The rate of rotation of the aircraft in direction t, w and l
  // are rtdot, rwdot and rldot.

  // Compute the speeds in directions t, w and l from
  // cgndot, cgwdot and cghdot using the direction cosines.

  { 
    stepcount := stepcount + 1
    IF stepcount MOD 20 = 0 DO
    { LET prevmsecs = msecs1
      LET v = VEC 2
      LET s = VEC 15
      datstamp(v)
      msecs1 := v!1
      datstring(s)
      steprate := 20 * 1000 / FLOAT(msecs1 - prevmsecs)
      //sawritef("stepcount=%n msecs diff=%i5 steprate = %6.3f %s*n",
      //          stepcount, msecs1-prevmsecs, steprate, s+5)
    }
  }

  tdot := cgndot*ctn + cgwdot*ctw + cghdot*cth
  wdot := cgndot*cwn + cgwdot*cww + cghdot*cwh
  ldot := cgndot*cln + cgwdot*clw + cghdot*clh

  // Calculate the square of the speed in each direction.
  tdotsq := tdot * tdot
  wdotsq := wdot * wdot
  ldotsq := ldot * ldot

//sawritef("tdotsq=%13.1f wdotsq=%13.1f ldotsq=%13.1f*n", tdotsq, wdotsq, ldotsq)

  // Calculate the angle of attack in degrees.
  // If tdot = 100.0 and ldot =  100.0, atl will be  45.0
  // If tdot = 100.0 and ldot = -100.0, atl will be -45.0
//sawritef("angle(100.0,  100.0) = %8.2f*n", angle(100.0,  100.0))
//sawritef("angle(100.0, -100.0) = %8.2f*n", angle(100.0, -100.0))
  atl := angle(tdot, ldot) // Positive is airflow is from below in t-l plane

//sawritef("angle(%13.2f, %13.2f) = %8.2f*n", tdot, ldot, angle(tdot, ldot))
//abort(1000)

  // Now deal with the aerodynamic and gravity forces on the aircraft.

  // The linear forces on the CG of the aircraft in directions
  // t, w and l initialised as follows.
  ft,  fw,  fl  := Zro, Zro, Zro

  // These are in poundals.
  // 1 poundal will accelerate a mass of 1 lb at a rate
  // of 1 ft/s/s. A force of mass x g will just hold the aircraft 
  // up against gravity.

  // The rotational forces about axes t, w and l initialised as follows.
  rft, rfw, rfl := Zro, Zro, Zro

  // These are in ft-poundals.
  // 1 ft-poundal will give a body with moment of inertia equivalent 
  // to a mass of 1 lb at a distance of 1 ft a rotational acceleration
  // of 1 radian per second.

  // First calculate the engine RPM.
  targetrpm := 0.0    // If engine is not started
  IF enginestarted DO
  { // The throttle is in the range 0.0 to 1.0
    targetrpm := 600.0 + 1700.0 * throttle +
                 0.7 * tdot        // + air speed effect
  }

  // The rpm take time to change.
  rpm := rpm + (targetrpm - rpm) / 4.0 - 1.0
  IF rpm < 0.0 DO rpm := 0.0
//rpm :=1800.0 // For debugging.

  // Now calculate the thrust.
  thrust := VALOF
  { // At rpm=2200 the aircraft should read a take off speed of 65mph after
    // travelling about 1700 ft along the runway.
    // Assume the angle of attack of the propeller is such that
    // when rpm=2500 the speed at which the thrust is zero is about 200mph
    // or 200*5280 / (60*60) = about 293.0 ft/s.

    // At a speed of tdot ft/s the rpm giving zero thrust is
    // (2500/293) * tdot. Ie linear between 0 and 293.
    // At a speed of 65mph = 95ft/s the rpm = (2500/293)*95 = 810 (rpm0)
    // Assume the the thrust at this speed is Kt * (rpm-rpm0)^2
    // and that the drag is Kd * tdot^2 = Kd * 95^2 = 9025*Kd
    // Assume that at this speed with rpm=2000 the thrust-drag
    // is sufficient to give the aircraft an accelleration of g/8.
    // Thus mass*g/8 = Kt * (2000-810)^2 - 9025 * Kd
    // Assume mass=2000 and g=32, this gives
    // 2000*32/8 = Kt * (2000-810)^2 - 9025 * Kd
    // or 8000 = Kt * 134300 = 9025 * Kd
    LET FLT vmax = 293.0 * rpm / 2500 // Speed in ft/s at which thrust=0
    LET FLT vmaxby2 = vmax/2  // use linear interpolation between vmax and vmaxby2
    LET FLT tmax = 1.7 * mass // Thrust to give acceleration of 1.7 * g
                              // ignoring drag.
    LET FLT t = ?
    IF rpm<600 | tdot>vmax RESULTIS 0.0
    // When rpm>=600 the thrust is proportional to the square of (rpm-600)
    // When rpm=2200 the thrust is sufficient to accelerate the aircraft at g/8
    t := tmax * (rpm-600)*(rpm-600) / ((2200-600) * (2200-600))
    IF tdot<vmaxby2 RESULTIS t
    RESULTIS t * (vmax-tdot) / vmaxby2
  }

  // Next calculate the linear forces on the aircraft.

  // Gravity effect
  ft := ft - mass * k_g * cth  // Gravity in direction t
  fw := fw - mass * k_g * cwh  // Gravity in direction w
  fl := fl - mass * k_g * clh  // Gravity in direction l

  // Lift and drag force of the main wings.
  { //LET atl = angle(tdot, ldot)
    // Lift is proportions to speed squared (= tdot**2 + ldot**2)
    // multiplied by the lift coefficient rdtab(angle, lifttab)
    // When angle=0 and speed=100 ft/sec lift is mass * k_g which
    // just counteracts gravity.
    // rdtab(0, lifttab) = 1.0
    // so lift = mass * k_g * rdtab(angle, lifttab) * speed/100
    LET FLT a = mass * k_g * rdtab(atl, lifttab)
    LET airspeed = radius2(tdot, ldot)
    // Main wing lift force due to air at speed airspeed coming
    // towards the aircraft at angle atl in the tl plane.
    fl := fl + 1.4 * a * airspeed / 100.0
    // Main wing drag force.
    //ft := ft + 0.1 * b * (tdot*tdot+ldot*ldot)
  }

  // Airframe drag force -- using quartic to increase the
  // drag effect at high speed.
  ft := ft - 0.020 * mass * (tdot * tdot * tdot * tdot) / 100000000.0

  // Side effect
  fw  := fw  - 0.5  * mass * wdot  // Sideways force

  // Thust force
  ft := ft + thrust
  
  // Apply linear forces ft, fw and fl using the trapizoidal rule
  // for integration.
  tdot := tdot + (ft+ft1)/(mass*steprate)
  wdot := wdot + (fw+fw1)/(mass*steprate)
  ldot := ldot + (fl+fl1)/(mass*steprate)

  ft1, fw1, fl1 := ft, fw, fl  // Save the previous values

  // Calculate the real world velocity
  cgndot := tdot*ctn + wdot*cwn + ldot*cln
  cgwdot := tdot*ctw + wdot*cww + ldot*clw
  cghdot := tdot*cth + wdot*cwh + ldot*clh

  // Calculate new n, w and h positions.
  cgn := cgn + cgndot / steprate
  cgw := cgw + cgwdot / steprate
  cgh := cgh + cghdot / steprate

//writef("cgn=%13.3f  cgw=%13.3f  cgh=%13.3f*n", cgn, cgw, cgw)
//abort(1003)


  // Calculate the rotational forces rft, rfw and rfl

  // Dihedral effect
  // If wdot>0 there is a clockwise force about the t axis
  rft := rft - 0.005 * mit * wdot

  // Fixed stabiliser effect 
  // If ldot>0 there is an anti-clockwise force about the w axis
  rfw := rfw + 4500*(ldot - 15.0 * rwdot) / mil

  // Fixed fin effect
  // If wdot>0 there is an clockwise force about the l axis.
  rfl := rfl - 950*(wdot + 15.0 * rldot) / mil
  
  // Aileron effect
  rft :=  rft - 0.001 * aileron * mit * tdot

  // Elevator effect
  rfw :=  rfw - 0.002 * elevator * miw * (tdot+rpm/50)

  // Rudder effect
  rfl :=  rfl + 0.001 * rudder * mil * (tdot+rpm/10)

  // Apply rotational damping.
  // rtdot, rwdot and rldot are in radians per second.
  rft := rft - 0.8 * mit * rtdot
  rfw := rfw - 3.0 * miw * rwdot
  rfl := rfl - 0.8 * mil * rldot

//writef("rft=%9.6f rft1=%9.6f*n", rft, rft1)  
//writef("rfw=%9.6f rfw1=%9.6f*n", rft, rft1)  
//writef("rfl=%9.6f rfl1=%9.6f*n", rft, rft1)  

  // Apply rotational effects using the trapizoidal rule.
  rtdot := rtdot + (rft+rft1)/(mit * steprate * 2)
  rwdot := rwdot + (rfw+rfw1)/(miw * steprate * 2)
  rldot := rldot + (rfl+rfl1)/(mil * steprate * 2)

  rft1, rfw1, rfl1 := rft, rfw, rfl // Save previous values

  // Limit rotational rates to no more than 1 radian per second
  //IF rtdot >  1.0 DO rtdot :=  1.0
  //IF rtdot < -1.0 DO rtdot := -1.0
  //IF rwdot >  1.0 DO rwdot :=  1.0
  //IF rwdot < -1.0 DO rwdot := -1.0
  //IF rldot >  1.0 DO rldot :=  1.0
  //IF rldot < -1.0 DO rldot := -1.0

  // Rotate the aircraft.
  // Anti-clockwise rotation rates in radians per second
  // about axes t, w and l.
  rotate(rtdot/steprate, rwdot/steprate, rldot/steprate)

  // Test for contact with the ground.
  IF cgh < 10.0 DO
  { // The aircraft is near the ground

    IF cgh < 2.0 | clh<0.8 DO
    { crashed := TRUE
      stepping := FALSE
      RETURN
    }
  }
}

AND plotcraft() BE
{ IF depthv FOR i = 0 TO screenxsize*screenysize-1 DO
    depthv!i := FLOAT maxint
  // Draw a Tigermoth
  drawtigermoth(elevator, aileron, rudder)
}

AND gdrawquad3d(FLT x1, FLT y1, FLT z1,
                FLT x2, FLT y2, FLT z2,
                FLT x3, FLT y3, FLT z3,
                FLT x4, FLT y4, FLT z4) BE
{ // Draw a 3D quad (not rotated)
  LET FLT sx1, FLT sy1, FLT sz1 = ?,?,?
  LET FLT sx2, FLT sy2, FLT sz2 = ?,?,?
  LET FLT sx3, FLT sy3, FLT sz3 = ?,?,?
  LET FLT sx4, FLT sy4, FLT sz4 = ?,?,?

  UNLESS screencoords(x1-eyen-cgn, y1-eyew-cgw, z1-eyeh-cgh, @sx1) RETURN
  UNLESS screencoords(x2-eyen-cgn, y2-eyew-cgw, z2-eyeh-cgh, @sx2) RETURN
  UNLESS screencoords(x3-eyen-cgn, y3-eyew-cgw, z3-eyeh-cgh, @sx3) RETURN
  UNLESS screencoords(x4-eyen-cgn, y4-eyew-cgw, z4-eyeh-cgh, @sx4) RETURN

  drawquad3d(FIX sx1, FIX sy1, FIX sz1,
             FIX sx2, FIX sy2, FIX sz2,
             FIX sx3, FIX sy3, FIX sz3,
             FIX sx4, FIX sy4, FIX sz4)
}

AND cdrawquad3d(FLT x1, FLT y1, FLT z1,
                FLT x2, FLT y2, FLT z2,
                FLT x3, FLT y3, FLT z3,
                FLT x4, FLT y4, FLT z4) BE
{ LET FLT rx1 = x1*ctn + y1*cwn + z1*cln
  LET FLT ry1 = x1*ctw + y1*cww + z1*clw
  LET FLT rz1 = x1*cth + y1*cwh + z1*clh

  LET FLT rx2 = x2*ctn + y2*cwn + z2*cln
  LET FLT ry2 = x2*ctw + y2*cww + z2*clw
  LET FLT rz2 = x2*cth + y2*cwh + z2*clh

  LET FLT rx3 = x3*ctn + y3*cwn + z3*cln
  LET FLT ry3 = x3*ctw + y3*cww + z3*clw
  LET FLT rz3 = x3*cth + y3*cwh + z3*clh

  LET FLT rx4 = x4*ctn + y4*cwn + z4*cln
  LET FLT ry4 = x4*ctw + y4*cww + z4*clw
  LET FLT rz4 = x4*cth + y4*cwh + z4*clh

  LET FLT sx1, FLT sy1, FLT sz1 = ?,?,?
  LET FLT sx2, FLT sy2, FLT sz2 = ?,?,?
  LET FLT sx3, FLT sy3, FLT sz3 = ?,?,?
  LET FLT sx4, FLT sy4, FLT sz4 = ?,?,?

  UNLESS screencoords(rx1-eyen, ry1-eyew, rz1-eyeh, @sx1) RETURN
  UNLESS screencoords(rx2-eyen, ry2-eyew, rz2-eyeh, @sx2) RETURN
  UNLESS screencoords(rx3-eyen, ry3-eyew, rz3-eyeh, @sx3) RETURN
  UNLESS screencoords(rx4-eyen, ry4-eyew, rz4-eyeh, @sx4) RETURN

  drawquad3d(FIX sx1, FIX sy1, sz1,
             FIX sx2, FIX sy2, sz2,
             FIX sx3, FIX sy3, sz3,
             FIX sx4, FIX sy4, sz4)
}

AND cdrawtriangle3d(FLT x1, FLT y1, FLT z1,
                    FLT x2, FLT y2, FLT z2,
                    FLT x3, FLT y3, FLT z3) BE
{ LET FLT rx1 = x1*ctn + y1*cwn + z1*cln
  LET FLT ry1 = x1*ctw + y1*cww + z1*clw
  LET FLT rz1 = x1*cth + y1*cwh + z1*clh

  LET FLT rx2 = x2*ctn + y2*cwn + z2*cln
  LET FLT ry2 = x2*ctw + y2*cww + z2*clw
  LET FLT rz2 = x2*cth + y2*cwh + z2*clh

  LET FLT rx3 = x3*ctn + y3*cwn + z3*cln
  LET FLT ry3 = x3*ctw + y3*cww + z3*clw
  LET FLT rz3 = x3*cth + y3*cwh + z3*clh

  LET FLT sx1, FLT sy1, FLT sz1 = ?,?,?
  LET FLT sx2, FLT sy2, FLT sz2 = ?,?,?
  LET FLT sx3, FLT sy3, FLT sz3 = ?,?,?

  UNLESS screencoords(rx1-eyen, ry1-eyew, rz1-eyeh, @sx1) RETURN
  UNLESS screencoords(rx2-eyen, ry2-eyew, rz2-eyeh, @sx2) RETURN
  UNLESS screencoords(rx3-eyen, ry3-eyew, rz3-eyeh, @sx3) RETURN

//newline()
//writef("x1=%13.3f y1=%13.3f z1=%13.3f*n", x1, y1, z1)
//writef("x2=%13.3f y2=%13.3f z2=%13.3f*n", x2, y2, z2)
//writef("x3=%13.3f y3=%13.3f z3=%13.3f*n", x3, y3, z3)

//writef("ctn=%6.3f cwn=%6.3f cln=%6.3f radius3=%8.3f*n", ctn, cwn, cln, radius3(ctn,cwn,cln))
//writef("ctw=%6.3f cww=%6.3f clw=%6.3f radius3=%8.3f*n", ctw, cww, clw, radius3(ctw,cww,clw))
//writef("cth=%6.3f cwh=%6.3f clh=%6.3f radius3=%8.3f*n", cth, cwh, clh, radius3(cth,cwh,clh))

//writef("sx1=%13.3f sy1=%13.3f sz1=%13.3f*n", sx1, sy1, sz1)
//writef("sx2=%13.3f sy2=%13.3f sz2=%13.3f*n", sx2, sy2, sz2)
//writef("sx3=%13.3f sy3=%13.3f sz3=%13.3f*n", sx3, sy3, sz3)

  drawtriangle3d(FIX sx1, FIX sy1, sz1,
                 FIX sx2, FIX sy2, sz2,
                 FIX sx3, FIX sy3, sz3)
  //writef("sx1=%5i sy1=%5i sz1=%13.1f*n", FIX sx1, FIX sy1, sz1)
  //writef("sx2=%5i sy2=%5i sz2=%13.1f*n", FIX sx2, FIX sy2, sz2)
  //writef("sx3=%5i sy3=%5i sz2=%13.1f*n", FIX sx3, FIX sy3, sz3)

  //updatescreen()
//delay(1000)
//abort(1000)
}

AND screencoords(FLT x, FLT y, FLT z, v) = VALOF
{ // If the point (x,y,z) is in view, set v!0, v!1 and v!2 to
  // the integer screen coordinates and depth and return TRUE
  // otherwise return FALSE
  LET FLT sx = x*cewn + y*ceww + z*cewh // Horizontal
  LET FLT sy = x*celn + y*celw + z*celh // Vertical
  LET FLT sz = x*cetn + y*cetw + z*ceth // Depth
  LET FLT fscreensize = fscreenxsize<=fscreenysize -> fscreenxsize, fscreenysize

//writef("screencoords: x=%13.3f  y=%13.3f  z=%13.3f*n", x,y,z)
//writef("cetn=%6.3f  cetw=%6.3f  ceth=%6.3f*n", cetn,cetw,ceth)
//writef("cewn=%6.3f  ceww=%6.3f  cewh=%6.3f*n", cewn,ceww,cewh)
//writef("celn=%6.3f  celw=%6.3f  celh=%6.3f*n", celn,celw,celh)
//writef("eyen=%13.3f  eyew=%13.3f  eyeh=%13.3f*n", eyen,eyew,eyeh)
//writef("  sx=%13.3f    sy=%13.3f    sz=%13.3f*n", sx,sy,sz)

  // Test that the point is in view, ie at least 1.0ft in front
  // and no more than about 27 degrees (inverse tan 1/2) from the
  // direction of view.
  IF sz<1 &  sz*sz / 2 >= sx*sx + sy*sy
    RESULTIS FALSE

  // A point screensize pixels away from the centre of the screen is
  // 45 degrees from the direction of view.
  // Note that many pixels in this range are off the screen.
  v!0 := fscreenxsize * 0.5 - fscreensize * (sx / sz) * 2
  v!1 := fscreenysize * 0.5 + fscreensize * (sy / sz) * 2   + 200.0
  v!2 := sz // This distance into the screen in arbitrary units, used
            // for hidden surface removal.
//writef("screencoords:  v!0=%13.3f  v!1=%13.3f  v!2=%13.3f)*n", v!0, v!1, v!2)
//abort(1119)
  RESULTIS TRUE
}

AND orthocoords(FLT n, FLT w, FLT h, v) = VALOF
{ // (n,w,h) is a point relative to (cgn,cgw,cgh).
  // It is viewed with orientation (t,w,l) using an orthogonal projection.
  // The screen (x,y) coordinates are placed in v!0 and v!1.
  // The result is TRUE if (n,w,h) is in front.
  LET sx, sy = 0.0, 0.0
  LET res = FALSE

  // Screen z is the inner product of (n,w,h) and (ctn,ctw,cth)
  IF n*ctn + w*ctw + h*cth > 0.0 DO
  { // The direction of motion circle is in front
    // Screen x is the inner product of (n,w,h) and (cwn,cww,cwh)
    sx := n*cwn + w*cww + h*cwh
    // Screen y is the inner product of (n,w,h) and (cln,clw,clh)
    sy := n*cln + w*clw + h*clh
    res := TRUE

    //writef("orthocoords:*n")
    //writef("  ctn=%6.3f  ctw=%6.3f  cth=%6.3f*n", ctn,ctw,cth)
    //writef("  cwn=%6.3f  cww=%6.3f  cwh=%6.3f*n", cwn,cww,cwh)
    //writef("  cln=%6.3f  clw=%6.3f  clh=%6.3f*n", cln,clw,clh)
    //writef(" n=%13.3f  w=%13.3f  h=%13.3f*n", n,w,h)
    //writef("sx=%13.3f sy=%13.3f*n", sx,sy)
    v!0 := FIX(fscreenxsize - 100 - sx)
    v!1 := FIX(fscreenysize * 0.5 + sy)
    //writef("v!0=%6i v!1=%6i*n", v!0,v!1)
    RESULTIS TRUE
  }

  v!0 := -1
  v!1 := -1
  //writef("v!0=%6i v!1=%6i*n", v!0,v!1)
  RESULTIS FALSE
}

AND draw_artificial_horizon() BE
{ // This function draws the artificial horizon and a small circle
  // representing the direction of travel.
  // The n and w components of the direction of thrust t are used
  // to make a horizontal vector (n,w,0) which is above or below
  // the direction of thrust. This is then scaled to make it of
  // unit length. Suppose the resulting vector is d = (dn,dw,0).
  // Let P be a point in direction d 100 ft from the aircraft's CG,
  // ie (100dn, 100dw,0).  This point will be above or below the
  // line in direction t from the CG.
  // P (cgn+100dn,cgw+100dw,cgh) is in world coordinates.
  // The artificial horizon is made up of four line segments
  // A-B, B-C, C-D and D-E where A, B, D and E are on
  // the horizontal line passing through P at right angles to d.
  // A is 30ft to the left of P and E is 30ft to the right of P.
  // On the screen, B, C and D form an equilateral triangle half
  // way between A and E.

  // The direction of motion is represented by a small circle at
  // point X which has coordinates (cgn+100xn,cgw+100xw,cgh+100xh)
  // where (xn,xw,xh) is a unit vector in direction
  // (cgndot,cgwdot,cghdot). The screen position of X is calculated
  // using the same orthogonal projection as the points A, B, C, D
  // and E.


  LET px, py = ?, ?  // For screen coordinates
  LET ax, ay = ?, ?  // For screen coordinates
  LET bx, by = ?, ?  // For screen coordinates
  LET cx, cy = ?, ?  // For screen coordinates
  LET dx, dy = ?, ?  // For screen coordinates
  LET ex, ey = ?, ?  // For screen coordinates
  LET FLT n,  FLT w,  FLT h  =  ctn, ctw, 0.0  // A horizontal vector
  LET FLT a,  FLT b,  FLT c  =    ?,   ?,   ?  // Unit vector orthogonal to (n,w,h)
  LET FLT Pn, FLT Pw, FLT Ph =    ?,   ?,   ?
  LET FLT An, FLT Aw, FLT Ah =    ?,   ?,   ?
  LET FLT En, FLT Ew, FLT Eh =    ?,   ?,   ?
  LET FLT Xn, FLT Xw, FLT Xh =    ?,   ?,   ?  // A point in direction
                                               // (cgndot,cgwdot,cghdot).

  setcolour(col_white)

  //{ moveto(100,200)
  //  drawto(110,210)
  //}
//updatescreen()
//abort(1002)


  adjustlength(@n)  // Make (n,w,0) a unit vector, direction d.

  // Make a unit vector in direction A->E (orthogonal to d).
  a, b, c := w, -n, 0.0

  // Set P to be 100ft from CG in direction d
  Pn, Pw, Ph := cgn+100*n,  cgw+100*w,  cgh // A point on the horizon
                                            // 100ft from CG.
  // Set A 30ft left of from P.
  An, Aw, Ah := Pn-30*a, Pw-30*b, Ph
  // Set A 30ft left of from P.
  En, Ew, Eh := Pn+30*a, Pw+30*b, Ph

  //    A-----------B  P  D----------E
  //                 \   /
  //                   C
  //
  // AE is othogonal to the line from CG to P.
  // 

  orthocoords(An-cgn, Aw-cgw, Ah-cgh, @ax)
  orthocoords(En-cgn, Ew-cgw, Eh-cgh, @ex)
  px, py := (ax+ex)/2, (ay+ey)/2
  bx, by := px + (ax-ex)*5/60, py + (ay-ey)*5/60
  dx, dy := px - (ax-ex)*5/60, py - (ay-ey)*5/60
  // BCD is an equilateral triangle with sides of length 10,
  // CP has length appoximately 8.66.
  // (ey-ay, ax-ay) is a vector of length 60 in direction PC
  // so the screen coordinates of C can be calculated as follows.
  cx, cy := px+(ey-ay)*8_66/60_00, py+(ax-ex)*8_66/60_00
  // We can now draw the artificial horizon
  moveto(ax,ay)
  drawto(bx,by)
  drawto(cx,cy)
  drawto(dx,dy)
  drawto(ex,ey)

  // Set (n,w,h) to be a point in direction (cgndot,cgwdot,cghdot).
  n, w, h :=  cgndot, cgwdot, cghdot
  // Make (n,w,h) a unit vector
  adjustlength(@n)
  // X is the centre of the direction of motion circle.
  Xn, Xw, Xh := cgn+100*n, cgw+100*w, cgh+100*h

//drawf(20, 85, "Xn=%i6 Xn=%i6 Dn=%i6", FIX (Xn-cgn), FIX (Xw-cgw), FIX (Xh-cgh))
  IF orthocoords(Xn-cgn, Xw-cgw, Xh-cgh, @px) DO
  { drawcircle(px, py, 5)
//writef("Draw circle at %n %n*n", px,py)
  }
//updatescreen()
//abort(1001)
}

AND draw_ground_point(FLT x, FLT y) BE
{ LET FLT gx, FLT gy, FLT gz = Zro, Zro, Zro
//newline()
//writef("draw_ground_point: x=%13.2f y=%13.2f*n", x, y)
//writef("draw_ground_point: cgn=%13.2f cgw=%13.2f cgh=%13.2f*n", cgn, cgw, cgh)
//abort(1001)
  IF screencoords(x-cgn, y-cgw, -cgh-cockpitl, @gx) DO
  { 
//writef("gx=%13.3f  gy=%13.3f gz=%13.3f*n", gx, gy, gz)
    drawrect(FIX gx, FIX gy, FIX gx+2, FIX gy+2)
    updatescreen()
//abort(1000)
  }
}

AND drawgroundpoints() BE
{
  setcolour(col_red)
  gdrawquad3d( 0.0,   -5.0, 1.0,
              20.0,   -5.0, 1.0,
              20.0,    5.0, 1.0,
               0.0,    5.0, 1.0)
  setcolour(col_green)
  gdrawquad3d(20.0,   -5.0, 1.0,
              40.0,   -5.0, 1.0,
              40.0,    5.0, 1.0,
              20.0,    5.0, 1.0)
//  updatescreen()

//IF FALSE DO
  FOR x = 0 TO 200-150 BY 20 DO
  { LET FLT fx = FLOAT x
    FOR y = -50 TO 45 BY 5 DO
    { LET FLT fy = FLOAT y
      LET r = ABS(3*x + 5*y) MOD 73
      LET g = ABS(53*x + 25*y) MOD 73
      LET b = ABS(103*x + 125*y) MOD 73
//sawritef("fx=%13.3f fy=%13.3f*n", fx, fy)      
      setcolour(maprgb(30+r,30+g,30+b))
//writef("Calling gdrawquad3d*n")
      gdrawquad3d(fx,    fy,   Zro,
                  fx+20, fy,   Zro,
                  fx+20, fy+5, Zro,
                  fx,    fy+5, Zro)
      //updatescreen()
    }
  }

RETURN
    

  setcolour(col_white)
  ///draw_ground_point(      Zro,       Zro)
IF FALSE DO
  FOR x = 0 TO 3000 BY 100 DO
  { LET FLT fx = FLOAT x
    draw_ground_point(fx, -50.0)
    draw_ground_point(fx, +50.0)
  }
//  draw_ground_point(3000.0, Zro)

IF FALSE DO
  FOR k = 1000 TO 10000 BY 1000 DO
  { LET FLT fk = FLOAT k
    setcolour(col_lightmajenta)
    IF fk > 3000.0 DO draw_ground_point( k,  Zro)
    setcolour(col_white)
    draw_ground_point(-fk,  Zro)
    setcolour(col_red)
    draw_ground_point( Zro,  fk)
    setcolour(col_green)
    draw_ground_point( Zro, -fk)
  }
}

AND initposition(n) BE SWITCHON n INTO
{ DEFAULT:

  CASE 1: // Take off position
    cgn,    cgw,    cgh    := 100.0,  0,  100.0
    cgndot, cgwdot, cghdot := Zro,  Zro,  Zro

    tdot,   wdot,   ldot   := Zro, Zro, Zro  // Not needed

    // The aircraft orientation
    ctn, ctw, cth := One, Zro, Zro  // Direction cosines of aircraft
    cwn, cww, cwh := Zro, One, Zro
    cln, clw, clh := Zro, Zro, One

    rtdot,  rwdot,  rldot  := Zro, Zro, Zro  // Rate of rotation

    // Linear forces
    ft,   fw,   fl   := Zro, Zro, Zro
    ft1,  fw1,  fl1  := Zro, Zro, Zro // Previous linear forces

    // Rotational forces
    rft,  rfw,  rfl  := Zro, Zro, Zro
    rft1, rfw1, rfl1 := Zro, Zro, Zro // Previous rotational forces

    stepping := TRUE
    crashed := FALSE
    enginestarted, rpm := FALSE, 0.0
    targetrpm := rpm
    RETURN

  CASE 2: // Position on the glide slope
    cgn,    cgw,    cgh    := -4000.0, Zro,  1000.0      // Height of 1000 ft
    cgndot, cgwdot, cghdot :=   100.0, Zro,   Zro

    tdot,   wdot,   ldot   :=   100.0, Zro,   Zro  // Not needed

    // The aircraft orientation
    ctn, ctw, cth := One, Zro, Zro  // Direction cosines with
    cwn, cww, cwh := Zro, One, Zro  // six decimal digits
    cln, clw, clh := Zro, Zro, One  // after to decimal point.

    rtdot, rwdot, rldot :=  Zro, Zro, Zro

    // Linear forces
    ft,   fw,   fl   := Zro, Zro, Zro
    ft1,  fw1,  fl1  := Zro, Zro, Zro // Previous linear forces

    // Rotational forces
    rft,  rfw,  rfl  := Zro, Zro, Zro
    rft1, rfw1, rfl1 := Zro, Zro, Zro // Previous rotational forces

    stepping := TRUE
    crashed := FALSE
    enginestarted, rpm := TRUE, 1600.0
    targetrpm := rpm
    RETURN

  CASE 3: // Set flying level at 10000 ft at 65mph
    cgn,    cgw,    cgh    := -10_000.0, Zro, 10000.0    // Height of 10000 ft
    cgndot, cgwdot, cghdot :=      95.0, Zro,   Zro      // 65mph = 95 ft/s
//cgwdot := 15.0
    tdot,   wdot,   ldot   :=  cgndot, cgwdot, cghdot

    // The aircraft orientation
    ctn, ctw, cth := One, Zro, Zro  // Direction cosines of aircraft.
    cwn, cww, cwh := Zro, One, Zro
    cln, clw, clh := Zro, Zro, One

    rtdot,  rwdot,  rldot  := Zro, Zro, Zro  // Rate of rotation

    // Linear forces
    ft,   fw,   fl   := Zro, Zro, Zro
    ft1,  fw1,  fl1  := Zro, Zro, Zro // Previous linear forces

    // Rotational forces
    rft,  rfw,  rfl  := Zro, Zro, Zro
    rft1, rfw1, rfl1 := Zro, Zro, Zro // Previous rotational forces

    ft1,  fw1,  fl1  := Zro, Zro, Zro // Previous linear forces
    rft1, rfw1, rfl1 := Zro, Zro, Zro // Previous rotational forces

    stepping := TRUE
    crashed := FALSE
    enginestarted, rpm := TRUE, 1900.0
    targetrpm := rpm
    RETURN
}

LET start() = VALOF
{ LET v = VEC 2
  datstamp(v)
  msecs1 := v!1 
  One := 1.0
  Zro := 0.0
  stepcount := 0
  steprate := 5.0

/*
  FOR i = 1 TO 10 DO
  { LET t0 = sys(Sys_cputime)
    LET v = VEC 3
    LET s = VEC 16
    datstamp(v)
    datstring(s)
    sawritef("%i4: msecs1=%i8  %s*n", i, v!1, s+5)
    delay(1000)
  }
*/

//writef("%8x %-%32b*n%-%20.6f*n%-%20.3e*n", 123.456789)
//writef("%8x %-%32b*n%-%20.6f*n%-%20.3e*n", 123.456789e20)
//writef("%8x %-%32b*n%-%20.6f*n%-%20.3e*n", 123.456789e-20)
//RESULTIS 0 

//writef("radius3(0.0, 0.0,  0.0) = %13.3f*n", radius3(0.0, 0.0,  0.0))
//writef("radius3(1.0, 1.0,  0.0) = %13.3f*n", radius3(1.0, 1.0,  0.0))
//writef("radius3(3.0, 0.0,  4.0) = %13.3f*n", radius3(3.0, 0.0,  4.0))
//writef("radius3(0.0, 3.0, -4.0) = %13.3f*n", radius3(0.0, 3.0, -4.0))
//abort(1000)

//  writef("3.1               =%20.9e %-%13.9f*n", 3.1)
//  writef("3.14              =%20.9e %-%13.9f*n", 3.14)
//  writef("3.141             =%20.9e %-%13.9f*n", 3.141)
//  writef("3.1415            =%20.9e %-%13.9f*n", 3.1415)
//  writef("3.14159           =%20.9e %-%13.9f*n", 3.14159)
//  writef("3.141592          =%20.9e %-%13.9f*n", 3.141592)
//  writef("3.1415926         =%20.9e %-%13.9f*n", 3.1415926)
//  writef("3.14159265        =%20.9e %-%13.9f*n", 3.14159265)
//  writef("3.141592653       =%20.9e %-%13.9f*n", 3.141592653)
//  writef("3.1415926535      =%20.9e %-%13.9f*n", 3.1415926535)
//  writef("3.14159265358     =%20.9e %-%13.9f*n", 3.14159265358)
//  writef("3.141592653589    =%20.9e %-%13.9f*n", 3.141592653589)
//  writef("3.1415926535897   =%20.9e %-%13.9f*n", 3.1415926535897)
//  writef("3.14159265358979  =%20.9e %-%13.9f*n", 3.14159265358979)
//  writef("3.141592653589793 =%20.9e %-%13.9f*n", 3.141592653589793)
//RESULTIS 0
//  angle( 1.0,  0.0)
//  angle( 0.0,  1.0)
//  angle(-1.0,  0.0)
//  angle( 0.0, -1.0)
//  angle( 1.0,  1.0)
//  angle(-1.0,  1.0)
//  angle(-1.0, -1.0)
//  angle( 1.0, -1.0)
//RESULTIS 0

  IF FALSE DO
  { // Test rdtab
    writef("Testing rdtab*n")
    FOR a = -200 TO 200 BY 10 DO
    { LET fa = FLOAT a
      LET val = Zro
      LET tab = TABLE    4,             // Number of points in the curve
                    -150.0, 100.000,
                       0.0, -50.000,
                     100.0,   0.000,
                     150.0, 100.000
      val := rdtab(fa, tab)
      IF a MOD 25 = 0 DO writef("*n%9.1f:", fa)
      writef(" %8.3f", rdtab(fa, tab))
    }
    newline()
    RESULTIS 0
  }

  IF FALSE DO
  { // The the angle function
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n", 1.000, 1.000, angle( 1.000, 1.000))
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n", 0.000, 1.000, angle( 0.000, 1.000))
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n",-1.000, 1.000, angle(-1.000, 1.000))
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n",-1.000,-1.000, angle(-1.000,-1.000))
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n", 1.000,-1.000, angle( 1.000,-1.000))
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n",-1.000, 0.000, angle(-1.000, 0.000))
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n", 0.060, 0.001, angle( 0.060, 0.001))
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n", 0.060,-0.001, angle( 0.060,-0.001))

    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n",-1.000, 0.001, angle(-1.000, 0.001))
    writef("x=%8.3f  y=%8.3f    angle=%9.3f*n",-1.000,-1.000, angle(-1.000,-1.000))
    RESULTIS 0
  }

  initposition(1) // Get ready for take off

  cetn, cetw, ceth := ctn, ctw, cth
  cewn, ceww, cewh := cwn, cww, cwh
  celn, celw, celh := cln, clw, clh

  eyen, eyew, eyeh := Zro, Zro, Zro   // Relative eye position
  //hatdir, hatmsecs, eyedir := 0, 0, 0
  hatdir, hatmsecs := #b0001, 0 // From behind
  eyedir := 1
  eyedist := 100.0  // Eye x or y distance from aircraft

  cockpitl := 6.0   // Cockpit 8 feet above the ground

  c_throttle, c_elevator, c_aileron, c_rudder := Zro, Zro, Zro, Zro
  c_trimthrottle, c_trimelevator, c_trimaileron, c_trimrudder := Zro, Zro, Zro, Zro
  throttle, elevator, aileron, rudder := Zro, Zro, Zro, Zro

  // Set rotational damping parameters 
  rdt,   rdw,  rdl := 1.800, 1.800, 1.800

  ft,     fw,    fl   := Zro, Zro, Zro
  ft1,    fw1,   fl1  := Zro, Zro, Zro
  rft,    rfw,   rfl  := Zro, Zro, Zro
  rft1,  rfw1,  rfl1  := Zro, Zro, Zro
  rtdot, rwdot, rldot := Zro, Zro, Zro
  //writef("%13.1f %13.1f %13.1f*n", cgn,   cgw, cgh)

  usage := 0

  initsdl()
  mkscreen("Tiger Moth", 800, 500)

  // Declare a few colours in the pixel format of the screen
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
  col_darkred     := maprgb( 64,   0,   0)
  col_darkmajenta := maprgb( 64,   0,  64)
  col_darkcyan    := maprgb( 64,  64,   0)
  col_gray        := maprgb(128, 128, 128)
  col_lightblue   := maprgb(128, 128, 255)
  col_lightgreen  := maprgb(128, 255, 128)
  col_lightyellow := maprgb(128, 255, 255)
  col_lightred    := maprgb(255, 128, 128)
  col_lightmajenta:= maprgb(255, 128, 255)
  col_lightcyan   := maprgb(255, 255, 128)

  done      := FALSE
  debugging := TRUE//FALSE
  plotusage := FALSE

  mass := 2000.0    // Aircraft mass is 2000 lbs
  mit :=   200.0    // Moment of inertial about t
  miw :=   400.0    // Moment of inertial about w
  mil :=   400.0    // Moment of inertial about l

  lifttab := TABLE       10,         // 10 entries
                   -180.000, -0.100, // The angle is relative to direction t
                    -90.000,  0.400,
                    -15.000,  0.100, // Stalled
                    -11.000,  2.000,
                      0.000,  1.000, // Lift coefficient when ldot=0
                      4.000,  0.000,
                     19.000, -0.600,
                     24.000, -0.100, // Inverted stall
                     90.000, -0.400,
                    180.000, -0.100


  //initposition(3)
  currcolour := col_red
  drawtriangle(100,200, 300,500, 500,200)
  updatescreen()
  plotscreen()
//abort(1000)

  UNTIL done DO
  { // Read joystick and keyboard events
    LET t0 = sdlmsecs()
    LET t1 = ?

    processevents()

    IF stepping DO step()

    //writef("x=%9.2f y=%9.2f h=%9.2f %9.2f*n", cgn, cgw, cgh, tdot)
    plotscreen()

    updatescreen()

    t1 := sdlmsecs()
//writef("time %9.3d  %9.3d  %9.3d %9.3d*n", t0, t1, t1-t0, t0+100-t1)
    usage := 100*(t1-t0)/100

    //IF t0+100 < t1 DO
      //sdldelay(t0+100-t1)
      sdldelay(100)
      //sdldelay(900)
//abort(1111)
  }

  writef("*nQuitting*n")
  sdldelay(0_100)
  closesdl()
  RESULTIS 0
}

AND drawcontrols() BE
{ LET mx = screenxsize/2
  LET my = screenysize - 70 //- 100

  seteyeposition()

  fillsurf(col_blue)

  setcolour(col_lightcyan)
  
  drawstr(240, 50, done -> "Quitting", "Tiger Moth Flight Simulator")

  setcolour(col_lightgray) // Draw runway line
  moveto(mx-1, my)
  drawby(0, FIX(3000.0/100.0))
  moveto(mx,   my)
  drawby(0, FIX(3000.0/100.0))
  moveto(mx+1, my)
  drawby(0, FIX(3000.0/100.0))

  { LET dx =    FIX(ctn*20)  // Orientation of the aircraft
    LET dy =    FIX(ctw*20)
    LET sdx =   dx / 10      // Ground speed of the aircraft
    LET sdy =   dy / 10
    LET x  = mx-FIX(cgw/100)
    LET y  = my+FIX(cgn/100)
    LET tx  = x+5*dy/8
    LET ty  = y-5*dx/8
    setcolour(col_red)       // Draw aircraft symbol
    moveto(x-dy/4, y+dx/4)   // Fuselage
    drawby(+dy, -dx)
    moveto( x-dx/2,  y-dy/2) // Wings
    drawby(+dx, +dy)
    moveto(tx, ty)           // Tail
    moveby(dx/4, dy/4)
    drawby(-dx/2, -dy/2)
  }

  // Draw the controls
  setcolour(col_darkgray)
  drawfillrect(screenxsize-20-100, screenysize-20-100, // Joystick
               screenxsize-20,     screenysize-20)
  drawfillrect(screenxsize-50-100, screenysize-20-100, // Throttle
               screenxsize-30-100, screenysize-20)
  drawfillrect(screenxsize-20-100, screenysize-50-100, // Rudder
               screenxsize-20,     screenysize-30-100)

  IF crashed DO
  { setcolour(col_red)
    drawf(mx-50, my+50, "CRASHED")
  }

  setcolour(col_green)     // Real world velocity
  moveto(mx, my)
  drawby(-FIX(cgwdot/10), FIX(cgndot/10))

  { LET pos = FIX(80 * throttle)
    setcolour(col_red)
    drawfillrect(screenxsize-45-100, pos+screenysize-15-100,
                 screenxsize-35-100, pos+screenysize- 5-100)
  }

  { LET pos = FIX(45 * rudder)
    setcolour(col_red)
    drawfillrect(pos+screenxsize-25-50, -5+screenysize-40-100,
                 pos+screenxsize-15-50, +5+screenysize-40-100)
  }

  { LET posx = FIX(45 * aileron)
    LET posy = FIX(45 * elevator)
    setcolour(col_red)
    drawfillrect(posx+screenxsize-25-50, posy+screenysize-25-50,
                 posx+screenxsize-15-50, posy+screenysize-15-50)
  }

  setcolour(col_white)

  IF debugging DO
  { 
    drawf(20, my+ 15, "rpm=%6.1f target rpm=%6.1f thrust=%8.3f",
                       rpm, targetrpm, thrust)
    drawf(20, my,     "Throttle=%6.3f Elevator=%6.3f Aileron=%6.3f Rudder=%6.3f",
                       throttle,      elevator,      aileron,      rudder)
    drawf(20, my- 15, "cgn=    %13.3f cgw=   %13.3f cgh=   %13.3f", cgn,   cgw,   cgh)
    drawf(20, my- 30, "cgndot= %13.3f cgwdot=%13.3f cghdot=%13.3f", cgndot,cgwdot,cghdot)
    drawf(20, my- 45, "tdot=   %13.3f wdot=  %13.3f ldot=  %13.3f", tdot,  wdot,  ldot)
    drawf(20, my- 60, "ctn= %7.3f ctw= %7.3f cth= %7.3f", ctn,   ctw,   cth)
    drawf(20, my- 75, "cwn= %7.3f cww= %7.3f cwh= %7.3f", cwn,   cww,   cwh)
    drawf(20, my- 90, "cln= %7.3f clw= %7.3f clh= %7.3f", cln,   clw,   clh)
    drawf(20, my-105, "ft=     %13.3f fw=    %13.3f fl=    %13.3f", ft,    fw,    fl)
    drawf(20, my-120, "rft=    %13.3f rfw=   %13.3f rfl=   %13.3f", rft,   rfw,   rfl)
    drawf(20, my-135, "rtdot=  %13.3f rwdot= %13.3f rldot= %13.3f", rtdot, rwdot, rldot)
    drawf(20, my-150, "steprate=%8.3f", steprate)

    drawf(20, 130, "tdot=%13.3f  ldot=%13.3f => angle=%6.1f airspeed=%13.3f",
                    tdot, ldot, atl, radius2(tdot, ldot))
    drawf(20, 115, "atl=%6.1f rdtab(atl,lifttab)=%9.3f", atl, rdtab(atl,lifttab))

  }

  IF plotusage DO
  { drawf(20, 20, "CPU usage = %3i%%", usage)
  }

  { LET heading = - FIX (angle(ctn,ctw))
    IF heading < 0 DO heading := 360 + heading
    drawf(20, 5, "      RPM %i4  Speed %3i mph  Altiude %i5 ft  Heading %i3",
          FIX rpm, FIX (tdot/mph2fps), FIX cgh, heading)
  }
//updatescreen()
}

AND plotscreen() BE
{ LET mx = screenxsize /  2
  LET my = screenysize - 70

  fillsurf(col_lightblue)

  setcolour(col_lightcyan)
  setcolour(col_red)
  //abort(1999)
  drawstr(240, 50, done -> "Quitting", "Tiger Moth Flight Simulator")
  updatescreen()
//delay(1000)

  drawcontrols()

  setcolour(col_gray)
  moveto(mx, my)
  drawby(0, FIX(cgh/100))

  setcolour(col_majenta)
  moveto(mx+200, my)
  drawby(FIX(ctn * 20.0), FIX(ctw * 20.0))

  //draw_artificial_horizon()

  //drawgroundpoints()

  IF eyedir DO plotcraft()
  updatescreen()
//abort(1090)

}

AND seteyeposition1() BE
{ cetn, cetw, ceth :=  One, Zro, Zro
  cewn, ceww, cewh :=  Zro, One, Zro
  celn, celw, celh :=  Zro, Zro, One
  eyen, eyew, eyeh :=  -eyedist,   Zro, Zro   // Relative eye position
}

AND seteyeposition() BE
{ LET FLT d1 = eyedist
  LET FLT d2 = d1 * 0.707
  LET FLT d3 = d2 / 3

  cetn, cetw, ceth :=  One, Zro, Zro
  cewn, ceww, cewh :=  Zro, One, Zro
  celn, celw, celh :=  Zro, Zro, One
  eyen, eyew, eyeh :=  -eyedist,   Zro, Zro   // Relative eye position


UNLESS 0<=eyedir<=8 DO eyedir := 1

  IF hatdir & sdlmsecs()>hatmsecs+100 DO
  { eyedir := FIX((angle(ctn, ctw)+360.0+22.5) / 45.0) & 7
    // dir = 0  heading N
    // dir = 1  heading NE
    // dir = 2  heading E
    // dir = 3  heading SE
    // dir = 4  heading S
    // dir = 5  heading SW
    // dir = 6  heading W
    // dir = 7  heading NW
    SWITCHON hatdir INTO
    { DEFAULT:
      CASE #b0001:                     ENDCASE // Forward
      CASE #b0011: eyedir := eyedir+1; ENDCASE // Forward right
      CASE #b0010: eyedir := eyedir+2; ENDCASE // Right
      CASE #b0110: eyedir := eyedir+3; ENDCASE // Backward right
      CASE #b0100: eyedir := eyedir+4; ENDCASE // Backward
      CASE #b1100: eyedir := eyedir+5; ENDCASE // Backward left
      CASE #b1000: eyedir := eyedir+6; ENDCASE // Left
      CASE #b1001: eyedir := eyedir+7; ENDCASE // Forward left
    }
    eyedir := (eyedir & 7) + 1
    hatdir := 0

//writef("ctn=%9.3f ctw=%9.3f eyedir=%9.1f*n", ctn, ctw, eyedir)
//abort(1009) 
  }

  SWITCHON eyedir INTO
  { DEFAULT:

    CASE 0: // Pilot's view
      cetn, cetw, ceth := ctn, ctw, cth
      cewn, ceww, cewh := cwn, cww, cwh
      celn, celw, celh := cln, clw, clh

      eyen, eyew, eyeh := Zro, Zro, Zro   // Relative eye position
      RETURN

     CASE 1: // North
       cetn, cetw, ceth :=  One, Zro, Zro
       cewn, ceww, cewh :=  Zro, One, Zro
       celn, celw, celh :=  Zro, Zro, One
       eyen, eyew, eyeh :=  -d1, Zro,  d3   // Relative eye position
       RETURN

     CASE 2: // North east
       cetn, cetw, ceth :=  D45, D45, Zro
       cewn, ceww, cewh := -D45, D45, Zro
       celn, celw, celh :=  Zro, Zro, One
       eyen, eyew, eyeh :=  -d2, -d2,  d3   // Relative eye position
       RETURN

     CASE 3: // East
       cetn, cetw, ceth :=  Zro, One, Zro
       cewn, ceww, cewh := -One, Zro, Zro
       celn, celw, celh :=  Zro, Zro, One
       eyen, eyew, eyeh :=  Zro, -d1,  d3   // Relative eye position
       RETURN

     CASE 4: // South east
       cetn, cetw, ceth := -D45, D45, Zro
       cewn, ceww, cewh := -D45,-D45, Zro
       celn, celw, celh :=  Zro, Zro, One
       eyen, eyew, eyeh :=   d2, -d2,  d3   // Relative eye position
       RETURN

     CASE 5: // South
       cetn, cetw, ceth := -One,  Zro, Zro
       cewn, ceww, cewh :=  Zro, -One, Zro
       celn, celw, celh :=  Zro,  Zro, One
       eyen, eyew, eyeh :=   d1,  Zro,  d3   // Relative eye position
       RETURN

     CASE 6: // South west
       cetn, cetw, ceth :=-D45,-D45, Zro
       cewn, ceww, cewh := D45,-D45, Zro
       celn, celw, celh := Zro, Zro, One
       eyen, eyew, eyeh :=  d2,  d2,  d3   // Relative eye position
       RETURN

     CASE 7: // West
       cetn, cetw, ceth := Zro,-One, Zro
       cewn, ceww, cewh := One, Zro, Zro
       celn, celw, celh := Zro, Zro, One
       eyen, eyew, eyeh := Zro,  d1,  d3   // Relative eye position

       RETURN

     CASE 8: // North west
       cetn, cetw, ceth := D45,-D45, Zro
       cewn, ceww, cewh := D45, D45, Zro
       celn, celw, celh := Zro, Zro, One
       eyen, eyew, eyeh := -d2,  d2,  d3   // Relative eye position
       RETURN
  }
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ DEFAULT:
    //writef("Unknown event type = %n*n", eventtype)
    LOOP

  CASE sdle_keydown:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:                                     LOOP

      CASE 'A':  TEST eventa2='a'
                 THEN rotate( 0.1, 0.0, 0.0)
                 ELSE rotate(-0.1, 0.0, 0.0)
                 plotscreen()
                 LOOP

      CASE 'B':  TEST eventa2='b'
                 THEN rotate( 0.0,  0.1, 0.0)
                 ELSE rotate( 0.0, -0.1, 0.0)
                 plotscreen()
                 LOOP

      CASE 'C':  TEST eventa2='c'
                 THEN rotate( 0.0, 0.0,  0.1)
                 ELSE rotate( 0.0, 0.0, -0.1)
                 plotscreen()
                 LOOP

      CASE 'Q':  done := TRUE;                     LOOP

      CASE 'D':  debugging := ~debugging;          LOOP

      CASE 'P':  stepping := ~stepping;            LOOP

      CASE 'U':  plotusage := ~plotusage;          LOOP

      CASE 'S':  enginestarted := ~enginestarted;  LOOP

      CASE 'G': // Position aircraft on the glide path
                initposition(2)
                LOOP

      CASE 'L': // Set level flight at 3000 ft and speed 65 mph.
                initposition(3)
                LOOP

      CASE 'T': // Position the aircraft ready for take off
                initposition(1)
                LOOP

      CASE 'N': // Reduce eye distance
                eyedist := eyedist*5/6
                IF eyedist<60.0 DO eyedist := 60.0
                LOOP

      CASE 'F': // Increase eye distance
                eyedist := eyedist*6/5
                IF eyedist > 1000.0 DO eyedist := 1000.0
                LOOP

      CASE 'Z': c_trimthrottle := c_trimthrottle - 0.05
                throttle := c_trimthrottle+c_throttle
                IF throttle < 0.0 DO throttle, c_trimthrottle := 0.0, -c_throttle
                LOOP

      CASE 'X': c_trimthrottle := c_trimthrottle + 0.050
                throttle := c_trimthrottle+c_throttle
                IF throttle > 1.0 DO throttle, c_trimthrottle := 1.0, 1.0-c_throttle
                LOOP

      CASE ',':
      CASE '<': c_trimrudder := c_trimrudder - 0.050
                rudder := c_trimrudder+c_rudder
                IF rudder < -1.0 DO rudder, c_trimrudder := -1.0, -1.0-c_rudder
                LOOP

      CASE '.':
      CASE '>': c_trimrudder := c_trimrudder + 0.050
                rudder := c_trimrudder+c_rudder
                IF rudder > 1.0 DO rudder, c_trimrudder := 1.0, 1.0-c_rudder
                LOOP

      CASE '0': eyedir, hatdir := 0, 0;        LOOP // Pilot's view
      CASE '1': hatdir, hatmsecs := #b0001, 0; LOOP // From behind
      CASE '2': hatdir, hatmsecs := #b0011, 0; LOOP // From behind right
      CASE '3': hatdir, hatmsecs := #b0010, 0; LOOP // From right
      CASE '4': hatdir, hatmsecs := #b0110, 0; LOOP // From in front right
      CASE '5': hatdir, hatmsecs := #b0100, 0; LOOP // From in front
      CASE '6': hatdir, hatmsecs := #b1100, 0; LOOP // From in front left
      CASE '7': hatdir, hatmsecs := #b1000, 0; LOOP // From left
      CASE '8': hatdir, hatmsecs := #b1001, 0; LOOP // From behind left

      CASE sdle_arrowup:
                c_trimelevator := c_trimelevator + 0.050
                elevator := c_trimelevator+c_elevator
                IF elevator > 1.0 DO elevator, c_trimelevator := 1.0, 1.0-c_elevator
                LOOP

      CASE sdle_arrowdown:
                c_trimelevator := c_trimelevator - 0.050
                elevator := c_trimelevator+c_elevator
                IF elevator < -1.0 DO elevator, c_trimelevator := -1.0, -1.0-c_elevator
                LOOP

      CASE sdle_arrowright:
                c_trimaileron := c_trimaileron + 0.050
                aileron := c_trimaileron+c_aileron
                IF aileron > 1.0 DO aileron, c_trimaileron := 1.0, 1.0-c_aileron
                LOOP

      CASE sdle_arrowleft:
                c_trimaileron := c_trimaileron - 0.050
                aileron := c_trimaileron+c_aileron
                IF aileron < -1.0 DO aileron, c_trimaileron := -1.0, -1.0-c_aileron
                LOOP
    }
    LOOP

  CASE sdle_joyaxismotion:    // 7
  { // This currently assumes that the joystick
    // is a CyborgX.
    LET which = eventa1
    LET axis  = eventa2
    LET FLT value = (FLOAT eventa3) / 32768.0
//writef("axismotion: which=%n axis=%n value=%8.6f*n", which, axis, value)
    SWITCHON axis INTO
    { DEFAULT:  LOOP
      CASE 0:   c_aileron  :=  value;           // Aileron
                aileron := c_trimaileron+c_aileron
                IF aileron < -1.0 DO aileron, c_trimaileron := -1.0, -1.0-c_aileron
                IF aileron >  1.0 DO aileron, c_trimaileron :=  1.0,  1.0-c_aileron
                LOOP
      CASE 1:   c_elevator := -value;           // Elevator
                elevator := c_trimelevator+c_elevator
                IF elevator < -1.0 DO elevator, c_trimelevator := -1.0, -1.0-c_elevator
                IF elevator >  1.0 DO elevator, c_trimelevator :=  1.0,  1.0-c_elevator
                LOOP
      CASE 2:   c_throttle := (1.0-value)/2.0;  // Throttle
                throttle := c_trimthrottle+c_throttle
                IF throttle <  0.0 DO throttle, c_trimthrottle :=  0.0,    -c_throttle
                IF throttle >  1.0 DO throttle, c_trimthrottle :=  1.0, 1.0-c_throttle
                LOOP
      CASE 3:   c_rudder   :=  value;           // Rudder
                rudder := c_trimrudder+c_rudder
                IF rudder < -1.0 DO rudder, c_trimrudder := -1.0, -1.0-c_rudder
                IF rudder >  1.0 DO rudder, c_trimrudder :=  1.0,  1.0-c_rudder
                LOOP
      CASE 4:   LOOP                            // Right throttle
    }
  }

  CASE sdle_joyhatmotion:
  { LET which = eventa1
    LET axis  = eventa2
    LET value = eventa3

    //writef("joyhatmotion %n %n %n*n", eventa1, eventa2, eventa3)

    SWITCHON value INTO
    { DEFAULT:   
      CASE #b0000: // None                                        LOOP
      CASE #b0001: // North
      CASE #b0011: // North east
      CASE #b0010: // East
      CASE #b0110: // South east
      CASE #b0100: // South
      CASE #b1100: // South west
      CASE #b1000: // West
      CASE #b1001: // North west
             IF value>hatdir DO
             { hatdir, hatmsecs := value, sdlmsecs()
//writef("hatdir=%b4  %n msecs*n", hatdir, hatmsecs)
             }
             LOOP
    }
  }

  CASE sdle_joybuttondown:    // 10
    //writef("joybuttondown %n %n %n*n", eventa1, eventa2, eventa3)
    SWITCHON eventa2 INTO
    { DEFAULT:   LOOP
      CASE  7:     // Left rudder trim
                c_trimrudder := c_trimrudder - 0.050
                rudder := c_trimrudder+c_rudder
                IF rudder < -1.0 DO rudder, c_trimrudder := -1.0, -1.0-c_rudder
                LOOP

      CASE  8:     // Right rudder trim
                c_trimrudder := c_trimrudder + 0.050
                rudder := c_trimrudder+c_rudder
                IF rudder >  1.0 DO rudder, c_trimrudder :=  1.0, 1.0-c_rudder
                LOOP

      CASE 11:     // Reduce eye distance
              eyedist := eyedist*5/6
              IF eyedist < 60.0 DO eyedist := 60.0
//writef("eyedist=%9.3f*n", eyedist)
              LOOP
      CASE 12:     // Increase eye distance
              eyedist := eyedist*6/5
              IF eyedist > 1000.0 DO eyedist := 1000.0
//writef("eyedist=%9.3f*n", eyedist)
              LOOP
      CASE 13:     // Set pilot view
              eyedir, hatdir := 0, 0;              LOOP
    }
    LOOP

  CASE sdle_joybuttonup:      // 11
    //writef("joybuttonup*n", eventa1, eventa2, eventa3)
    LOOP

  CASE sdle_quit:             // 12
    writef("QUIT*n");
    LOOP

  CASE sdle_videoresize:      // 14
    //writef("videoresize*n", eventa1, eventa2, eventa3)
    LOOP
}

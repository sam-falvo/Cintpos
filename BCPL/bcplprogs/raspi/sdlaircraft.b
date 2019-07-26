/*
This is a simple demonstration of drawing in 3D using
the SDL graphics library.

Implemented by Martin Richards (c) January 2012

History

31/05/2018
Significant reimplementation of the program.

12/03/2018
Extensively modified to use floating point and the new FLT feature.


Controls

Either use a USB Joystick for elevator, ailerons and throttle, or
use the keyboard as follows:

Up arrow      Trim joystick forward a bit
Down arrow    Trim joystick backward a bit
Left arrow    Trim joystick left a bit
Right arrow   Trim joystick right a bit

, or <        Trim rudder left
. or >        Trim rudder right

p             pause/unpause the simulation
d             Toggle debugging
g             Toggle gear down
s             Toggle start engine
x/z           More/less throttle

0             Display the pilot's view
1,2,3,4,5,6,7,8 Choose various eye directions
U/u           Raise/lower the eye level relative to the aircraft

f             View aircraft from a greater distance
n             View aircraft from a closer position
l             Reset the aircraft to level flight at 10000 ft.
g             Reset the aircraft on the glide path
t             Reset the aircraft ready for take off -- default
              ie stationary on the ground near one end of the runway
q             Quit

There are joystick buttons equivalent to Up arrow, Down arrow, Left
Arrow and Right arrow. There are also joystick buttons to trim the
rudder left and right, useful for steering on the runway.

The display shows various beacons on the ground including the lights
on the sides and the ends of the runway.

The display also shows various flight instruments including the
artificial horizon, the height and speed and various navigational aids
to help the pilot find the runway.
*/

GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the SDL BCPL library
.
GET "libhdr"
GET "sdl.h"

MANIFEST {
  FLT D45 = 0.70710678  // cosine of pi/4 = sqrt(2)

  // Most measurements are in feet, held as floating point numbers.

  FLT k_g = 32.0      // Acceleration due to gravity, 32 ft per sec per sec

  FLT mass = 2000.0   // Mass of the aircraft including the pilot and fuel.
                      // This should perhaps be a variable.

  FLT massg = mass * k_g // The vertical force of gravity on the aircraft
                         // in poundals.

  // A rotational force of 1 ft-poundal will cause a body with
  // unit moment of inertia to rotate at a rate of one radian per second
  // after one second. Unit moment of inertia corresponds to one pound
  // positioned one foot from the axis of rotation.
  // Note that a force of 1 poundal will accelerate a mass of one pound
  // to a speed of one foot per second in one second.
  FLT mntt = 4.0*mass // Moment of inertia about t
  FLT mntw = 6.0*mass // Moment of inertia about w
  FLT mntl = 8.0*mass // Moment of inertia about l

  // Conversion factors
  FLT fps2mph   = 60*60/5280.0
  FLT mph2fps   = 1.0/fps2mph

  FLT Left  = +1.0  // Used when drawing wings etc.
  FLT Right = -1.0
}

GLOBAL {
  done:ug

  FLT One      // Set to 1.0      Loading globals are cheaper than
  FLT Zro      // Set to 0.0      loading 32-bit constants.

  stepping     // =FALSE if not stepping the simulation
  stepcount
  msecs1       // Used in the calculation of steprate
  FLT steprate

  crashed        // =TRUE if crashed
  debugging      // Toggled by the D command.
  geardown       // Toggled by the G command.
  enginestarted  // Toggled by S.
  
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

  FLT c_throttle; FLT c_trimthrottle; FLT throttle // 0.0 to +1.0 zero to full power
  FLT c_aileron;  FLT c_trimaileron;  FLT aileron  //-1.0 to +1.0 stick left to right
  FLT c_elevator; FLT c_trimelevator; FLT elevator //-1.0 to +1.0 stick back to forward
  FLT c_rudder;   FLT c_trimrudder;   FLT rudder   //-1.0 to +1.0 full left to right

  FLT rpm; FLT targetrpm
  // The target rpm depends on the throttle setting and the air speed.
  // The engine rpm take time to reach this speed.
  FLT thrust   // Depends on rpm and the air speed.

  FLT coselevator; FLT sinelevator
  FLT cosaileron;  FLT sinaileron
  FLT cosrudder;   FLT sinrudder

  FLT pilotl // The l component of the pilot's eye, typically 6.0 ft.

  // Below t is the direction of the aircraft
  FLT ctn; FLT ctw; FLT cth    // Direction cosines of direction t (forward)
  FLT cwn; FLT cww; FLT cwh    // Direction cosines of direction w (left)
  FLT cln; FLT clw; FLT clh    // Direction cosines of direction l (lift)

  // Below t is the direction of the eye
  FLT cetn; FLT cetw; FLT ceth // Eye direction cosines of direction t (forward)
  FLT cewn; FLT ceww; FLT cewh // Eye direction cosines of direction w (left)
  FLT celn; FLT celw; FLT celh // Eye direction cosines of direction l (lift)

  FLT eyedist                  // Eye distance from aircraft
  FLT eyeheight                // Eye height relative to cgh

  // The following is the matrix used by screencoords when
  // transforming either aircraft or ground points to
  // screen coordinates.
  FLT m00; FLT m01; FLT m02    // First row
  FLT m10; FLT m11; FLT m12    // Second row
  FLT m20; FLT m21; FLT m22    // Third row

  setaircraftmat    // Use to set the above matrix.
  setgroundmat

  // The following take vertices of the aircraft model.
  cdrawquad3d       // (x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4)
  cdrawtriangle3d   // (x1,y1,z1, x2,y2,z2, x3,y3,z3)
                    // These use twl coordinates relative to the
                    // aircraft's origin.

  // The following take vertices of points on the ground.
  gdrawquad3d       // (x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4)
  gdrawtriangle3d   // (x1,y1,z1, x2,y2,z2, x3,y3,z3)
                    // These use nwh world coordinates to draw
                    // the runway and other points on the ground.

                    // All these coordinates are floating point values.

  FLT cgn; FLT cgw; FLT cgh          // Aircraft position in world coordinates
  FLT cgndot; FLT cgwdot; FLT cghdot // Aircraft velocity in world coordinates

  // Speed in various directions is measured in ft/s.
  FLT tdot; FLT wdot; FLT ldot // Speed in the aircraft t, w and l directions
  FLT   ft; FLT   fw; FLT   fl // Linear forces on the aircraft
  FLT  rft; FLT  rfw; FLT  rfl // Rotational forces on the aircraft

  FLT usage                    // 0 to 100 percentage cpu usage

  hatdir           // Hat direction given by the joystick.
  hatmsecs         // msecs of last hat change.
  eyedir           // An integer specifying the eye direction
                   // 0 = cockpit view
                   // 1,...,8 looking N,NE,E,SE,S,SW,W and NW.
}

LET rotate(FLT rt, FLT rw, FLT rl) BE
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

  adjustorientation()
}

AND adjustorientation() BE
{ // Make minor corrections to ensure that the axes are orthogonal and
  // of unit length.
  adjustlength(@ctn);      adjustlength(@cwn);      adjustlength(@cln) 
  adjustortho(@ctn, @cwn); adjustortho(@ctn, @cln); adjustortho(@cwn, @cln)
}

AND adjustlength(v) BE
{ // This helps to keep vector v of unit length
  LET FLT x, FLT y, FLT z = v!0, v!1, v!2
  LET FLT r = sys(Sys_flt, fl_radius3, x, y, z)
  v!0 := x / r
  v!1 := y / r
  v!2 := z / r
}

AND adjustortho(a, b) BE
{ // This helps to keep the unit vector b orthogonal to a
  LET FLT a0, FLT a1, FLT a2 = a!0, a!1, a!2
  LET FLT b0, FLT b1, FLT b2 = b!0, b!1, b!2
  LET FLT corr = a0*b0 + a1*b1 + a2*b2 // cos of angle between a and b.
  b!0 := b0 - a0 * corr
  b!1 := b1 - a1 * corr
  b!2 := b2 - a2 * corr
}

AND angle(FLT x, FLT y) = x=0 & y=0 -> 0.0, VALOF
{ // Calculate the angle in degrees between
  // point (x,y) and the x axis using atan2.
  // If (x,y) is above the x-axis the result is betweem 0 and +180
  // If (x,y) is below the x-axis the result is betweem 0 and -180
  LET FLT a = sys(Sys_flt, fl_atan2, y, x) * 180.0 / 3.14159
//plotf(20,  30, "angle: x=%13.3f y=%13.3f => angle = %8.3f*n",
//        x, y, a)
  RESULTIS a
}

AND applygforce(FLT t, FLT w, FLT l, FLT gn, FLT gw, FLT gh) BE
{ // (t,w,l) are the coordinates of a point on the aircraft
  // and (gn,gw,gh) is the force vector in world coordinates.
  // Typically (gn,gw,gh) is a force on a wheel touching the ground.

  // Transform (gn,gw,gh) to aircraft coordinates.
  LET FLT pt = gn*ctn + gw*ctw + gh*cth
  LET FLT pw = gn*cwn + gw*cww + gh*cwh
  LET FLT pl = gn*cln + gw*clw + gh*clh
//newline()
//writef("applygforce:   t=%10.3f   w=%10.3f   l=%10.3f*n",   t,w,l)
//writef("applygforce:  gn=%10.3f  gw=%10.3f  gh=%10.3f*n*n", gn,gw,gh)

//writef("applygforce: ctn=%10.3f ctw=%10.3f cth=%10.3f*n",   ctn,ctw,cth)
//writef("applygforce: cwn=%10.3f cww=%10.3f cwh=%10.3f*n",   cwn,cww,cwh)
//writef("applygforce: cln=%10.3f clw=%10.3f clh=%10.3f*n*n", cln,clw,clh)

//writef("applygforce:  pt=%10.3f  pw=%10.3f  pl=%10.3f*n*n", pt,pw,pl)
  // Apply this force at point (t,w,l) of the aircraft.
  applyforce(t, w, l, pt, pw, pl)
}

AND applyforce(FLT t, FLT w, FLT l, FLT pt, FLT pw, FLT pl) BE
{ // (t,w,l) are the coordinates of a point on the aircraft
  // and (pt,pw,pl) is the force vector in aircraft coordinates.
  // The linear forces (ft,fw,fl) are measured in poundals
  // and their effect depends on the mass of the aircraft.
  // The rotational forces (rft,rfw,rfl) are measured in ft-poundals
  // and their effect depends on the momements of inertia of the
  // aircraft about the aircraft's three axes.
//writef("Current forces:*n")
//writef("applyforce: ft=%10.3f rft=%10.3f*n",   ft, rft)
//writef("applyforce: fw=%10.3f rfw=%10.3f*n",   fw, rfw)
//writef("applyforce: fl=%10.3f rfl=%10.3f*n*n", fl, rfl)

  ft, rft := ft + pt, rft + 0.5*pw*l - 0.5*pl*w
  fw, rfw := fw + pw, rfw + 0.5*pl*t - 0.5*pt*l
  fl, rfl := fl + pl, rfl + 0.5*pt*w - 0.5*pw*t

//writef("applyforce: t=%6.3f w=%6.3f l=%6.3f*n", t,w,l)

//writef("applyforce: changes pt=%10.3f rft=%10.3f*n", pt, 0.5*pw*l - 0.5*pl*w)
//writef("applyforce: changes pw=%10.3f rfw=%10.3f*n", pw, 0.5*pl*t - 0.5*pt*l)
//writef("applyforce: changes pl=%10.3f rfl=%10.3f*n", pl, 0.5*pt*w - 0.5*pw*t)

//writef("*nResulting forces:*n")
//writef("applyforce: ft=%10.3f rft=%10.3f*n", ft, rft)
//writef("applyforce: fw=%10.3f rfw=%10.3f*n", fw, rfw)
//writef("applyforce: fl=%10.3f rfl=%10.3f*n", fl, rfl)
//abort(2211)
}

LET step() BE
{ LET FLT speed = 0.0

//writef("Incrementing stepcount to %n*n", stepcount)
  stepcount := stepcount + 1

  // The linear forces on the CG of the aircraft in directions
  // t, w and l initialised as follows.
  ft, fw, fl  := Zro, Zro, Zro

  // These are in poundals.
  // 1 poundal will accelerate a mass of 1 lb at a rate
  // of 1 ft/s/s. A force of mass x g will just hold the aircraft 
  // up against gravity.

  // The rotational forces on the CG of the aircraft about axes
  // t, w and l initialised as follows.
  rft, rfw, rfl  := Zro, Zro, Zro

  // These are in ft-poundals.
  // 1 ft-poundal will cause a rotational acceleration of 1 radian/s/s of
  // an object with moment of inertia 1 ft-pound.

writef("Velocity:      t'=%13.3f w'=%13.3f l'=%13.3f*n", tdot, wdot, ldot)

  // First calculate the engine RPM.
  targetrpm := 0.0    // If engine is not started
  IF enginestarted DO
  { // The throttle is in the range 0.0 to 1.0
    targetrpm := 600.0 + 1700.0 * throttle + // 600 to 2300 when tdot=0
                 0.7 * tdot        // + air speed effect
  }

  // The rpm take time to change.
  rpm := rpm + (targetrpm - rpm) / (4.0*steprate) - 1.0
  //rpm := targetrpm // For debugging always set rpm to targetrpm
  IF rpm < 0.0 DO rpm := 0.0

  // Now calculate the thrust.
  thrust := VALOF
  { // At rpm=2200 the aircraft should reach a take off speed of 65mph after
    // travelling about 1700 ft along the runway.
    // Assume the angle of attack of the propeller is such that
    // when rpm=2500 the speed at which the thrust is zero is about 200mph
    // or 200*5280 / (60*60) = about 293.0 ft/s.

    // At a speed of tdot ft/s, the rpm giving zero thrust is
    // (2500/293) * tdot. Ie linear between 0 and 293.
    // At a speed of 65mph = 95ft/s the rpm of zero thrust is thus
    // (2500/293)*95 = 810 (rpm0)
    // Assume that the thrust at this speed is Kt * (rpm-rpm0)^2
    // and that the drag is Kd * tdot^2 = Kd * 95^2 = 9025*Kd
    // Assume that at this speed with rpm=2000 the thrust-drag
    // is sufficient to give the aircraft an acceleration of g/8.
    // Thus mass*g/8 = Kt * (2000-810)^2 - 9025 * Kd
    // Assume mass=2000 and g=32, this gives
    // 2000*32/8 = Kt * (2000-810)^2 - 9025 * Kd
    // or 8000 = Kt * 134300 = 9025 * Kd
    LET FLT vmax = 293.0 * rpm / 2500 // Speed in ft/s at which thrust=0
    LET FLT vmaxby2 = vmax/2          // use linear interpolation between
                                      // vmax and vmaxby2
    LET FLT tmax = 1.7 * massg        // Thrust to give acceleration of
                                      // 1.7 * g ignoring drag.
    LET FLT t = ?
    // IF rpm is too small or tdot is too great there is no thust.
    IF rpm<600 | tdot>vmax RESULTIS 0.0
    // When rpm>=600 the thrust is proportional to the square of (rpm-600)
    // When rpm=2200 the thrust is sufficient to accelerate the aircraft at g/8
    t := tmax * (rpm-600)*(rpm-600) / ((2200-600) * (2200-600))
    IF tdot<vmaxby2 RESULTIS t
    RESULTIS t * (vmax-tdot) / vmaxby2
  }

  // Gravity effect
  ft := ft - massg * cth  // Gravity in direction t
  fw := fw - massg * cwh  // Gravity in direction w
  fl := fl - massg * clh  // Gravity in direction l

writef("Gravity force: ft=%13.3f fw=%13.3f fl=%13.3f*n", ft, fw, fl)
writef("Gravity accel:  t=%13.3f  w=%13.3f  l=%13.3f*n", ft/mass, fw/mass, fl/mass)
  // Lift effect
  { LET FLT lift = 0.0

    // Lift is proportional to speed above 30 ft/s and
    // at 95 ft/s is just sufficient to hold the aircraft
    // up against gravity.

    IF tdot>30.0 DO lift := massg * (tdot-30.0) / (95.0-30.0)

    // Lift and elevator effect
    fl := fl + lift - massg * 2.50 * elevator * tdot/95.0
writef("Lift force: %13.3f => fl=%13.3f*n",
        lift - massg * 2.50 * elevator * tdot/95.0, fl)
writef("Lift accel: %13.3f => fl=%13.3f*n", 
       (lift - massg * 2.50 * elevator * tdot/95.0)/mass, fl/mass)
  }
      
  // Drag effect proportional to tdot squared.
  // throttle at 0.5 gives rpm=1466 and level speed of 62 mph
  // throttle at 1.0 gives rpm=2371 and level speed of 116 mph
  ft := ft -
        massg * 0.50 * tdot * tdot / (95.0 * 95.0)

writef("Drag force: %13.3f => ft=%13.3f*n",
        massg * 0.50 * tdot * tdot / (95.0 * 95.0), ft)
writef("Drag accel: %13.3f => ft=%13.3f*n",
       (massg * 0.50 * tdot * tdot / (95.0 * 95.0))/mass, ft/mass)

  // Thrust effect
  ft := ft + thrust

writef("Thrust force: %13.3f ft=>%13.3f*n", thrust, ft)
writef("Thrust accel: %13.3f ft=>%13.3f*n", thrust/mass, ft/mass)

  // Rudder effect.
  applyforce(-12.0, 0.0, 0.0,
               0.0,
               0.0,
               tdot * 1.5 * rudder - // Rotational effect of the rudder.
               wdot * 3.0)           // Effect of side slip.

  // Elevator effect.
  applyforce(-12.0, 0.0, 0.0,
               0.0,
               tdot * 1.5 * elevator - // Rotational effect of the elevator.
               ldot * 3.0,             // Effect of angle of attack.
               0.0)

writef("Linear accel:  ft=>%13.3f  fw=>%13.3f  fl=>%13.3f*n",  ft/mass,  fw/mass,  fl/mass)
writef("Rotate accel: wft=>%13.3f rfw=>%13.3f rfl=>%13.3f*n", rft/mntt, rfw/mntw, rfl/mntl)

  //fl := -500.0

  IF cgh < 20.0 DO
  { // The aircraft is may be in contact with the ground.
    // Lowest point of the wheel is -0.9 - 1.0 - 0.285 = -2.185
    // In due cause change the l value of the front wheel so that,
    // when the aircraft is stationary on the ground, it is level,
    // ie cth is zero and rfw is zero.
    wheeleffect( 4.0,  0.0,  -2.185) // Front wheel effect
    wheeleffect(-0.7,  3.0,  -2.185) // Left wheel effect
    wheeleffect(-0.7, -3.0,  -2.185) // Right wheel effect

    IF cghdot < -1.0 DO cghdot := -1.0
    IF cghdot >  1.0 DO cghdot :=  1.0
  }

  // Aileron effect -- cause a rotational force about axis t.
  // The force is proportional to tdot.
  rft := rft - 1.8 * aileron * tdot / 100.0
writef("Aileron  rotational force: %13.3f => rft=%13.3f*n", - 1.8 * aileron * tdot / 100.0, rft)

  // Deal with dihedral effect.
  rft := rft - 0.08 * fw // In ft-poundals
writef("Dihedral rotational force: %13.3f => rft=%13.3f*n", - 0.08 * fw, rft)

// Limit the lift force
IF fl > 3*massg DO { writef("step: limiting lift force*n")
                     fl := 3*massg
                   }

writef("step:  ft=%10.3f  fw=%10.3f  fl=%10.3f*n",  ft,  fw,  fl)
writef("step: rft=%10.3f rfw=%10.3f rfl=%10.3f*n", rft, rfw, rfl)
//abort(1789)

  // The rotational forces are now known. The angle of rotation
  // depends on the rotational force, the moment of insertia and
  // the step rate.
  rotate(rft/(mntt*steprate),
         rfw/(mntw*steprate),
         rfl/(mntl*steprate))

  // Hopefully the rotation angles are quite small, so we can use the
  // old tdot, wdot and ldot.
  tdot := tdot + 1.00 * ft / (mass * steprate)
  wdot := wdot + 1.00 * fw / (mass * steprate)
  ldot := ldot + 1.00 * fl / (mass * steprate)

  tdot := tdot + 1.00 * ft / (mass * steprate)
  wdot := wdot + 1.00 * fw / (mass * steprate)
  ldot := ldot + 1.00 * fl / (mass * steprate)

  IF tdot > 0.01 DO tdot := tdot - 0.01/steprate
  IF wdot > 0.01 DO wdot := wdot - 0.01/steprate
  IF ldot > 0.01 DO ldot := ldot - 0.01/steprate

  IF tdot < -0.01 DO tdot := tdot + 0.01/steprate
  IF wdot < -0.01 DO wdot := wdot + 0.01/steprate
  IF ldot < -0.01 DO ldot := ldot + 0.01/steprate

  IF ABS tdot < 0.05 DO tdot := 0.0
  IF ABS wdot < 0.05 DO wdot := 0.0
  IF ABS ldot < 0.05 DO ldot := 0.0

  // Calculate the real world velocity
  cgndot := tdot*ctn + wdot*cwn + ldot*cln
  cgwdot := tdot*ctw + wdot*cww + ldot*clw
  cghdot := tdot*cth + wdot*cwh + ldot*clh

  IF cgh < 10.0 DO
  { IF cghdot < -1.0 DO cghdot := -1.0
    IF cghdot >  1.0 DO cghdot :=  1.0
  }

  IF ABS cgndot < 0.05 DO cgndot := 0.0 
  IF ABS cgwdot < 0.05 DO cgwdot := 0.0 
  IF ABS cghdot < 0.05 DO cghdot := 0.0
 
  // Calculate new n, w and h positions.
  cgn := cgn + cgndot / steprate
  cgw := cgw + cgwdot / steprate
  cgh := cgh + cghdot / steprate

writef("step: stepcount=%n steprate=%9.3f cgh=%10.3f cghdot=%10.3f*n",
       stepcount, steprate, cgh, cghdot)

  IF cgh<2.0 DO
  { // The aircraft is on the ground reduce cghdot
    cghdot := cghdot - 0.1*cghdot/steprate
    // Recalculate tdot
    tdot := cgndot*ctn + cgwdot*ctw + cghdot*cth
    // The aircraft will rotate causing wdot and ldot to equal zero.
  }

  IF sys(Sys_flt, fl_radius3, cgndot, cgwdot, cghdot) > 90.0 DO
  { // Make the aircraft point in the direction of the velocity
    // vector unless the speed is less than 5 ft/s.

    // OV is the velocity vector
    // x, y and z are it coordinates in directions t, w and l.
    // r3 is the length of OV
    // r2 is the length of OP where P lies in the plane tw
    //                        and VP is orthogonal to this plane.
    // phi is the angle between OP and the t axis.
    // theta is the angle between OP and the plane tw.
    // To avoid overflow these angles have special values
    // if r2 and/or r3 are too small.

    LET FLT x = cgndot*ctn + cgwdot*ctw + cghdot*cth  // = v.t
    LET FLT y = cgndot*cwn + cgwdot*cww + cghdot*cwh  // = v.w
    LET FLT z = cgndot*cln + cgwdot*clw + cghdot*clh  // = v.l
    LET FLT r2 = sys(Sys_flt, fl_radius2, x, y)
    LET FLT r3 = sys(Sys_flt, fl_radius3, x, y, z)
    // If the speed is not sufficient the rotation angles are given
    // special values.
    LET FLT cphi   = r3<1.0 | r2<0.001 -> 1.0, // No rotation about l
                     x  / r2        // phi is the rotation angle about l
    LET FLT sphi   = r3<1.0 | r2<0.001 -> 0.0, // No rotation about l
                     y  / r2        // phi is the rotation angle about l

    LET FLT ctheta = r3<1.0 -> 1.0, // No rotation about theta
                     r2 / r3        // theta is the rotation angle about w
    LET FLT stheta = r3<1.0 -> 0.0, // No rotation about theta
                     z  / r3        // theta is the rotation angle about w

    // Matrix to rotate aircraft clockwise about axis l
    // ( cphi   sphi   0 )
    // (-sphi   cphi   0 )
    // (    0      0   1 )

    // Matrix to rotate aircraft clockwise about axis w
    // ( ctheta   0   stheta )
    // (      0   1        0 )
    // ( -stheta  0   ctheta )

    // Combining these gives
    // (  ctheta*cphi    ctheta*sphi   stheta )
    // (        -sphi           cphi        0 )
    // ( -stheta*cphi   -stheta*sphi   ctheta )

    // Apply this matrix to the old orientation

    LET FLT nctn = ctheta*cphi*ctn + ctheta*sphi*cwn + stheta*cln
    LET FLT nctw = ctheta*cphi*ctw + ctheta*sphi*cww + stheta*clw
    LET FLT ncth = ctheta*cphi*cth + ctheta*sphi*cwh + stheta*clh

    LET FLT ncwn = -sphi*ctn + cphi*cwn
    LET FLT ncww = -sphi*ctw + cphi*cww
    LET FLT ncwh = -sphi*cth + cphi*cwh

    LET FLT ncln = -stheta*cphi*ctn - stheta*sphi*cwn + ctheta*cln
    LET FLT nclw = -stheta*cphi*ctw - stheta*sphi*cww + ctheta*clw
    LET FLT nclh = -stheta*cphi*cth - stheta*sphi*cwh + ctheta*clh


IF FALSE DO
{
newline()
writef("cgndot=%13.3f cgwdot=%13.3f cghdot=%13.3f*n", cgndot, cgwdot, cghdot)
newline()
writef("x=     %13.3f y=     %13.3f z=     %13.3f*n", x, y, z)
writef("r2=%13.3f r3=%13.3f*n", r2, r3)
newline()
writef("cphi  =%8.3f sphi=  %8.3f*n", cphi,   sphi)
writef("ctheta=%8.3f stheta=%8.3f*n", ctheta, stheta)
newline()
writef("ctn=%8.3f ctw=%8.3f cth=%8.3f*n", ctn, ctw, cth)
writef("cwn=%8.3f cww=%8.3f cwh=%8.3f*n", cwn, cww, cwh)
writef("cln=%8.3f clh=%8.3f clh=%8.3f*n", cln, clw, clh)
writef("rotation matrix about l*n")
writef("( %8.3f %8.3f %8.3f*n",  cphi, sphi, 0.0)
writef("( %8.3f %8.3f %8.3f*n", -sphi, cphi, 0.0)
writef("( %8.3f %8.3f %8.3f*n",   0.0,  0.0, 1.0)
newline()
writef("rotation matrix about w*n")
writef("( %8.3f %8.3f %8.3f*n",  ctheta,  0.0,  stheta)
writef("( %8.3f %8.3f %8.3f*n",     0.0,  1.0,     0.0)
writef("( %8.3f %8.3f %8.3f*n", -stheta,  0.0,  ctheta)
newline()
writef("combined rotation matrix*n")
writef("( %8.3f %8.3f %8.3f*n",  ctheta*cphi,  ctheta*sphi,  stheta)
writef("( %8.3f %8.3f %8.3f*n",        -sphi,        cphi,      0.0)
writef("( %8.3f %8.3f %8.3f*n", -stheta*cphi, -stheta*sphi,  ctheta)
newline()
writef("nctn=%8.3f nctw=%8.3f ncth=%8.3f*n", nctn, nctw, ncth)
writef("ncwn=%8.3f ncww=%8.3f ncwh=%8.3f*n", ncwn, ncww, ncwh)
writef("ncln=%8.3f nclw=%8.3f nclh=%8.3f*n", ncln, nclw, nclh)
newline()
}
    // Update the orientation slowly to these values
    ctn := ctn+0.1*nctn/steprate
    ctw := ctw+0.1*nctw/steprate
    cth := cth+0.1*ncth/steprate

    cwn := cwn+0.1*ncwn/steprate
    cww := cww+0.1*ncww/steprate
    cwh := cwh+0.1*ncwh/steprate

    cln := cln+0.1*ncln/steprate
    clw := clw+0.1*nclw/steprate
    clh := clh+0.1*nclh/steprate

    // These will need adjustment
    adjustorientation()

    // The aircraft is now pointing in the direction of its
    // velocity vector.
    //wdot, ldot := 0.0, 0.0
  }

writef("tdot=%13.3f wdot=%13.3f ldot=%13.3f*n", tdot, wdot, ldot)
newline()
writef(" ctn=%10.6f  ctw=%10.6f  cth=%10.6f*n",  ctn,  ctw,  cth)
writef(" cwn=%10.6f  cww=%10.6f  cwh=%10.6f*n",  cwn,  cww,  cwh)
writef(" cln=%10.6f  clw=%10.6f  clh=%10.6f*n",  cln,  clw,  clh)
newline()
writef(" cgn=%13.3f  cgw=%13.3f  cgh=%13.3f*n",  cgn,  cgw,  cgh)
newline()
abort(4567)
}

AND wheeleffect(FLT t, FLT w, FLT l) BE
{ // (t,w,l) are the aircraft coordinates of the
  // lowest point of a wheel. 
  LET FLT h = cgh + t*cth + w*cwh + l*clh  // Height above the ground.

//writef("*nstepcount=%n steprate=%6.3f fl=%6.3f massg=%6.3f*n*n",
//        stepcount, steprate, fl, massg)
//writef("wheeleffect: ctn=%6.3f ctw=%6.3f cth=%6.3f   cgh=%6.3f cghdot=%6.3f*n",
//        ctn, ctw, cth, cgh, cghdot)
//writef("wheeleffect: cwn=%6.3f cww=%6.3f cwh=%6.3f*n",
//        cwn, cww, cwh)
//writef("wheeleffect: cln=%6.3f clw=%6.3f clh=%6.3f*n",
//        cln, clw, clh)
//writef("wheeleffect:   t=%6.3f   w=%6.3f   l=%6.3f     h=%6.3f*n", t, w, l, h)
//writef("wheeleffect:   h was the height of the lowest point of the wheel*n")
//writef("wheeleffect:   after rotation of the aircraft*n")

//IF h >= 0.0 DO { writef("wheeleffect:   The wheel is above the ground*n")
//                 //abort(4466)
//               }

  IF h < 0.0 DO
  { // The lift from the wheel is proportional to the depth -h
    // and modified by a small multiple of cghdot.
    LET FLT f = -h * 0.5 * massg // Depth of 1 ft gives an upward
                                 // force equal to the 1/2 x the weight of
                                 // the aircraft

    f := f - 0.1*cghdot*massg // Small damping effect
//writef("wheeleffect:  Wheel on ground  lift=%6.3f damping=%6.3f result=%6.3f*n",
//        f, -0.1*cghdot*massg, f - 0.1*cghdot*massg)

    // Limit the size of the force.
    IF f < -1.0*massg DO f := -1.0*massg
    IF f > +1.0*massg DO f := +1.0*massg
//writef("wheeleffect:  Adjusted wheel lift=%6.3f*n", f)

    //IF thrust < 100.0 DO 
    //{ tdot := tdot - 0.2*tdot/steprate
    //  wdot := wdot - 0.2*wdot/steprate
    //  ldot := ldot - 0.2*ldot/steprate
    //}

    // Apply an upward force in in world coordinates
    // to point (t,w,l) of the aircraft.
    applygforce(t,w,l, 0.0, 0.0, f)

    IF cghdot < -1.0 DO cghdot := -1.0
    IF cghdot >  1.0 DO cghdot :=  1.0
  }
}

AND cleardepthscreen() BE
{ LET FLT val = -1e10
  IF depthscreen FOR i = 0 TO screenxsize*screenysize-1 DO
    depthscreen!i := val
}

AND plotland() BE
{ setgroundmat()

  // Draw the runway 2000 x 200 ft
  FOR i = 0 TO 99 FOR j = 0 TO 19 DO
  { LET FLT n = 20.0 * FLOAT i
    LET FLT w = -100.0 + 10.0 * FLOAT j
    LET c = ((127*n XOR 541*j) MOD 13) * 4
    setcolour(maprgb(170+c, 170+c, 170+c))
    gdrawquad3d(      n,      w, 0.0,  // The runway
                      n, 40.0+w, 0.0,
                200.0+n, 40.0+w, 0.0,
                200.0+n,      w, 0.0)
  }
}

AND plotcraft() BE
{ coselevator := sys(Sys_flt, fl_cos, elevator*0.7)
  sinelevator := sys(Sys_flt, fl_sin, elevator*0.7)
  cosaileron  := sys(Sys_flt, fl_cos, aileron*0.7)
  sinaileron  := sys(Sys_flt, fl_sin, aileron*0.7)
  cosrudder   := sys(Sys_flt, fl_cos, rudder*0.7)
  sinrudder   := sys(Sys_flt, fl_sin, rudder*0.7)

//writef("plotcraft:   elevator =%8.3f     aileron=  %8.3f    rudder= %8.3f*n",
//        elevator, aileron, rudder)
//writef("plotcraft: coselevator=%8.3f   sinelevator=%8.3f*n", coselevator, sinelevator)
//writef("plotcraft: cosaileron =%8.3f   sinaileron= %8.3f*n",  cosaileron, sinaileron)
//writef("plotcraft: cosrudder  =%8.3f   sinrudder=  %8.3f*n",   cosrudder, sinrudder)

  setaircraftmat()

  IF FALSE DO
  { // Debugging simple aircraft
    setcolour(maprgb(255,0,0)) //Red
    cdrawtriangle3d( 2.0,  0.0, 0.0,    // left wing
                     0.0, 10.0, 0.0,
                    -2.0,  0.0, 0.0)

    setcolour(maprgb(0,255,0)) //Green
    cdrawtriangle3d( 2.0,  0.0, 0.0,    // Right wing
                     0.0,-10.0, 0.0,
                    -2.0,  0.0, 0.0)

    setcolour(maprgb(0,0,255)) //Blue
    cdrawtriangle3d( 10.0,  0.0,  0.0,    // Body
                      0.0,  0.0, -2.0,
                    -10.0,  0.0,  0.0)

    setcolour(maprgb(255,255,0)) //Yellow
    cdrawtriangle3d(  -8.0,  0.0, 0.0,    // Fin
                     -10.0,  0.0, 2.0,
                     -10.0,  0.0, 0.0)

    RETURN
  }

  drawcraftside(Left)
  drawcraftside(Right)
//updatescreen()
//abort(2345)
}

AND drawcraftside(side) BE
{ drawbody(side)
  drawwing(side)
  drawelevator(side)
  drawfin(side)
}

AND drawwheel(t, w, l) BE IF geardown DO
{ LET FLT T1t, FLT T1w, FLT T1l = t,  w,        l
  LET FLT T2t, FLT T2w, FLT T2l = t-0.2, w-0.1, l
  LET FLT T3t, FLT T3w, FLT T3l = t-0.2, w+0.1, l

  LET FLT B1t, FLT B1w, FLT B1l = T1t, T1w, T1l-1.0
  LET FLT B2t, FLT B2w, FLT B2l = T2t, T2w, T3l-1.0
  LET FLT B3t, FLT B3w, FLT B3l = T3t, T3w, T3l-1.0

  LET FLT WRt, FLT WRw, FLT WRl = t-0.1, B1w-0.13, B1l 
  LET FLT WLt, FLT WLw, FLT WLl = t-0.1, B1w+0.13, B1l

  // Draw strut
  setcolour(maprgb(84,84,84))
  cdrawquad3d(T1t, T1w, T1l,
              T2t, T2w, T2l,
              B2t, B2w, B2l,
              B1t, B1w, B2l)
  cdrawquad3d(T1t, T1w, T1l,
              T3t, T3w, T3l,
              B3t, B3w, B3l,
              B1t, B1w, B1l)
  setcolour(maprgb(54,54,54))
  cdrawquad3d(T3t, T3w, T3l,
              T2t, T2w, T2l,
              B2t, B2w, B2l,
              B3t, B3w, B3l)
 
  // Draw tyre
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WRt,        WRw, WRl,        // Top quadrant
                  WRt+0.000, WRw, WRl+0.282,
                  WRt-0.200, WRw, WRl+0.200)
  setcolour(maprgb(150,81,75))
  cdrawquad3d(WRt+0.000, WRw, WRl+0.282,
              WRt-0.200, WRw, WRl+0.200,
              WLt-0.200, WLw, WLl+0.200,
              WLt+0.000, WLw, WLl+0.282)
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WLt,        WLw, WLl,
                  WLt+0.000, WLw, WLl+0.282,
                  WLt-0.200, WLw, WLl+0.200)

  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WRt,        WRw, WRl,
                  WRt+0.000, WRw, WRl+0.282,
                  WRt+0.200, WRw, WRl+0.200)
  setcolour(maprgb(150,81,75))
  cdrawquad3d(WRt+0.000, WRw, WRl+0.282,
              WRt+0.200, WRw, WRl+0.200,
              WLt+0.200, WLw, WLl+0.200,
              WLt+0.000, WLw, WLl+0.282)
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WLt,        WLw, WLl,
                  WLt+0.000, WLw, WLl+0.282,
                  WLt+0.200, WLw, WLl+0.200)

  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WRt,       WRw, WRl,        // Back quadrant
                  WRt-0.282, WRw, WRl+0.000,
                  WRt-0.200, WRw, WRl+0.200)
  setcolour(maprgb(150,81,75))
  cdrawquad3d(WRt-0.200, WRw, WRl+0.200,
              WRt-0.282, WRw, WRl+0.000,
              WLt-0.282, WLw, WLl+0.000,
              WLt-0.200, WLw, WLl+0.200)
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WLt,       WLw, WLl,
                  WLt-0.282, WLw, WLl+0.000,
                  WLt-0.200, WLw, WLl+0.200)

  cdrawtriangle3d(WRt,       WRw, WRl,
                  WRt-0.282, WRw, WRl-0.000,
                  WRt-0.200, WRw, WRl-0.200)
  setcolour(maprgb(150,81,75))
  cdrawquad3d(WRt-0.200, WRw, WRl-0.200,
              WRt-0.282, WRw, WRl-0.000,
              WLt-0.282, WLw, WLl-0.000,
              WLt-0.200, WLw, WLl-0.200)
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WLt,       WLw, WLl,
                  WLt-0.282, WLw, WLl-0.000,
                  WLt-0.200, WLw, WLl-0.200)

  cdrawtriangle3d(WRt,       WRw, WRl,        // Forward quadrant
                  WRt+0.282, WRw, WRl+0.000,
                  WRt+0.200, WRw, WRl+0.200)
  setcolour(maprgb(150,81,75))
  cdrawquad3d(WRt+0.200, WRw, WRl+0.200,
              WRt+0.282, WRw, WRl+0.000,
              WLt+0.282, WLw, WLl+0.000,
              WLt+0.200, WLw, WLl+0.200)
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WLt,       WLw, WLl,
                  WLt+0.282, WLw, WLl+0.000,
                  WLt+0.200, WLw, WLl+0.200)
  cdrawtriangle3d(WRt,       WRw, WRl,
                  WRt+0.282, WRw, WRl-0.000,
                  WRt+0.200, WRw, WRl-0.200)
  setcolour(maprgb(150,81,75))
  cdrawquad3d(WRt+0.200, WRw, WRl-0.200,
              WRt+0.282, WRw, WRl-0.000,
              WLt+0.282, WLw, WLl-0.000,
              WLt+0.200, WLw, WLl-0.200)
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WLt,       WLw, WLl,
                  WLt+0.282, WLw, WLl-0.000,
                  WLt+0.200, WLw, WLl-0.200)


  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WRt,       WRw, WRl,        // Bottom quadrant
                  WRt+0.000, WRw, WRl-0.282,
                  WRt-0.200, WRw, WRl-0.200)
  setcolour(maprgb(150,81,75))
  cdrawquad3d(WRt+0.000, WRw, WRl-0.282,
              WRt-0.200, WRw, WRl-0.200,
              WLt-0.200, WLw, WLl-0.200,
              WLt+0.000, WLw, WLl-0.282)
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WLt,       WLw, WLl,
                  WLt+0.000, WLw, WLl-0.282,
                  WLt-0.200, WLw, WLl-0.200)

  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WRt,       WRw, WRl,
                  WRt+0.000, WRw, WRl-0.282,
                  WRt+0.200, WRw, WRl-0.200)
  setcolour(maprgb(150,81,75))
  cdrawquad3d(WRt+0.000, WRw, WRl-0.282,
              WRt+0.200, WRw, WRl-0.200,
              WLt+0.200, WLw, WLl-0.200,
              WLt+0.000, WLw, WLl-0.282)
  setcolour(maprgb(180,50,104))
  cdrawtriangle3d(WLt,       WLw, WLl,
                  WLt+0.000, WLw, WLl-0.282,
                  WLt+0.200, WLw, WLl-0.200)
}

AND drawbody(FLT side) BE
{ LET FLT  B1t, FLT  B1w, FLT  B1l  =   6.0, side*0.50,  0.50
  LET FLT  B2t, FLT  B2w, FLT  B2l  =   6.0, side*0.30, -0.50
  LET FLT  B3t, FLT  B3w, FLT  B3l  =   3.2, side*1.00, -1.00
  LET FLT  B4t, FLT  B4w, FLT  B4l  =   2.5, side*1.00,  1.00
  LET FLT  B5t, FLT  B5w, FLT  B5l  =   1.8, side*0.70,  2.30
  LET FLT  B6t, FLT  B6w, FLT  B6l  =  -0.5, side*0.70,  2.40
  LET FLT  B7t, FLT  B7w, FLT  B7l  =  -1.0, side*1.00,  1.00
  LET FLT  B8t, FLT  B8w, FLT  B8l  =  -1.0, side*1.00, -1.00
  LET FLT  B9t, FLT  B9w, FLT  B9l  = -12.0, side*0.06, -0.25
  LET FLT B10t, FLT B10w, FLT B10l  = -12.0, side*0.06, +0.25

  // These vertices are numbered as follow

  //              5 - - - 6 - _
  //             /         \    - - - _
  //      _ - - 4 - - - - - 7           - - - - -  _
  //    1       |           |                        - - - -  10
  //    |      /            |                                  |
  //    2 _   |             |         _ - - - - - - - - - - - -9
  //        - 3 - - - - - - 8 - - - - 

  setcolour(maprgb(164,160,114))
/*
  setcolour(maprgb(255,0,0))
  B1t, B1w, B1l :=  2.0,  1.0,  0.0
  B2t, B2w, B2l :=  2.0,  1.0,  3.0
  B3t, B3w, B3l :=  6.0,  1.0,  0.0
newline()
writef("drawing triangle: %10.3f %10.3f %10.3f red*n", B1t, B1w, B1l)
writef("                  %10.3f %10.3f %10.3f*n", B2t, B2w, B2l)
writef("                  %10.3f %10.3f %10.3f*n", B3t, B3w, B3l)
  cdrawtriangle3d( B1t,  B1w,  B1l, // Engine
                   B2t,  B2w,  B2l,
                   B3t,  B3w,  B3l)
  setcolour(maprgb(0,0,255))
  B1t, B1w, B1l :=  2.0, -1.0,  0.0
  B2t, B2w, B2l :=  2.0, -1.0,  3.0
  B3t, B3w, B3l :=  6.0, -1.0,  0.0
newline()
writef("drawing triangle: %10.3f %10.3f %10.3f blue*n", B1t, B1w, B1l)
writef("                  %10.3f %10.3f %10.3f*n", B2t, B2w, B2l)
writef("                  %10.3f %10.3f %10.3f*n", B3t, B3w, B3l)
  cdrawtriangle3d( B1t,  B1w,  B1l, // Engine
                   B2t,  B2w,  B2l,
                   B3t,  B3w,  B3l)
RETURN
*/
//newline()
//writef("drawing triangle: %10.3f %10.3f %10.3f*n", B1t, B1w, B1l)
//writef("                  %10.3f %10.3f %10.3f*n", B2t, B2w, B2l)
//writef("                  %10.3f %10.3f %10.3f*n", B3t, B3w, B3l)
  cdrawtriangle3d( B1t,  B1w,  B1l, // Engine
                   B2t,  B2w,  B2l,
                   B3t,  B3w,  B3l)

//updatescreen()
//abort(369)

  setcolour(maprgb(154,178,104))
  cdrawtriangle3d( B1t,  B1w,  B1l,
                   B3t,  B3w,  B3l,
                   B4t,  B4w,  B4l)
  setcolour(maprgb(190,190,170))
//  setcolour(maprgb(0,0,255))
  cdrawtriangle3d( B4t,  B4w,  B4l, // Cockpit
                   B5t,  B5w,  B5l,
                   B7t,  B7w,  B7l)
  cdrawtriangle3d( B5t,  B5w,  B5l,
                   B6t,  B6w,  B6l,
                   B7t,  B7w,  B7l)

  setcolour(maprgb(144,168,124))
  cdrawtriangle3d( B3t,  B3w,  B3l,
                   B4t,  B4w,  B4l,
                   B7t,  B7w,  B7l)
  cdrawtriangle3d( B3t,  B3w,  B3l,
                   B7t,  B7w,  B7l,
                   B8t,  B8w,  B8l)

  setcolour(maprgb(164,160,114))

  cdrawtriangle3d( B8t,  B8w,  B8l, // Tail
                   B7t,  B7w,  B7l,
                   B9t,  B9w,  B9l)
  setcolour(maprgb(154,168,114))
  cdrawtriangle3d( B7t,  B7w,  B7l,
                   B9t,  B9w,  B9l,
                   B10t, B10w, B10l)
  setcolour(maprgb(164,178,114))
  cdrawtriangle3d( B7t,  B7w,  B7l,
                   B6t,  B6w,  B6l,
                   B10t, B10w, B10l)

  IF side>0.0 DO
  { // Draw midline panels

    setcolour(maprgb(120,120,120))
    cdrawtriangle3d( B1t,  B1w,  B1l, // Engine front
                     B2t,  B2w,  B2l,
                     B2t, -B2w,  B2l)
    cdrawtriangle3d( B1t,  B1w,  B1l,
                     B2t, -B2w,  B2l,
                     B1t, -B1w,  B1l)
    setcolour(maprgb(174,158,154))
    cdrawtriangle3d( B1t,  B1w,  B1l, // Engine top
                     B1t, -B1w,  B1l,
                     B4t,  B4w,  B4l)
    cdrawtriangle3d( B1t, -B1w,  B1l,
                     B4t, -B4w,  B4l,
                     B4t,  B4w,  B4l)
    setcolour(maprgb(220,220,220))
    cdrawtriangle3d( B4t,  B4w,  B4l, // Wind shield
                     B4t, -B4w,  B4l,
                     B5t,  B5w,  B5l)
    cdrawtriangle3d( B4t, -B4w,  B4l,
                     B5t, -B5w,  B5l,
                     B5t,  B5w,  B5l)
    setcolour(maprgb(164,158,134))
    //setcolour(maprgb(255,0,0))
    cdrawtriangle3d( B5t,  B5w,   B5l, // Cockpit top
                     B6t,  B6w,   B6l,
                     B6t, -B6w,   B6l)
    //setcolour(maprgb(0,0,255))
    cdrawtriangle3d( B5t,  B5w,   B5l,
                     B5t, -B5w,   B5l,
                     B6t, -B6w,   B6l)
    setcolour(maprgb(154,148,144))
    cdrawtriangle3d( B6t,   B6w,  B6l, // Tail top
                     B10t,  B10w, B10l,
                     B10t, -B10w, B10l)
    cdrawtriangle3d( B6t,   B6w,  B6l,
                     B6t,  -B6w,  B6l,
                     B10t, -B10w, B10l)

    setcolour(maprgb(184,178,84))
    cdrawtriangle3d( B9t,   B9w,  B9l, // Tail end
                     B10t,  B10w, B10l,
                     B10t, -B10w, B10l)
    cdrawtriangle3d( B9t,  B9w,  B9l,
                     B9t, -B9w,  B9l,
                     B10t,-B10w, B10l)

    setcolour(maprgb(134,148,144))
    cdrawtriangle3d( B8t,  B8w,  B8l, // Tail bottom surface
                     B8t, -B8w,  B8l,
                     B9t,  B9w,  B9l)
    cdrawtriangle3d( B8t, -B8w,  B8l,
                     B9t, -B9w,  B9l,
                     B9t,  B9w,  B9l)

    setcolour(maprgb(144,168,134))
    cdrawtriangle3d( B3t,  B3w,  B3l, // Cockpit bottom surface
                     B3t, -B8w,  B8l,
                     B8t,  B8w,  B8l)
    cdrawtriangle3d( B3t, -B3w,  B3l,
                     B8t, -B8w,  B8l,
                     B8t,  B8w,  B8l)

    setcolour(maprgb(144,148,114))
    cdrawtriangle3d( B2t,  B2w,  B2l, // Engine bottom surface
                     B2t, -B2w,  B2l,
                     B3t,  B3w,  B3l)
    cdrawtriangle3d( B2t, -B2w,  B2l,
                     B3t, -B3w,  B3l,
                     B3t,  B3w,  B3l)

    drawwheel(4.0, 0.0, -0.9)
    // Lowest point of wheel is -0.9 - 1.0 - 0.285 = -2.185
  }
}

AND drawwing(FLT side) BE
{ LET FLT  W1t, FLT  W1w, FLT  W1l  =  3.0, side*1.00,  -1.00
  LET FLT  W2t, FLT  W2w, FLT  W2l  =  1.5, side*1.00,  -0.55
  LET FLT  W3t, FLT  W3w, FLT  W3l  =  1.5, side*1.00,  -1.00
  LET FLT  W4t, FLT  W4w, FLT  W4l  = -1.0, side*1.00,  -0.90
  LET FLT  W5t, FLT  W5w, FLT  W5l  = -1.0, side*1.00,  -1.00
  LET FLT  W6t, FLT  W6w, FLT  W6l  = -1.8, side*1.40,  -1.00 // Aileron near edge
  LET FLT  W7t, FLT  W7w, FLT  W7l  = -2.8, side*11.30,  0.40 // Aileron near edge
  LET FLT  W8t, FLT  W8w, FLT  W8l  = -2.0, side*12.20,  0.60
  LET FLT  W9t, FLT  W9w, FLT  W9l  = -2.0, side*12.20,  0.50
  LET FLT W10t, FLT W10w, FLT W10l  = -0.3, side*12.50,  0.85
  LET FLT W11t, FLT W11w, FLT W11l  = -0.3, side*12.50,  0.65
  LET FLT W12t, FLT W12w, FLT W12l  =  1.3, side*11.50,  0.55

  //       12----------------------------1
  //      /                              |
  //   10/11         Left wing          2/3
  //     |                               |
  //    8/9---------------------------- 4/5
  //      7----------------------------6

  W6l := W6l + side*sinaileron*0.8       // Aileron adjustment
  W6t := W6t + side*(1.0-cosaileron)*0.8
  W7l := W7l + side*sinaileron*0.8
  W7t := W7t + side*(1.0-cosaileron)*0.8

  colour(120,150,140)
  //IF side = Left  DO colour(255,0,0)   // Debugging code
  //IF side = Right DO colour(0,255,0)

  cdrawtriangle3d(W1t,  W1w,  W1l,  // Top surface
                  W2t,  W2w,  W2l,
                  W12t, W12w, W12l)
  cdrawtriangle3d(W2t,  W2w,  W2l,
                  W10t, W10w, W10l,
                  W12t, W12w, W12l)
  colour(100,160,120)
  cdrawtriangle3d(W2t,  W2w,  W2l,
                  W4t,  W4w,  W4l,
                  W10t, W10w, W10l)
  cdrawtriangle3d(W4t,  W4w,  W4l,
                  W10t, W10w, W10l,
                  W8t,  W8w,  W8l)
  colour(130,150,130)
  cdrawtriangle3d(W4t,  W4w,  W4l,
                  W8t,  W8w,  W8l,
                  W6t,  W6w,  W6l)
  cdrawtriangle3d(W8t,  W8w,  W8l,
                  W6t,  W6w,  W6l,
                  W7t,  W7w,  W7l)

  colour(120,160,70)
  cdrawtriangle3d(W1t,  W1w,  W1l,  // Bottom surface
                  W3t,  W3w,  W3l,
                  W12t, W12w, W12l)
  colour(110,150,80)
  cdrawtriangle3d(W3t,  W3w,  W3l,
                  W11t, W11w, W11l,
                  W12t, W12w, W12l)
  colour(120,150,80)
  cdrawtriangle3d(W3t,  W3w,  W3l,
                  W5t,  W5w,  W5l,
                  W11t, W11w, W11l)
  colour(130,160,70)
  cdrawtriangle3d(W5t,  W5w,  W5l,
                  W11t, W11w, W11l,
                  W9t,  W9w,  W9l)
  colour(120,170,65)
  cdrawtriangle3d(W5t,  W5w,  W5l,
                  W9t,  W9w,  W9l,
                  W6t,  W6w,  W6l)
  colour(110,160,75)
  cdrawtriangle3d(W9t,  W9w,  W9l,
                  W6t,  W6w,  W6l,
                  W7t,  W7w,  W7l)

  colour(130,100,100)
  cdrawtriangle3d(W4t, W4w, W4l,     // Root end triangles
                  W5t, W5w, W5l,
                  W6t, W6w, W6l)
  colour(140,120,60)
  cdrawtriangle3d(W10t, W10w, W10l,  // Tip end triangles
                  W11t, W11w, W11l,
                  W12t, W12w, W12l)
  colour(130,130,60)
  cdrawtriangle3d(W10t, W10w, W10l,
                  W11t, W11w, W11l,
                  W8t,  W8w,  W8l)
  colour(140,130,70)
  cdrawtriangle3d(W8t,  W8w,  W8l,
                  W9t,  W9w,  W9l,
                  W11t, W11w, W11l)
  colour(120,120,80)
  cdrawtriangle3d(W7t,  W7w,  W7l,  // Aileron tip
                  W8t,  W8w,  W8l,
                  W9t,  W9w,  W9l)

  drawwheel(-0.7, side*2.0, -0.9)

}

AND drawelevator(FLT side) BE
{ LET FLT E1t, FLT E1w, FLT E1l  = -10.0, side*0.00,  0.00
  LET FLT E2t, FLT E2w, FLT E2l  = -12.0, side*0.10,  0.05
  LET FLT E3t, FLT E3w, FLT E3l  = -12.0, side*0.10, -0.05
  LET FLT E4t, FLT E4w, FLT E4l  = -12.7, side*0.70,  0.00
  LET FLT E5t, FLT E5w, FLT E5l  = -12.7, side*3.50,  0.00
  LET FLT E6t, FLT E6w, FLT E6l  = -12.0, side*4.05,  0.05
  LET FLT E7t, FLT E7w, FLT E7l  = -12.0, side*4.05, -0.05
  LET FLT E8t, FLT E8w, FLT E8l  = -11.1, side*3.75,  0.00

  //        8--------------1
  //       /    Elevator   |
  //     6/7--------------2/3
  //       \              /
  //        5------------4

  E4l := E4l - sinelevator*0.7       // Elevator adjustment
  E4t := E4t + (1.0-coselevator)*0.7
  E5l := E5l - sinelevator*0.7
  E5t := E5t + (1.0-coselevator)*0.7

  colour(133,155,70)
  cdrawtriangle3d(E1t, E1w, E1l,  // Top surface
                  E2t, E2w, E2l,
                  E6t, E6w, E6l)
  colour(155,178,90)
  cdrawtriangle3d(E1t, E1w, E1l,
                  E6t, E6w, E6l,
                  E8t, E8w, E8l)
  colour(154,150,60)
  cdrawtriangle3d(E2t, E2w, E2l,
                  E4t, E4w, E4l,
                  E5t, E5w, E5l)
  cdrawtriangle3d(E2t, E2w, E2l,
                  E5t, E5w, E5l,
                  E6t, E6w, E6l)

  colour(140,150,80)
  cdrawtriangle3d(E1t, E1w, E1l,  // Bottom surface
                  E3t, E3w, E3l,
                  E7t, E7w, E7l)
  colour(120,140,105)
  cdrawtriangle3d(E1t, E1w, E1l,
                  E7t, E7w, E7l,
                  E8t, E8w, E8l)
  colour(150,150,80)
  cdrawtriangle3d(E3t, E3w, E3l,
                  E4t, E4w, E4l,
                  E5t, E5w, E5l)
  cdrawtriangle3d(E3t, E3w, E3l,
                  E5t, E5w, E5l,
                  E7t, E7w, E7l)

  colour(130,100,120)
  cdrawtriangle3d(E2t, E2w, E2l,  // end triangles
                  E3t, E3w, E3l,
                  E4t, E4w, E4l)
  colour(140,130,160)
  cdrawtriangle3d(E6t, E6w, E6l,
                  E7t, E7w, E7l,
                  E5t, E5w, E5l)
  colour(150,120,160)
  cdrawtriangle3d(E6t, E6w, E6l,
                  E7t, E7w, E7l,
                  E8t, E8w, E8l)
}

AND colour(r,g,b) BE setcolour(maprgb(r,g,b))

AND drawfin(FLT side) BE
{ LET FLT F1t, FLT F1w, FLT F1l  = -10.0, side*0.00, 0.25
  LET FLT F2t, FLT F2w, FLT F2l  = -12.0, side*0.05,-0.25
  LET FLT F3t, FLT F3w, FLT F3l  = -12.7, side*0.00, 0.00
  LET FLT F4t, FLT F4w, FLT F4l  = -12.7, side*0.00, 2.00
  LET FLT F5t, FLT F5w, FLT F5l  = -12.0, side*0.05, 2.50
  LET FLT F6t, FLT F6w, FLT F6l  = -11.3, side*0.00, 2.50

  //          6----5
  //         /     | \
  //        / side |  4
  //       /   of  |  |
  //      /   fin  |  3
  //     1___      | /
  //          -----2

  F3w := F3w - sinrudder*0.7       // Rudder adjustment
  F3t := F3t + (1.0-cosrudder)*0.7
  F4w := F4w - sinrudder*0.7
  F4t := F4t + (1.0-cosrudder)*0.7

  colour(135,135,100)
  cdrawtriangle3d(F1t, F1w, F1l,
                  F2t, F2w, F2l,
                  F6t, F6w, F6l)
  colour(135,145,120)
  cdrawtriangle3d(F2t, F2w, F2l,
                  F5t, F5w, F5l,
                  F6t, F6w, F6l)
  colour(165,155,110)
  cdrawtriangle3d(F2t, F2w, F2l,
                  F5t, F5w, F5l,
                  F3t, F3w, F3l)
  cdrawtriangle3d(F4t, F4w, F4l,
                  F5t, F5w, F5l,
                  F3t, F3w, F3l)

  IF side>0 DO
  { colour(105,135,120)
    cdrawtriangle3d(F2t, F2w, F2l, // Fin end triangles
                    F2t,-F2w, F2l,
                    F3t, F3w, F3l)
    cdrawtriangle3d(F5t, F5w, F5l,
                    F5t,-F5w, F5l,
                    F4t, F4w, F4l)
    colour(135,125,130)
    cdrawtriangle3d(F5t, F5w, F5l,
                    F5t,-F5w, F5l,
                    F6t, F6w, F6l)
 
  }

}

AND cdrawquad3d(FLT t1, FLT w1, FLT l1,
                FLT t2, FLT w2, FLT l2,
                FLT t3, FLT w3, FLT l3,
                FLT t4, FLT w4, FLT l4) BE
{ // The rotation matrix used by screencoords is already set
  // by setaircraftmat or setgroundmat in

  //     ( m00 m01 m02 )
  //     ( m10 m11 m12 )
  //     ( m20 m21 m22 )

  // Screen coordinates of the four vertices.
  LET FLT sx1, FLT sy1, FLT sz1 = ?,?,?
  LET FLT sx2, FLT sy2, FLT sz2 = ?,?,?
  LET FLT sx3, FLT sy3, FLT sz3 = ?,?,?
  LET FLT sx4, FLT sy4, FLT sz4 = ?,?,?

  // Calculate the screen coordinates
  UNLESS screencoords(t1, w1, l1, @sx1) RETURN
  UNLESS screencoords(t2, w2, l2, @sx2) RETURN
  UNLESS screencoords(t3, w3, l3, @sx3) RETURN
  UNLESS screencoords(t4, w4, l4, @sx4) RETURN

//writef("cdrawtri3d: %13.3f %13.3f %13.3f*n", t1, w1, l1)
//writef("            %13.3f %13.3f %13.3f*n", t2, w2, l2)
//writef("            %13.3f %13.3f %13.3f*n", t3, w3, l3)
//writef("            %13.3f %13.3f %13.3f*n", t4, w4, l4)

//writef("cetn=%6.3f cewn=%6.3f celn=%6.3f*n", cetn, cewn, celn)
//writef("cetw=%6.3f ceww=%6.3f celw=%6.3f*n", cetw, ceww, celw)
//writef("ceth=%6.3f cewh=%6.3f celh=%6.3f*n", ceth, cewh, celh)

//writef("ctn=%6.3f cwn=%6.3f cln=%6.3f*n", ctn, cwn, cln)
//writef("ctw=%6.3f cww=%6.3f clw=%6.3f*n", ctw, cww, clw)
//writef("cth=%6.3f cwh=%6.3f clh=%6.3f*n", cth, cwh, clh)

//writef("m00=%6.3f m01=%6.3f m02=%6.3f*n", m00, m01, m02)
//writef("m10=%6.3f m11=%6.3f m12=%6.3f*n", m10, m11, m12)
//writef("m20=%6.3f m21=%6.3f m22=%6.3f*n", m20, m21, m22)

//writef("sx1=%5i sy1=%5i sz1=%13.1f*n", FIX sx1, FIX sy1, sz1)
//writef("sx2=%5i sy2=%5i sz2=%13.1f*n", FIX sx2, FIX sy2, sz2)
//writef("sx3=%5i sy3=%5i sz2=%13.1f*n", FIX sx3, FIX sy3, sz3)
//writef("sx4=%5i sy4=%5i sz4=%13.1f*n", FIX sx4, FIX sy4, sz4)
//newline()

  drawquad3d(FIX sx1, FIX sy1, sz1,
             FIX sx2, FIX sy2, sz2,
             FIX sx3, FIX sy3, sz3,
             FIX sx4, FIX sy4, sz4)

//updatescreen()
//abort(2468)
}

AND cdrawtriangle3d(FLT t1, FLT w1, FLT l1,
                    FLT t2, FLT w2, FLT l2,
                    FLT t3, FLT w3, FLT l3) BE
{ // The rotation matrix used by screencoords is already set
  // by setaircraftmat or setgroundmat in

  //     ( m00 m01 m02 )
  //     ( m10 m11 m12 )
  //     ( m20 m21 m22 )

  // Screen coordinates of the three vertices.
  LET FLT sx1, FLT sy1, FLT sz1 = ?,?,?
  LET FLT sx2, FLT sy2, FLT sz2 = ?,?,?
  LET FLT sx3, FLT sy3, FLT sz3 = ?,?,?

  // Calculate the screen coordinates
  UNLESS screencoords(t1, w1, l1, @sx1) RETURN
  UNLESS screencoords(t2, w2, l2, @sx2) RETURN
  UNLESS screencoords(t3, w3, l3, @sx3) RETURN

//writef("cdrawtri3d: %13.3f %13.3f %13.3f*n", t1, w1, l1)
//writef("            %13.3f %13.3f %13.3f*n", t2, w2, l2)
//writef("            %13.3f %13.3f %13.3f*n", t3, w3, l3)

//writef("cetn=%6.3f cewn=%6.3f celn=%6.3f*n", cetn, cewn, celn)
//writef("cetw=%6.3f ceww=%6.3f celw=%6.3f*n", cetw, ceww, celw)
//writef("ceth=%6.3f cewh=%6.3f celh=%6.3f*n", ceth, cewh, celh)

//writef("ctn=%6.3f cwn=%6.3f cln=%6.3f*n", ctn, cwn, cln)
//writef("ctw=%6.3f cww=%6.3f clw=%6.3f*n", ctw, cww, clw)
//writef("cth=%6.3f cwh=%6.3f clh=%6.3f*n", cth, cwh, clh)

//writef("m00=%6.3f m01=%6.3f m02=%6.3f*n", m00, m01, m02)
//writef("m10=%6.3f m11=%6.3f m12=%6.3f*n", m10, m11, m12)
//writef("m20=%6.3f m21=%6.3f m22=%6.3f*n", m20, m21, m22)

//writef("sx1=%5i sy1=%5i sz1=%13.1f*n", FIX sx1, FIX sy1, sz1)
//writef("sx2=%5i sy2=%5i sz2=%13.1f*n", FIX sx2, FIX sy2, sz2)
//writef("sx3=%5i sy3=%5i sz2=%13.1f*n", FIX sx3, FIX sy3, sz3)
//newline()

  drawtriangle3d(FIX sx1, FIX sy1, sz1,
                 FIX sx2, FIX sy2, sz2,
                 FIX sx3, FIX sy3, sz3)

//updatescreen()
//abort(2468)
}

AND setaircraftmat() BE
{ // It is easy to see that

  // ( ctn ctw cth )   ( N )    ( t )
  // ( cwn cww cwh ) x ( W ) => ( w )      (equation 1)
  // ( cln clw clh )   ( H )    ( l )

  // where (N,W,H) are the real world coordinates of a
  // vertex and (t,w,l) are its coordinates with respect
  // to the aircraft axes with the same origin. To convert
  // aircraft coordinates to world coordinates we need the
  // inverse matrix, but this is simple since, in this
  // case, it just turns out to be its transpose.

  // ( ctn cwn cln )   ( t )    ( N )
  // ( ctw cww clw ) x ( w ) => ( W )
  // ( cth cwh clh )   ( l )    ( H )

  // We can see this by multiplying both sides of equation 1
  // by the transposed matrix.

  // ( ctn cwn cln )   ( ctn ctw cth )   ( N )
  // ( ctw cww clw ) x ( cwn cww cwh ) x ( W ) =>
  // ( cth cwh clh )   ( cln clw clh )   ( H )

  //                   ( ctn cwn cln )   ( t )
  //                   ( ctw cww clw ) x ( w )
  //                   ( cth cwh clh )   ( l )

  // The product of the two matrices on the left reduces to
  // the identity matrix since their elements are all direction
  // cosines of orthogonal unit vectors.

  // The rotation matrix required by screencoords for vertices in
  // the aircraft model is thus

  // ( cetn cetw ceth )   ( ctn cwn cln )    ( m00 m01 m02 )
  // ( cewn ceww cewh ) x ( ctw cww clw ) => ( m10 m11 m12 )
  // ( celn celw celh )   ( cth cwh clh )    ( m20 m21 m22 )

  m00 := cetn*ctn + cetw*ctw + ceth*cth
  m01 := cetn*cwn + cetw*cww + ceth*cwh
  m02 := cetn*cln + cetw*clw + ceth*clh

  m10 := cewn*ctn + ceww*ctw + cewh*cth
  m11 := cewn*cwn + ceww*cww + cewh*cwh
  m12 := cewn*cln + ceww*clw + cewh*clh

  m20 := celn*ctn + celw*ctw + celh*cth
  m21 := celn*cwn + celw*cww + celh*cwh
  m22 := celn*cln + celw*clw + celh*clh

//writef("*nsetaircraftmat:*n")
//writef("           cetn=%10.3f  cetw=%10.3f  ceth=%10.3f*n", cetn,cetw,ceth)
//writef("           cewn=%10.3f  ceww=%10.3f  cewh=%10.3f*n", cewn,ceww,cewh)
//writef("           celn=%10.3f  celw=%10.3f  celh=%10.3f*n", celn,celw,celh)

//newline()
//writef("            ctn=%10.3f   ctw=%10.3f   cth=%10.3f*n", ctn,ctw,cth)
//writef("            cwn=%10.3f   cww=%10.3f   cwh=%10.3f*n", cwn,cww,cwh)
//writef("            cln=%10.3f   clw=%10.3f   clh=%10.3f*n", cln,clw,clh)

//writef("Transposed*n"
//writef("            ctn=%10.3f   ctw=%10.3f   cth=%10.3f*n", ctn,cwn,cln)
//writef("            cwn=%10.3f   cww=%10.3f   cwh=%10.3f*n", ctw,cww,clw)
//writef("            cln=%10.3f   clw=%10.3f   clh=%10.3f*n", cth,cwh,clh)

//newline()
//writef("            m00=%10.3f   m01=%10.3f   m02=%10.3f*n", m00,m01,m02)
//writef("            m10=%10.3f   m11=%10.3f   m12=%10.3f*n", m10,m11,m12)
//writef("            m20=%10.3f   m21=%10.3f   m22=%10.3f*n", m20,m21,m22)
}

AND setgroundmat() BE
{ // For ground points no aircraft rotation is necessary, so

  // ( cetn cetw ceth )    ( m00 m01 m02 )
  // ( cewn ceww cewh ) => ( m10 m11 m12 )
  // ( celn celw celh )    ( m20 m21 m22 )

  m00, m01, m02 := cetn, cetw, ceth
  m10, m11, m12 := cewn, ceww, cewh
  m20, m21, m22 := celn, celw, celh
}

AND screencoords(FLT x, FLT y, FLT z, v) = VALOF
{ // (x,y,z) are either the coordinates of a vertex in the aircraft
  // model or the coordinates of a point on the ground using
  // (cgn,cgw,cgh) as the origin. The eye is looking directly
  // at this origin. If the point is in view its screen coordinates
  // are placed in v!0, v!1 and v!2 and the result is TRUE. If the
  // point is not in view the result is FALSE.
  // The one or two rotations are performed by multiplying the
  // point by the matrix
  //     ( m00 m01 m02 )
  //     ( m10 m11 m12 )
  //     ( m20 m21 m22 )

  LET FLT sizeby2 = (fscreenxsize<=fscreenysize ->
                     fscreenxsize, fscreenysize) / 2.0

  LET FLT mx = fscreenxsize / 2.0  // The mid point of the screen
  LET FLT my = fscreenysize / 2.0

  LET FLT sx, FLT sy, FLT sz = ?, ?, ?

  // Deal with eye orientation
  sz := m00*x + m01*y + m02*z + eyedist // Positive depth

  IF sz < 1.0 DO
  { // The point is behind the eye or less than 1 ft in front
    RESULTIS FALSE
  }

  sx :=  mx - sizeby2 * (m10*x + m11*y + m12*z) / sz // Horizontal
  sy :=  my + sizeby2 * (m20*x + m21*y + m22*z) / sz // Vertical

//newline()
//writef("screencoords: x=%10.3f     y=%10.3f     z=%10.3f*n", x,y,z)
//newline()
//writef("           cetn=%10.3f  cetw=%10.3f  ceth=%10.3f*n", cetn,cetw,ceth)
//writef("           cewn=%10.3f  ceww=%10.3f  cewh=%10.3f*n", cewn,ceww,cewh)
//writef("           celn=%10.3f  celw=%10.3f  celh=%10.3f*n", celn,celw,celh)

//newline()
//writef("            ctn=%10.3f   ctw=%10.3f   cth=%10.3f*n", ctn,ctw,cth)
//writef("            cwn=%10.3f   cww=%10.3f   cwh=%10.3f*n", cwn,cww,cwh)
//writef("            cln=%10.3f   clw=%10.3f   clh=%10.3f*n", cln,clw,clh)

//writef("Transposed*n")
//writef("            ctn=%10.3f   ctw=%10.3f   cth=%10.3f*n", ctn,cwn,cln)
//writef("            cwn=%10.3f   cww=%10.3f   cwh=%10.3f*n", ctw,cww,clw)
//writef("            cln=%10.3f   clw=%10.3f   clh=%10.3f*n", cth,cwh,clh)

//newline()
//writef("            m00=%10.3f   m01=%10.3f   m02=%10.3f*n", m00,m01,m02)
//writef("            m10=%10.3f   m11=%10.3f   m12=%10.3f*n", m10,m11,m12)
//writef("            m20=%10.3f   m21=%10.3f   m22=%10.3f*n", m20,m21,m22)

  // If the resulting (x,y) coordinate are not on the screen return FALSE
  UNLESS 0.0 <= sx < fscreenxsize &  
         0.0 <= sy < fscreenysize RESULTIS FALSE

  // A point screensize pixels away from the centre of the screen is
  // 45 degrees from the direction of view.
  // Note that many pixels in this range are off the screen.
  v!0 :=  sx
  v!1 :=  sy
  v!2 := -sz // This distance into the screen in arbitrary units, used
             // for hidden surface removal.
//newline()
//writef("        sizeby2=%10.3f    mx=%10.3f    my=%10.3f*n", sizeby2, mx, my)
//writef("            v!0=%10.3f   v!1=%10.3f   v!2=%10.3f*n", v!0, v!1, v!2)
//abort(1119)
  RESULTIS TRUE
}

AND gdrawquad3d(FLT x1, FLT y1, FLT z1,
                FLT x2, FLT y2, FLT z2,
                FLT x3, FLT y3, FLT z3,
                FLT x4, FLT y4, FLT z4) BE
{ cdrawquad3d(x1-cgn, y1-cgw, z1-cgh,
              x2-cgn, y2-cgw, z2-cgh,
              x3-cgn, y3-cgw, z3-cgh,
              x4-cgn, y4-cgw, z4-cgh)
}

AND gdrawtriangle3d(FLT x1, FLT y1, FLT z1,
                    FLT x2, FLT y2, FLT z2,
                    FLT x3, FLT y3, FLT z3) BE
{ cdrawtriangle3d(x1-cgn, y1-cgw, z1-cgh,
                  x2-cgn, y2-cgw, z2-cgh,
                  x3-cgn, y3-cgw, z3-cgh)
}

AND plotscreen() BE
{ fillsurf(maprgb(100,100,255))
  //seteyeposition()
  cleardepthscreen()


//ctn, ctw, cth := 1.0, 0.0, 0.0  // Aircraft orientation pointing due north
//cwn, cww, cwh := 0.0, 1.0, 0.0
//cln, clw, clh := 0.0, 0.0, 1.0

//cetn, cetw, ceth := 1.0, 0.0, 0.0  // Eye orientation pointing due north
//cewn, ceww, cewh := 0.0, 1.0, 0.0
//celn, celw, celh := 0.0, 0.0, 1.0

//cgn, cgw, cgh := 0, 0, 0

  plotcraft()
  plotland()
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
{ // This function draws the artificial horizon.

  // Using the ctn and ctw components of the direction of thrust t
  // we make a horizontal vector (ctn,ctw,0) and scaled it to be
  // of unit length. Suppose the resulting vector is d = (dn,dw,0),
  // It will be horizontal and in the same vertical plane as t.
  // Let P be a point 100 ft from the aircraft's CG in direction d,
  // ie (100dn, 100dw,0).
  // P (cgn+100dn,cgw+100dw,cgh) is in world coordinates.
  // The artificial horizon is made up of four line segments
  // A-B, B-C, C-D and D-E where A, B, D and E are on
  // the horizontal line passing through P at right angles to d.
  // A is 30ft to the left of P and E is 30ft to the right of P.
  // On the screen, B, C and D form an equilateral triangle half
  // way between A and E.

  // orthogonal projection as the points A, B, C, D
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
  An, Aw, Ah := Pn+30*a, Pw+30*b, Ph
  // Set A 30ft left of from P.
  En, Ew, Eh := Pn-30*a, Pw-30*b, Ph

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
  cx, cy := px-(ey-ay)*8_66/60_00, py+(ex-ax)*8_66/60_00
  // We can now draw the artificial horizon
  moveto(ax,ay)
  drawto(bx,by)
  drawto(cx,cy)
  drawto(dx,dy)
  drawto(ex,ey)

//updatescreen()
//abort(1001)
}

AND draw_ground_point(FLT x, FLT y) BE
{ LET FLT gx, FLT gy, FLT gz = Zro, Zro, Zro
//newline()
//writef("draw_ground_point: x=%13.2f y=%13.2f*n", x, y)
//writef("draw_ground_point: cgn=%13.2f cgw=%13.2f cgh=%13.2f*n", cgn, cgw, cgh)
//abort(1001)
  IF screencoords(x-cgn, y-cgw, -cgh-pilotl, @gx) DO
  { 
//writef("gx=%13.3f  gy=%13.3f gz=%13.3f*n", gx, gy, gz)
    drawrect(FIX gx, FIX gy, 2 + FIX gx, 2 + FIX gy)
    //updatescreen()
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

AND seteyeposition() BE
{ cetn, cetw, ceth :=  One, Zro, Zro
  cewn, ceww, cewh :=  Zro, One, Zro
  celn, celw, celh :=  Zro, Zro, One

  IF FALSE DO
  { // Debugging eye orientation and distance
    cetn, cetw, ceth :=  One, Zro, Zro
    cewn, ceww, cewh :=  Zro, One, Zro
    celn, celw, celh :=  Zro, Zro, One
    eyedist := 20.0
    RETURN
  }

UNLESS 0<=eyedir<=8 DO eyedir := 1

  IF hatdir & sdlmsecs()>hatmsecs+100 DO
  { eyedir := 0 //FIX((angle(ctn, ctw)+360.0+22.5) / 45.0) & 7
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

//writef("ctn=%10.3f ctw=%10.3f eyedir=%9.1f*n", ctn, ctw, eyedir)
//abort(1009) 
  }

  SWITCHON eyedir INTO
  { DEFAULT:

    CASE 0: // Pilot's view
      cetn, cetw, ceth :=   ctn, ctw, cth
      cewn, ceww, cewh :=   cwn, cww, cwh
      celn, celw, celh :=   cln, clw, clh
      RETURN

     CASE 1: // Looking North
       cetn, cetw, ceth :=  One, Zro, Zro
       cewn, ceww, cewh :=  Zro, One, Zro
       ENDCASE

     CASE 2: // North east
       cetn, cetw, ceth :=  D45, D45, Zro
       cewn, ceww, cewh := -D45, D45, Zro
       ENDCASE

     CASE 3: // East
       cetn, cetw, ceth :=  Zro, One, Zro
       cewn, ceww, cewh := -One, Zro, Zro
       ENDCASE

     CASE 4: // South east
       cetn, cetw, ceth := -D45, D45, Zro
       cewn, ceww, cewh := -D45,-D45, Zro
       ENDCASE

     CASE 5: // South
       cetn, cetw, ceth := -One,  Zro, Zro
       cewn, ceww, cewh :=  Zro, -One, Zro
       ENDCASE

     CASE 6: // South west
       cetn, cetw, ceth := -D45,-D45, Zro
       cewn, ceww, cewh :=  D45,-D45, Zro
       ENDCASE

     CASE 7: // West
       cetn, cetw, ceth :=  Zro,-One, Zro
       cewn, ceww, cewh :=  One, Zro, Zro
       ENDCASE

     CASE 8: // North west
       cetn, cetw, ceth := D45,-D45, Zro
       cewn, ceww, cewh := D45, D45, Zro
       ENDCASE
  }

  // make the eye look slightly down
  ceth := ceth - eyeheight
  standardize(@cetn)
  crossprod(@cetn, @cewn, @celn)
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ DEFAULT:
    LOOP

  CASE sdle_keydown:
    SWITCHON capitalch(eventa2) INTO
    { DEFAULT:  LOOP

      CASE 'Q': done := TRUE
                LOOP

      CASE 'P': // Toggle stepping
                stepping := ~stepping
                LOOP

      CASE 'G': // Toggle stepping
                geardown := ~geardown
                LOOP

      CASE 'D': // Toggle stepping
                debugging := ~debugging
                LOOP

      CASE 'U': // Adjust eye height
                TEST eventa2='u' THEN eyeheight := eyeheight - 0.1
                                 ELSE eyeheight := eyeheight + 0.1
                LOOP

      CASE 'S': // Toggle engin started
                enginestarted := ~enginestarted
                LOOP 

      CASE 'N': // Reduce eye distance
                eyedist := eyedist*5 / 6
                IF eyedist<5.0 DO eyedist := 5.0
                seteyeposition()
                LOOP

      CASE 'F': // Increase eye distance
                eyedist := eyedist * 6 / 5
                seteyeposition()
                LOOP

      CASE 'Z': c_trimthrottle := c_trimthrottle - 0.02
                throttle := c_trimthrottle+c_throttle
                IF throttle < 0.0 DO throttle, c_trimthrottle := 0.0, -c_throttle
                LOOP

      CASE 'X': c_trimthrottle := c_trimthrottle + 0.02
                throttle := c_trimthrottle+c_throttle
                IF throttle > 1.0 DO throttle, c_trimthrottle := 1.0, 1.0-c_throttle
                LOOP

      CASE ',':
      CASE '<': c_trimrudder := c_trimrudder - 0.050
                rudder := c_trimrudder+c_rudder
                IF rudder < -1.0 DO rudder, c_trimrudder := -1.0, -1.0-c_rudder
//writef("Trim rudder left  %6.3f*n", rudder)
                LOOP

      CASE '.':
      CASE '>': c_trimrudder := c_trimrudder + 0.050
                rudder := c_trimrudder+c_rudder
                IF rudder > 1.0 DO rudder, c_trimrudder := 1.0, 1.0-c_rudder
//writef("Trim rudder right %6.3f*n", rudder)
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

      CASE 'T': initposition(1) // Set take off position
                LOOP

      CASE 'A': initposition(2) // Set final approach position
                LOOP

      CASE 'L': initposition(3) // Set level flight at 10000 ft.
                LOOP

      CASE 'I': initposition(4) // Set nearly inverted at 10000 ft.
                LOOP

      CASE 'W': initposition(5) // Set Level flight a 1ft to test wheels.
                LOOP

      CASE sdle_arrowup:
                c_trimelevator := c_trimelevator + 0.020
                elevator := c_trimelevator+c_elevator
                IF elevator > 1.0 DO elevator, c_trimelevator := 1.0, 1.0-c_elevator
                LOOP

      CASE sdle_arrowdown:
                c_trimelevator := c_trimelevator - 0.020
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
//writef("eyedist=%10.3f*n", eyedist)
              LOOP
      CASE 12:     // Increase eye distance
              eyedist := eyedist*6/5
              IF eyedist > 1000.0 DO eyedist := 1000.0
//writef("eyedist=%10.3f*n", eyedist)
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

AND initposition(n) BE SWITCHON n INTO
{ DEFAULT:

  CASE 1: // Take off position
    cgn,    cgw,    cgh    :=  100.0,    0,  2.185
    cgndot, cgwdot, cghdot :=   10.0,  Zro,  0.0
    tdot,   wdot,   ldot   := cgndot,  0.0,  0.0

    // The aircraft orientation -- level due north
    ctn, ctw, cth :=   One, Zro, -0.001  // Direction cosines of aircraft
    cwn, cww, cwh :=   Zro, One,    Zro
    cln, clw, clh := 0.001, Zro,    One

    stepping := TRUE
    crashed := FALSE
    enginestarted, rpm := FALSE, 0.0
    targetrpm := rpm
    thrust := 0.0
    RETURN

  CASE 2: // Position on the glide slope -- level due north
    cgn,    cgw,    cgh    := -10000.0, Zro,  1000.0    // Height of 1000 ft
    cgndot, cgwdot, cghdot :=     95.0, Zro,     Zro
    tdot,   wdot,   ldot   :=   cgndot, 0.0,     0.0

    // The aircraft orientation
    ctn, ctw, cth := One, Zro, Zro  // Direction cosines with
    cwn, cww, cwh := Zro, One, Zro  // six decimal digits
    cln, clw, clh := Zro, Zro, One  // after to decimal point.

    tdot, wdot, ldot := 95.0, 0.0, 0.0

    stepping := TRUE
    crashed := FALSE
    throttle := 0.5
    enginestarted, targetrpm := TRUE, 1600.0
    rpm := targetrpm
    thrust := 0.0
    RETURN

  CASE 3: // Set flying level at 10000 ft at 65mph -- level due north
    cgn,    cgw,    cgh    := -20000.0, Zro, 10000.0    // Height of 10000 ft
    cgndot, cgwdot, cghdot :=     95.0, Zro,   Zro     // 65mph = 95 ft/s
    tdot,   wdot,   ldot   :=   cgndot, 0.0,    0.0

    // The aircraft orientation
    ctn, ctw, cth := One, Zro, Zro  // Direction cosines of aircraft.
    cwn, cww, cwh := Zro, One, Zro
    cln, clw, clh := Zro, Zro, One

    //{ ctn, ctw, cth := One, Zro, Zro  // Debugging values
    //  cwn, cww, cwh := Zro, One, Zro
    //  cln, clw, clh := Zro, Zro, One
    //}

    stepping := TRUE
    crashed := FALSE
    throttle := 0.5
    enginestarted, rpm := TRUE, 1900.0
    targetrpm := rpm
    thrust := 0.0
    RETURN

  CASE 4: // Nearly inverted at 10000 ft to test stability.
    cgn,    cgw,    cgh    := -20000.0, Zro, 10000.0    // Height of 10000 ft
    cgndot, cgwdot, cghdot :=     95.0, Zro,   Zro     // 65mph = 95 ft/s
    tdot,   wdot,   ldot   :=   cgndot, 0.0,    0.0

    // The aircraft orientation
    ctn, ctw, cth := 1.0,-D45, Zro  // Direction cosines of aircraft.
    cwn, cww, cwh := D45, Zro, D45
    cln, clw, clh := Zro,-D45, 1.0

    stepping := TRUE
    crashed := FALSE
    throttle := 0.5
    enginestarted, rpm := TRUE, 1900.0
    targetrpm := rpm
    thrust := 0.0
    RETURN

  CASE 5: // Level flight at 1 ft to test wheels.
    cgn,    cgw,    cgh    :=  100.0,    0,  2.178-0.666
    cgndot, cgwdot, cghdot :=   10.0,  Zro,  0.0
    tdot,   wdot,   ldot   := cgndot,  0.0,  0.0

    // The aircraft orientation -- level due north
    ctn, ctw, cth :=   One, Zro,    Zro  // Direction cosines of aircraft
    cwn, cww, cwh :=   Zro, One,    Zro
    cln, clw, clh := 0.001, Zro,    One

    stepping := TRUE
    crashed := FALSE
    enginestarted, rpm := FALSE, 0.0
    targetrpm := rpm
    thrust := 0.0
    RETURN
}

LET start() = VALOF
{ LET v = VEC 2
  datstamp(v)
  msecs1 := v!1 
  One := 1.0
  Zro := 0.0
  stepcount := 0
  steprate := 20.0
  crashed := FALSE
  geardown := TRUE
  debugging := TRUE
  enginestarted := FALSE

  c_throttle, c_elevator, c_aileron, c_rudder := Zro, Zro, Zro, Zro
  c_trimthrottle, c_trimelevator, c_trimaileron, c_trimrudder := Zro, Zro, Zro, Zro
  throttle, elevator, aileron, rudder := Zro, Zro, Zro, Zro

  //initposition(1) // Get ready for take off
  initposition(3)   // Set flying level at 10000 ft at 65mph
  //initposition(4)   // Nearly inverted at 10000 ft
  //initposition(5)   // level at 1 ft.

  cetn, cetw, ceth := ctn, ctw, cth
  cewn, ceww, cewh := cwn, cww, cwh
  celn, celw, celh := cln, clw, clh

  hatdir, hatmsecs := #b0001, 0 // From behind
  eyedir := 1
  eyedist := 20.0   // Distance  from the eye to the aircraft.
  eyeheight := 0.2  // Eye height above cgh
  seteyeposition()

  pilotl := 6.0   // Pilot's eye level.

  ft, fw, fl := Zro, Zro, Zro

  usage := 0

  initsdl()
  mkscreen("SDL Aircraft", 800, 500)

  fscreenxsize, fscreenysize := FLOAT screenxsize, FLOAT screenysize

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

  done := FALSE

  UNTIL done DO
  { // Read joystick and keyboard events
    LET t0 = sdlmsecs()
    LET t1 = ?
    IF stepcount MOD 20 = 0 DO steprate := 20.0
IF steprate<20.0 DO steprate := 20.0

    processevents()
    FOR i = 1 TO 5 IF stepping UNLESS crashed DO step()
    plotscreen()
    drawcontrols()
    draw_artificial_horizon()
    updatescreen()
//abort(6677)
    //IF FALSE DO
    IF stepcount MOD 20 = 0 DO
    { LET prevmsecs = msecs1
      LET v = VEC 2
      LET s = VEC 15
      datstamp(v)
      msecs1 := v!1
      datstring(s)
      steprate := 20 * 1000 / FLOAT(msecs1 - prevmsecs)
//      writef("stepcount=%n msecs diff=%i5 steprate = %6.3f %s*n",
//              stepcount, msecs1-prevmsecs, steprate, s+5)
    }

    t1 := sdlmsecs()
//writef("time %10.3d  %10.3d  %10.3d %10.3d*n", t0, t1, t1-t0, t0+100-t1)
    usage := 100*(t1-t0)/100

    ///IF t0+100 < t1 DO sdldelay(t0+100-t1)
    //sdldelay(100)
    sdldelay(20)
//abort(1120)
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

  setcolour(col_lightcyan)
  
  drawstring(240, 50, done -> "Quitting", "Simple Flight Simulator")

  setcolour(col_lightgray) // Draw runway line
  moveto(mx-1, my)
  drawby(0, FIX(3000.0/100.0))
  moveto(mx,   my)
  drawby(0, FIX(3000.0/100.0))
  moveto(mx+1, my)
  drawby(0, FIX(3000.0/100.0))

  { LET dx  = FIX(ctn*20)    // Orientation of the aircraft
    LET dy  = FIX(ctw*20)
    LET sdx = dx / 10        // Ground speed of the aircraft
    LET sdy = dy / 10
    LET x   = mx-FIX(cgw/100)
    LET y   = my+FIX(cgn/100)
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
    plotf(mx-50, my+50, "CRASHED")
  }

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
    plotf(20, my+15,  "rpm=%i4", FIX rpm)
    plotf(20, my,     "Throttle=%6.3f Elevator=%6.3f Aileron=%6.3f Rudder=%6.3f",
                       throttle,      elevator,      aileron,      rudder)
    plotf(20, my- 15, "cgn=    %13.3f cgw=   %13.3f cgh=   %13.3f", cgn,   cgw,   cgh)
    plotf(20, my- 30, "cgndot= %13.3f cgwdot=%13.3f cghdot=%13.3f", cgndot,cgwdot,cghdot)
    plotf(20, my- 45, "tdot=   %13.3f wdot=  %13.3f ldot=  %13.3f", tdot,  wdot,  ldot)
    plotf(20, my- 60, "ctn= %7.3f ctw= %7.3f cth= %7.3f", ctn,   ctw,   cth)
    plotf(20, my- 75, "cwn= %7.3f cww= %7.3f cwh= %7.3f", cwn,   cww,   cwh)
    plotf(20, my- 90, "cln= %7.3f clw= %7.3f clh= %7.3f", cln,   clw,   clh)
    plotf(20, my-105, "ft=     %13.3f fw=    %13.3f fl=    %13.3f", ft,    fw,    fl)
    plotf(20, my-150, "steprate=%8.3f", steprate)

    plotf(20, my-200, "cetn= %7.3f cewn= %7.3f celn= %7.3f", cetn, cewn, celn)
    plotf(20, my-215, "cetw= %7.3f ceww= %7.3f celw= %7.3f", cetw, ceww, celw)
    plotf(20, my-230, "ceth= %7.3f cewh= %7.3f celh= %7.3f", ceth, cewh, celh)

    plotf(20, my-260, "eyedist= %7.3f", eyedist)

  }

  { LET heading = - FIX (angle(ctn,ctw))
    IF heading < 0 DO heading := 360 + heading
    plotf(20, 5, "RPM %4i Thrust %4i Speed %3i mph  Altiude %i5 ft  Heading %i3 Usage %3i%%",
//          FIX rpm, FIX thrust, FIX (tdot*fps2mph), FIX cgh, heading, usage)
          FIX rpm, FIX thrust, FIX (fps2mph*tdot), FIX cgh, heading, usage)
  }
//updatescreen()
}



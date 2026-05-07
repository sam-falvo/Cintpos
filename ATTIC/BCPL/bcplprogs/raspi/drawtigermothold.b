/*
This defined the function drawtegermoth() used by draw3d.b and tiger.b

Implemented by Martin Richards (c) January 2013

History

12/03/2018
Modified to use floaring point and the FLT feature.


The origin is near the centre of gravity
The actual centre of gravity is at
         actualcgt, actualcgw, actualcgl
typically  -0.500  0.0  0.0     ie behind the wheels but forward of the
                                   centre of lift of the wings.

The coordinates of the lowest points of the left and right hand wheels are:

                 t       w       l
               -0.268,  2.100, -3.800-0.700,
and            -0.268, -2.100, -3.800-0.700,

The coordinates of the tail skid are:

                 t       w       l
              -16.500,  0.0,   -0.900,

These are used to calculate the undercarriage forces when the aircraft
touched the ground.

Left  wings centre of lift:    t       w      l
                             -2.600 -10.000  0.0  
Right wings centre of lift:    t       w      l
                             -2.600 -10.000  0.0  

Weight:                        1100 lbs
Moment of inertia about t: 10 *  50 ft lbs    estimated values
                        w: 10 * 150 ft lbs
                        l: 10 * 250 ft lbs

Assume a force of 1 lb acting on a mass of 1 lb causes an 
accelleration of 32 ft per second per second.

Assume a moment of 1 ft lb acting on a structure with moment of
inertia 1 ft lb for one second would change the rate of rotation 32
radians per second.

*/

LET drawtigermoth() BE
{
  // Cockpit floor
  setcolour(maprgb(90,80,30))
  cdrawquad3d (1.000, 0.800, 0.000,
               1.000,-0.800, 0.000,
              -5.800,-0.800, 0.000,
              -5.800, 0.800, 0.000)

  // Left lower wing
  setcolour(maprgb(165,165,30))        // Under surface

  cdrawquad3d(-0.500,  1.000, -2.000,  // Panel A
              -3.767,  1.000, -2.218,
              -4.396,  6.000, -1.745, 
              -1.129,  6.000, -1.527)

  cdrawquad3d(-3.767,  1.000, -2.218,  // Panel B
              -4.917,  1.000, -2.294,
              -5.546,  6.000, -1.821,
              -4.396,  6.000, -1.745) 

  cdrawquad3d(-1.129,  6.000, -1.527,  // Panel C
              -4.396,  6.000, -1.745,
              -5.147, 14.166, -1.179,
              -1.880, 14.166, -0.961)

  { // Aileron deflection 1 inch from hinge
    LET a = 0.600 * c.aileron / 17

    setcolour(maprgb(155,155,20))            // Under surface
    cdrawquad3d(-4.396,      6.000, -1.745,  // Panel D Aileron
                -5.546+3*a,  6.000, -1.821-14*a,
                -6.297+3*a, 13.766, -1.255-14*a,
                -5.147,     14.166, -1.179)
  }

  // Left lower wing upper surface
  setcolour(maprgb(120,140,60))

  cdrawquad3d(-0.500,  1.000, -2.000,  // Panel A1
              -1.500,  1.000, -1.800,
              -2.129,  6.000, -1.327, 
              -1.129,  6.000, -1.527)

  setcolour(maprgb(120,130,50))
  cdrawquad3d(-1.500,  1.000, -1.800,  // Panel A2
              -3.767,  1.000, -2.118,
              -4.396,  6.000, -1.645, 
              -2.129,  6.000, -1.327)

  cdrawquad3d(-3.767,  1.000, -2.118,  // Panel B
              -4.917,  1.000, -2.294,
              -5.546,  6.000, -1.821,
              -4.396,  6.000, -1.645) 

  setcolour(maprgb(120,140,60))
  cdrawquad3d(-1.129,  6.000, -1.527,  // Panel C1
              -2.129,  6.000, -1.327,
              -2.880, 14.166, -0.761,
              -1.880, 14.166, -0.961)

  setcolour(maprgb(120,130,50))
  cdrawquad3d(-2.129,  6.000, -1.327,  // Panel C2
              -4.396,  6.000, -1.645,
              -5.147, 14.166, -1.079,
              -2.880, 14.166, -0.761)

  { // Aileron deflection 1 inch from hinge
    LET a = 0.600 * c.aileron / 17

    setcolour(maprgb(120,140,60))
    cdrawquad3d(-4.396,      6.000, -1.645,  // Panel D Aileron
                -5.546+3*a,  6.000, -1.821-14*a,
                -6.297+3*a, 13.766, -1.255-14*a,
                -5.147,     14.166, -0.979)
  }

  // Left lower wing tip
  setcolour(maprgb(130,150,60))
  cdrawtriangle3d(-1.880, 14.167,-1.006,
                  -2.880, 14.167,-0.761,
                  -3.880, 14.467,-0.980)
  setcolour(maprgb(130,150,60))
  cdrawtriangle3d(-2.880, 14.167,-0.761,
                  -5.147, 14.167,-1.079,
                  -3.880, 14.467,-0.980)
  setcolour(maprgb(160,160,40))
  cdrawtriangle3d(-5.147, 14.167,-1.079,
                  -5.147, 14.167,-1.179,
                  -3.880, 14.467,-0.980)
  setcolour(maprgb(170,170,50))
  cdrawtriangle3d(-5.147, 14.167,-1.179,
                  -1.880, 14.167,-0.961,
                  -3.880, 14.467,-0.980)

  // Right lower wing
  setcolour(maprgb(165,165,30))        // Under surface

  cdrawquad3d(-0.500, -1.000, -2.000,  // Panel A
              -3.767, -1.000, -2.218,
              -4.396, -6.000, -1.745, 
              -1.129, -6.000, -1.527)

  cdrawquad3d(-3.767, -1.000, -2.218,  // Panel B
              -4.917, -1.000, -2.294,
              -5.546, -6.000, -1.821,
              -4.396, -6.000, -1.745) 

  cdrawquad3d(-1.129, -6.000, -1.527,  // Panel C
              -4.396, -6.000, -1.745,
              -5.147,-14.166, -1.179,
              -1.880,-14.166, -0.961)

  { // Aileron deflection 1 inch from hinge
    LET a = 0.600 * c.aileron / 17

    setcolour(maprgb(155,155,20))            // Under surface
    cdrawquad3d(-4.396,     -6.000, -1.745,  // Panel D Aileron
                -5.546+3*a, -6.000, -1.821+14*a,
                -6.297+3*a,-13.766, -1.255+14*a,
                -5.147,    -14.166, -1.179)
  }

  // Right lower wing upper surface
  setcolour(maprgb(120,140,60))

  cdrawquad3d(-0.500, -1.000, -2.000,  // Panel A1
              -1.500, -1.000, -1.800,
              -2.129, -6.000, -1.327, 
              -1.129, -6.000, -1.527)

  setcolour(maprgb(120,130,50))
  cdrawquad3d(-1.500, -1.000, -1.800,  // Panel A2
              -3.767, -1.000, -2.118,
              -4.396, -6.000, -1.645, 
              -2.129, -6.000, -1.327)

  cdrawquad3d(-3.767, -1.000, -2.118,  // Panel B
              -4.917, -1.000, -2.294,
              -5.546, -6.000, -1.821,
              -4.396, -6.000, -1.645) 

  setcolour(maprgb(120,140,60))
  cdrawquad3d(-1.129, -6.000, -1.527,  // Panel C1
              -2.129, -6.000, -1.327,
              -2.880,-14.166, -0.761,
              -1.880,-14.166, -0.961)

  setcolour(maprgb(120,130,50))
  cdrawquad3d(-2.129, -6.000, -1.327,  // Panel C2
              -4.396, -6.000, -1.645,
              -5.147,-14.166, -1.079,
              -2.880,-14.166, -0.761)

  { // Aileron deflection 1 inch from hinge
    LET a = 0.600 * c.aileron / 17

    setcolour(maprgb(120,140,60))
    cdrawquad3d(-4.396,     -6.000, -1.645,  // Panel D Aileron
                -5.546+3*a, -6.000, -1.821+14*a,
                -6.297+3*a,-13.766, -1.255+14*a,
                -5.147,    -14.166, -0.979)
  }

  // Right lower wing tip
  setcolour(maprgb(130,150,60))
  cdrawtriangle3d(-1.880,-14.167,-1.006,
                  -2.880,-14.167,-0.761,
                  -3.880,-14.467,-0.980)
  setcolour(maprgb(130,150,60))
  cdrawtriangle3d(-2.880,-14.167,-0.761,
                  -5.147,-14.167,-1.079,
                  -3.880,-14.467,-0.980)
  setcolour(maprgb(160,160,40))
  cdrawtriangle3d(-5.147,-14.167,-1.079,
                  -5.147,-14.167,-1.179,
                  -3.880,-14.467,-0.980)
  setcolour(maprgb(170,170,50))
  cdrawtriangle3d(-5.147,-14.167,-1.179,
                  -1.880,-14.167,-0.961,
                  -3.880,-14.467,-0.980)

  // Left upper wing
  setcolour(maprgb(200,200,30))       // Under surface
  cdrawquad3d( 1.333,  1.000,  2.900,
              -1.967,  1.000,  2.671,
              -3.297, 14.167,  3.671, 
               0.003, 14.167,  3.894)
  cdrawquad3d(-1.967,  1.000,  2.671,
              -3.084,  2.200,  2.606,
              -4.414, 13.767,  3.645, 
              -3.297, 14.167,  3.671)

  setcolour(maprgb(150,170,90))       // Top surface
  cdrawquad3d( 1.333,  1.000,  2.900, // Panel A1
               0.333,  1.000,  3.100,
              -0.997, 14.167,  4.094, 
               0.003, 14.167,  3.894)

  setcolour(maprgb(140,160,80))       // Top surface
  cdrawquad3d( 0.333,  1.000,  3.100, // Panel A2
              -1.967,  1.000,  2.771,
              -3.297, 14.167,  3.771, 
              -0.997, 14.167,  4.094)

  setcolour(maprgb(150,170,90))       // Top surface
  cdrawquad3d(-1.967,  1.000,  2.771, // Panel B
              -3.084,  2.200,  2.606,
              -4.414, 13.767,  3.645, 
              -3.297, 14.167,  3.771)

  // Left upper wing tip
  setcolour(maprgb(130,150,60))
  cdrawtriangle3d( 0.003, 14.167, 3.894,
                  -0.997, 14.167, 4.094,
                  -1.997, 14.467, 3.874)
  setcolour(maprgb(130,150,60))
  cdrawtriangle3d(-0.997, 14.167, 4.094,
                  -3.297, 14.167, 3.771,
                  -1.997, 14.467, 3.874)
  setcolour(maprgb(160,160,40))
  cdrawtriangle3d(-3.297, 14.167, 3.771,
                  -3.297, 14.167, 3.671,
                  -1.997, 14.467, 3.874)
  setcolour(maprgb(170,170,50))
  cdrawtriangle3d(-3.297, 14.167, 3.671,
                   0.003, 14.167, 3.894,
                  -1.997, 14.467, 3.874)


  // Right upper wing
  setcolour(maprgb(200,200,30))       // Under surface
  cdrawquad3d( 1.333, -1.000,  2.900,
              -1.967, -1.000,  2.671,
              -3.297,-14.167,  3.671, 
               0.003,-14.167,  3.894)
  cdrawquad3d(-1.967, -1.000,  2.671,
              -3.084, -2.200,  2.606,
              -4.414,-13.767,  3.645, 
              -3.297,-14.167,  3.671)

  setcolour(maprgb(150,170,90))       // Top surface
  cdrawquad3d( 1.333, -1.000,  2.900, // Panel A1
               0.333, -1.000,  3.100,
              -0.997,-14.167,  4.094, 
               0.003,-14.167,  3.894)

  setcolour(maprgb(140,160,80))       // Top surface
  cdrawquad3d( 0.333, -1.000,  3.100, // Panel A2
              -1.967, -1.000,  2.771,
              -3.297,-14.167,  3.771, 
              -0.997,-14.167,  4.094)

  setcolour(maprgb(150,170,90))       // Top surface
  cdrawquad3d(-1.967, -1.000,  2.771, // Panel B
              -3.084, -2.200,  2.606,
              -4.414,-13.767,  3.645, 
              -3.297,-14.167,  3.771)

  // Right upper wing tip
  setcolour(maprgb(130,150,60))
  cdrawtriangle3d( 0.003,-14.167, 3.894,
                  -0.997,-14.167, 4.094,
                  -1.997,-14.467, 3.874)
  setcolour(maprgb(130,150,60))
  cdrawtriangle3d(-0.997,-14.167, 4.094,
                  -3.297,-14.167, 3.771,
                  -1.997,-14.467, 3.874)
  setcolour(maprgb(160,160,40))
  cdrawtriangle3d(-3.297,-14.167, 3.771,
                  -3.297,-14.167, 3.671,
                  -1.997,-14.467, 3.874)
  setcolour(maprgb(170,170,50))
  cdrawtriangle3d(-3.297,-14.167, 3.671,
                   0.003,-14.167, 3.894,
                  -1.997,-14.467, 3.874)


  // Wing root strut forward left
  setcolour(maprgb(80,80,80))
  cdrawquad3d(  0.433,  0.950, 2.900,
                0.633,  0.950, 2.900,
                0.633,  1.000, 0.000,
                0.433,  1.000, 0.000)

  // Wing root strut rear left
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -1.967,  0.950,  2.616,
               -1.767,  0.950,  2.616,
               -0.868,  1.000,  0.000,
               -1.068,  1.000,  0.000)

  // Wing root strut diag left
  setcolour(maprgb(80,80,80))
  cdrawquad3d(  0.433,  0.950, 2.900,
                0.633,  0.950, 2.900,
               -0.868,  1.000, 0.000,
               -1.068,  1.000, 0.000)

  // Wing root strut forward right
  setcolour(maprgb(80,80,80))
  cdrawquad3d(  0.433, -0.950, 2.900,
                0.633, -0.950, 2.900,
                0.633, -1.000, 0.000,
                0.433, -1.000, 0.000)

  // Wing root strut rear right
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -1.967, -0.950,  2.616,
               -1.767, -0.950,  2.616,
               -0.868, -1.000,  0.000,
               -1.068, -1.000,  0.000)

  // Wing root strut diag right
  setcolour(maprgb(80,80,80))
  cdrawquad3d(  0.433, -0.950, 2.900,
                0.633, -0.950, 2.900,
               -0.868, -1.000, 0.000,
               -1.068, -1.000, 0.000)

  // Wing strut forward left
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -2.200,  10.000, -1.120,
               -2.450,  10.000, -1.120,
               -0.550,  10.000,  3.315,
               -0.300,  10.000,  3.315)

  // Wing strut rear left
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -4.500,  10.000, -1.260,
               -4.750,  10.000, -1.260,
               -2.850,  10.000,  3.210,
               -2.500,  10.000,  3.210)

  // Wing strut forward right
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -2.200, -10.000, -1.120,
               -2.450, -10.000, -1.120,
               -0.550, -10.000,  3.315,
               -0.300, -10.000,  3.315)

  // Wing strut rear right
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -4.500, -10.000, -1.260,
               -4.750, -10.000, -1.260,
               -2.850, -10.000,  3.210,
               -2.500, -10.000,  3.210)

  // Wheel strut left
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -0.768,  1.000, -2.000,
               -1.168,  1.000, -2.000,
               -0.468,  2.000, -3.800,
               -0.068,  2.000, -3.800)

  // Wheel strut diag left
  setcolour(maprgb(80,80,80))
  cdrawquad3d(  1.600,  1.000, -2.000,
                1.800,  1.000, -2.000,
               -0.368,  2.000, -3.800,
               -0.168,  2.000, -3.800)

  // Wheel strut centre left
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -0.500,  0.000, -2.900,
               -0.650,  0.000, -2.900,
               -0.318,  2.000, -3.800,
               -0.168,  2.000, -3.800)

  // Wheel strut right
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -0.768, -1.000, -2.000,
               -1.168, -1.000, -2.000,
               -0.468, -2.000, -3.800,
               -0.068, -2.000, -3.800)

  // Wheel strut diag right
  setcolour(maprgb(80,80,80))
  cdrawquad3d(  1.600, -1.000, -2.000,
                1.800, -1.000, -2.000,
               -0.368, -2.000, -3.800,
               -0.168, -2.000, -3.800)

  // Wheel strut centre right
  setcolour(maprgb(80,80,80))
  cdrawquad3d( -0.500, -0.000, -2.900,
               -0.650, -0.000, -2.900,
               -0.318, -2.000, -3.800,
               -0.168, -2.000, -3.800)


  // Left wheel
  setcolour(maprgb(20,20,20))
  cdrawquad3d( -0.268,       2.100, -3.800,
               -0.268,       2.100, -3.800-0.700,
               -0.268-0.500, 2.100, -3.800-0.500,
               -0.268-0.700, 2.100, -3.800)
  cdrawquad3d( -0.268,       2.100, -3.800,
               -0.268,       2.100, -3.800-0.700,
               -0.268+0.500, 2.100, -3.800-0.500,
               -0.268+0.700, 2.100, -3.800)
  cdrawquad3d( -0.268,       2.100, -3.800,
               -0.268,       2.100, -3.800+0.700,
               -0.268-0.500, 2.100, -3.800+0.500,
               -0.268-0.700, 2.100, -3.800)
  cdrawquad3d( -0.268,       2.100, -3.800,
               -0.268,       2.100, -3.800+0.700,
               -0.268+0.500, 2.100, -3.800+0.500,
               -0.268+0.700, 2.100, -3.800)

  // Right wheel
  setcolour(maprgb(20,20,20))
  cdrawquad3d( -0.268,      -2.100, -3.800,
               -0.268,      -2.100, -3.800-0.700,
               -0.268-0.500,-2.100, -3.800-0.500,
               -0.268-0.700,-2.100, -3.800)
  cdrawquad3d( -0.268,      -2.100, -3.800,
               -0.268,      -2.100, -3.800-0.700,
               -0.268+0.500,-2.100, -3.800-0.500,
               -0.268+0.700,-2.100, -3.800)
  cdrawquad3d( -0.268,      -2.100, -3.800,
               -0.268,      -2.100, -3.800+0.700,
               -0.268-0.500,-2.100, -3.800+0.500,
               -0.268-0.700,-2.100, -3.800)
  cdrawquad3d( -0.268,      -2.100, -3.800,
               -0.268,      -2.100, -3.800+0.700,
               -0.268+0.500,-2.100, -3.800+0.500,
               -0.268+0.700,-2.100, -3.800)


  // Fueltank front
  setcolour(maprgb(200,200,230))       // Top surface
  cdrawquad3d( 1.333,  1.000,  2.900,
               1.333, -1.000,  2.900,
               0.033, -1.000,  3.100,
               0.033,  1.000,  3.100)

  // Fueltank back
  setcolour(maprgb(180,180,210))       // Top surface
  cdrawquad3d( 0.033,  1.000,  3.100,
               0.033, -1.000,  3.100,
              -1.967, -1.000,  2.616,
              -1.967,  1.000,  2.616)

  // Fueltank left side
  setcolour(maprgb(160,160,190))
  cdrawtriangle3d( 1.333,  1.000, 2.900,
                   0.033,  1.000, 3.100,
                  -1.967,  1.000, 2.616)

  // Fueltank right side
  setcolour(maprgb(160,160,190))
  cdrawtriangle3d(-0.500+1.833, -1.000, -2.000+4.900,
                  -1.800+1.833, -1.000, -1.800+4.900,
                  -3.800+1.833, -1.000, -2.284+4.900) 

  // Fuselage

  // Prop shaft
  setcolour(maprgb(40,40,90))
  cdrawtriangle3d( 5.500, 0.000,  0.000,
                   4.700, 0.200, 0.300,
                   4.700, 0.200,-0.300)
  setcolour(maprgb(60,60,40))
  cdrawtriangle3d( 5.500, 0.000,  0.000,
                   4.700, 0.200,-0.300,
                   4.700,-0.200,-0.300)
  setcolour(maprgb(40,40,90))
  cdrawtriangle3d( 5.500, 0.000,  0.000,
                   4.700,-0.200,-0.300,
                   4.700,-0.200, 0.300)
  setcolour(maprgb(60,60,40))
  cdrawtriangle3d( 5.500, 0.000,  0.000,
                   4.700,-0.200, 0.300,
                   4.700, 0.200, 0.300)


  // Engine front lower centre
  setcolour(maprgb(140,140,160))
  cdrawtriangle3d( 5.000, 0.000,  0.000,
                   4.500, 0.550, -1.750,
                   4.500,-0.550, -1.750)

  // Engine front lower left
  setcolour(maprgb(140,120,130))
  cdrawtriangle3d( 5.000, 0.000,  0.000,
                   4.500, 0.550, -1.750,
                   4.500, 0.550,  0.000)

  // Engine front lower right
  setcolour(maprgb(140,120,130))
  cdrawtriangle3d( 5.000, 0.000,  0.000,
                   4.500,-0.550, -1.750,
                   4.500,-0.550,  0.000)

  // Engine front upper centre
  setcolour(maprgb(140,140,160))
  cdrawtriangle3d( 5.000, 0.000, 0.000,
                   4.500, 0.550, 0.500,
                   4.500,-0.550, 0.500)

  // Engine front upper left
  setcolour(maprgb(100,140,180))
  cdrawtriangle3d( 5.000, 0.000, 0.000,
                   4.500, 0.550, 0.500,
                   4.500, 0.550, 0.000)
  cdrawtriangle3d( 5.000, 0.000, 0.000,
                   4.500,-0.550, 0.500,
                   4.500,-0.550, 0.000)

  // Engine left lower
  setcolour(maprgb(80,80,60))
  cdrawquad3d( 1.033, 1.000,  0.000,
               1.800, 1.000, -2.000,
               4.500, 0.550, -1.750,
               4.500, 0.550,  0.000)

  // Engine right lower
  setcolour(maprgb(80,100,60))
  cdrawquad3d( 1.033,-1.000,  0.000,
               1.800,-1.000, -2.000,
               4.500,-0.550, -1.750,
               4.500,-0.550,  0.000)

  // Engine top left
  setcolour(maprgb(100,130,60))
  cdrawquad3d(  1.033, 0.900,  0.950,
                1.033, 0.900,  0.000,
                4.500, 0.550,  0.000,
                4.500, 0.550,  0.500)

  // Engine top centre
  setcolour(maprgb(130,160,90))
  cdrawquad3d(  1.033, 0.900,  0.950,
                1.033,-0.900,  0.950,
                4.500,-0.550,  0.500,
                4.500, 0.550,  0.500)

  // Engine top right
  setcolour(maprgb(100,130,60))
  cdrawquad3d(  1.033,-0.900,  0.950,
                1.033,-0.900,  0.000,
                4.500,-0.550,  0.000,
                4.500,-0.550,  0.500)

  // Engine bottom
  setcolour(maprgb(100,80,50))
  cdrawquad3d(  4.500, 0.550, -1.750,
                4.500,-0.550, -1.750,
                1.800,-1.000, -2.000,
                1.800, 1.000, -2.000)


  // Front cockpit left
  setcolour(maprgb(120,140,60))
  cdrawquad3d( -2.000, 1.000,  0.000,
               -2.000, 0.870,  0.600,
               -3.300, 0.870,  0.600,
               -3.300, 1.000,  0.000)

  // Front cockpit right
  setcolour(maprgb(120,140,60))
  cdrawquad3d( -2.000,-1.000,  0.000,
               -2.000,-0.870,  0.600,
               -3.300,-0.870,  0.600,
               -3.300,-1.000,  0.000)

  // Top front left
  setcolour(maprgb(100,120,40))
  cdrawquad3d(  1.033, 0.900,  0.950,
               -2.000, 0.750,  1.000,
               -2.000, 0.750,  0.000,
                1.033, 0.900,  0.000)

  // Top front middle
  setcolour(maprgb(120,140,60))
  cdrawquad3d(  1.033, 0.900,  0.950,
                1.033,-0.900,  0.950,
               -2.000,-0.750,  1.000,
               -2.000, 0.750,  1.000)

  // Top front right
  setcolour(maprgb(100,120,40))
  cdrawquad3d(  1.033,-0.900,  0.950,
               -2.000,-0.750,  1.000,
               -2.000,-0.750,  0.000,
                1.033,-0.900,  0.000)


  // Front wind shield
  setcolour(maprgb(180,200,150))
  cdrawquad3d( -1.300, 0.450,  1.000,
               -2.000, 0.450,  1.400,
               -2.000,-0.450,  1.400,
               -1.300,-0.450,  1.000)
  setcolour(maprgb(220,220,180))
  cdrawtriangle3d( -1.300, 0.450,  1.000,
                   -2.000, 0.450,  1.400,
                   -2.000, 0.650,  1.000)

  setcolour(maprgb(170,200,150))
  cdrawtriangle3d( -1.300,-0.450,  1.000,
                   -2.000,-0.450,  1.400,
                   -2.000,-0.650,  1.000)


  // Top left middle
  setcolour(maprgb(130,160,90))
  cdrawquad3d( -3.300, 0.750,  1.000,
               -3.300, 1.000,  0.000,
               -4.300, 1.000,  0.000,
               -4.300, 0.750,  1.000)

  // Top centre middle
  setcolour(maprgb(120,140,60))
  cdrawquad3d( -3.300, 0.750,  1.000,
               -3.300,-0.750,  1.000,
               -4.300,-0.750,  1.000,
               -4.300, 0.750,  1.000)

  // Top right middle
  setcolour(maprgb(130,160,90))
  cdrawquad3d( -3.300,-0.750,  1.000,
               -3.300,-1.000,  0.000,
               -4.300,-1.000,  0.000,
               -4.300,-0.750,  1.000)

  // Rear cockpit left
  setcolour(maprgb(120,140,60))
  cdrawquad3d( -4.300, 1.000,  0.000,
               -4.300, 0.870,  0.600,
               -5.583, 0.870,  0.600,
               -5.583, 1.000,  0.000)

  // Rear wind shield
  setcolour(maprgb(180,200,150))
  cdrawquad3d( -3.600, 0.450,  1.000,
               -4.300, 0.450,  1.400,
               -4.300,-0.450,  1.400,
               -3.600,-0.450,  1.000)
  setcolour(maprgb(220,220,180))
  cdrawtriangle3d( -3.600, 0.450,  1.000,
                   -4.300, 0.450,  1.400,
                   -4.300, 0.650,  1.000)

  setcolour(maprgb(170,200,150))
  cdrawtriangle3d( -3.600,-0.450,  1.000,
                   -4.300,-0.450,  1.400,
                   -4.300,-0.650,  1.000)



  // Rear cockpit right
  setcolour(maprgb(110,140,70))
  cdrawquad3d( -4.300,-1.000,  0.000,
               -4.300,-0.870,  0.600,
               -5.583,-0.870,  0.600,
               -5.583,-1.000,  0.000)


  // Lower left middle
  setcolour(maprgb(140,110,70))
  cdrawquad3d(  1.033, 1.000,  0.000,
                1.800, 1.000, -2.000,
               -3.583, 1.000, -2.238,
               -3.583, 1.000,  0.000)

  // Bottom middle
  setcolour(maprgb(120,100,60))
  cdrawquad3d(  1.800, 1.000, -2.000,
               -3.583, 1.000, -2.238,
               -3.583,-1.000, -2.238,
                1.800,-1.000, -2.000)

  // Lower right middle
  setcolour(maprgb(140,110,70))
  cdrawquad3d(  1.033,-1.000,  0.000,
                1.800,-1.000, -2.000,
               -3.583,-1.000, -2.238,
               -3.583,-1.000,  0.000)

  // Lower left back
  setcolour(maprgb(160,120,80))
  cdrawquad3d( -3.583, 1.000,  0.000,
              -16.000, 0.050,  0.000,
              -16.000, 0.050, -0.667,
               -3.583, 1.000, -2.238)

  // Bottom back
  setcolour(maprgb(130,90,60))
  cdrawquad3d( -3.583, 1.000, -2.238,
              -16.000, 0.050, -0.667,
              -16.000,-0.050, -0.667,
               -3.583,-1.000, -2.238)

  // Lower right back
  setcolour(maprgb(160,140,80))
  cdrawquad3d( -3.583,-1.000,  0.000,
              -16.000,-0.050,  0.000,
              -16.000,-0.050, -0.667,
               -3.583,-1.000, -2.238)

  // Top left back
  setcolour(maprgb(130,130,80))
  cdrawtriangle3d( -5.583, 0.650,  0.950,
                   -5.583, 1.000,  0.000,
                  -13.900, 0.150,  0.000)

  // Top centre back
  setcolour(maprgb(130,160,90))
  cdrawquad3d( -5.583, 0.650,  0.950,
               -5.583,-0.650,  0.950,
              -13.900,-0.150,  0.000,
              -13.900, 0.150,  0.000)

  // Top right back
  setcolour(maprgb(130,130,80))
  cdrawtriangle3d( -5.583,-0.650,  0.950,
                   -5.583,-1.000,  0.000,
                  -13.900,-0.150,  0.000)



  // Fin
  { // Rudder deflection 1 inch from hinge
    LET a = 1.100 * c.rudder / 17

    setcolour(maprgb(170,180,80))
    cdrawquad3d(-14.000, 0.000, 0.000,   // Fin
                -16.000, 0.000, 0.000,
                -16.000, 0.000, 1.000,
                -15.200, 0.000, 1.000)
    
    setcolour(maprgb(70,120,40))
    cdrawquad3d(-15.200-3*a,  9*a, 1.000,  // Rudder
                -16.000,    0.000, 1.000,
                -16.800+3*a,-10*a, 3.100,
                -16.000,    0.000, 2.550)
    setcolour(maprgb(70, 80,40))
    cdrawquad3d(-16.000,    0.000, 1.000,
                -16.800+3*a,-10*a, 3.100,
                -17.566+4*a,-14*a, 2.600,
                -17.816+4*a,-17*a, 1.667)
    setcolour(maprgb(70,120,40))
    cdrawquad3d(-16.000,    0.000, 1.000,
                -17.816+4*a,-17*a, 1.667,
                -17.816+4*a,-17*a, 1.000,
                -17.566+4*a,-14*a, 0.000)
    setcolour(maprgb(70, 80,40))
    cdrawquad3d(-16.000,    0.000, 1.000,
                -17.566+4*a,-14*a, 0.000,
                -17.000+2*a,- 8*a,-0.583,
                -16.000,    0.000,-0.667)

    // Tail skid
    setcolour(maprgb(20, 20,20))
    cdrawquad3d(-16.000,    0.000, -0.667,
                -16.200,    0.000, -0.667,
                -16.500+2*a, -8*a, -0.900,
                -16.300+2*a, -7*a, -0.900)

  }

  // Tailplane and elevator
  { // Elevator deflection 1 inch from hinge
    LET a = 0.600 * c.elevator / 17

    setcolour(maprgb(160,200,50)) 
    cdrawquad3d(-16.000, 0.000, 0.000, // Left tailplane
                -13.900, 0.600, 0.000,
                -14.600, 2.800, 0.000,
                -16.000, 4.500, 0.000)

    setcolour(maprgb(120,200,50))
    cdrawtriangle3d(-13.900, 0.600, 0.000,
                    -13.900,-0.600, 0.000,
                    -16.000, 0.000, 0.000)

    cdrawquad3d(-16.000, 0.000, 0.000, // Right tailplane
                -13.900,-0.600, 0.000,
                -14.600,-2.800, 0.000,
                -16.000,-4.500, 0.000)

    setcolour(maprgb(170,150,80)) 
    cdrawquad3d(-16.000, 0.000, 0.000, // Left elevator
                -17.200+4*a, 0.600, -15*a, // pt 1
                -17.500+5*a, 0.900, -16*a, // pt 2
                -17.666+5*a, 2.000, -17*a) // pt 3

    setcolour(maprgb(120,170,60)) 
    cdrawquad3d(-16.000,     0.000, 0.000, // Left elevator
                -17.666+5*a, 2.000, -17*a, // pt 3
                -17.450+4*a, 3.500, -16*a, // pt 4
                -17.200+4*a, 4.650, -14*a) // pt 5

    setcolour(maprgb(160,120,40)) 
    cdrawquad3d(-16.000,     0.000, 0.000, // Left elevator
                -17.200+4*a, 4.650, -14*a, // pt 5
                -16.700+a/2, 4.833,  -2*a, // pt 6
                -16.000,     4.500,     a) // pt 7

    setcolour(maprgb(170,150,80)) 
    cdrawquad3d(-16.000, 0.000, 0.000,     // Right elevator
                -17.200+4*a,-0.600, -15*a, // pt 1
                -17.500+5*a,-0.900, -16*a, // pt 2
                -17.666+5*a,-2.000, -17*a) // pt 3

    setcolour(maprgb(120,170,60)) 
    cdrawquad3d(-16.000,     0.000, 0.000, // Right elevator
                -17.666+5*a,-2.000, -17*a, // pt 3
                -17.450+4*a,-3.500, -16*a, // pt 4
                -17.200+4*a,-4.650, -14*a) // pt 5

    setcolour(maprgb(160,120,40)) 
    cdrawquad3d(-16.000,     0.000, 0.000, // Right elevator
                -17.200+4*a,-4.650, -14*a, // pt 5
                -16.700+a/2,-4.833,  -2*a, // pt 6
                -16.000,    -4.500,     a) // pt 7
  }
}




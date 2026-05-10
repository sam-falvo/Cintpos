
/* A genome is a sequence of bases specified by the letters A, C, G
and T.  This progam converts a given sequence into a 3D coloured path
following a route based on the sequence of bases and a set of angle
specifying how the genome bends at each base pair junction. The
program in interactive allowing the angles and viewing parameters to
be varied. It current works with various versions of the Covid-19
virus whose sequences have a length of about 27000 bases. The images
produced are surprising.


Implemented by Martin Richards (c) 23 October 2020

The current plan is to display an image of the genome as a sequence as
coloured triangles using the BCPL OpenGL interface. The path along the
genome will rotate by an amount depending on the pair of bases
involved. These rotations can be thought of as adjustments made by
elevator, aileron and rudder controls. The 16 sets of three angles can
be adjusted interactively causing the resulting genome image to change
shape. The genome sequence is provided by the FROM file.  Only the
characters A, C, G and T not in comment lines are used. The initial
setting of the rotational angles is given by a data file. 
The current set of angles can be saved to file using the W command..

History

11/12/2020
Changed the controls. The camera is on a platform and can point in
one of 10 directions relative to the orientation of the platform.
The orientation of the platform is specified by a 3x3 rotation matrix
whose rows are the real world coordinates of the t, w and l axes of
the platform. This matrix is

    ( ptn  ptw  pth )  
    ( pwn  pww  pwh )  
    ( pln  plw  plh )

ptn ptw and pth are the real world (N,W,H) coordinates of the
intersection of the t axis with a unit sphere centred at (0,0,0).  The
other two rows are similardly define for the w amd l axes.  The
position of the platform in world coordinates is platfomn, platfomw
and platfomh. It can only move forwards or backwards along its t
axis. Pressing F repeatedly increases its speed in direction t, and
pressing R reduces the speed potentially making the platform move
backwards. The orientation of the platform is controlled by the arrow
keys simulating the position of a joystick and < and > representing
the rudder peddles. When moving forwards it behaves somewhat like an
aircraft. The commands 0 to 9 control the orientation of the camera
relative to the platform, rather like th pilot turning his head.

Note that multiplying a point (n,w,h) in world coordinates by the
matrix

    ( ptn  pwn  pln )  
    ( ptw  pww  plw )  
    ( pth  pwh  plh )

will return the coordinates of the point in platform coordinates.


16/11/2020
Started to make a substantial modification causing the genome model
to remain fixed in space but allowing the position and orientation
to be moved.
N/n move the eye forward/back in direction N.
E/e move the eye forward/back in direction E.
H/h move the eye forward/back in direction H.
F/f move the eye forward/back in the direction of view.
The eye orientation can be controlled by the arrow keys and < and >
which behave like the joystick and rudder peddles of an aircraft.


28/06/2020
Started initial implementation.



Usage

dnaplot "FROM,ANGLES,-d/s,-t/s"

FROM     gives the filename of the file of genome letters. The default
         file name is "seq3" a version of the corona virus genome.
         The only significant letters used are A, C, G and T not appearing
         in text between # and the end of the line.

ANGLES   Specifies a file that replaces the default setting of the
         rotational angles of each possible base pair. There are
         16 pair AA, AC,..., TG, TT and each has 3 angles. These are
         floating point numbers giving angles in degrees. The default
         filename is "angles".

SEED=Y/n Set the random number seed.

X/s      Add random values to the w and l angles of every base pair.

-d/s     Turns on some debugging output.


The world axes are:

      N      Direction north
      W      Direction west
      H      Direction up

The axes used when constructing the genome model are

      t      The forward direction alone the genome
             (the direction of thrust if the path of the genome is
             thought of as an aircraft flying along the genome)
      w      Left direction (The direction of the left wing of
             the aircraft)
      l      The direction orthogonal to t and w (the direction of lift
             of the aircraft)

These axes are also used by the camera platform.

Initially every base pair has a rotation of 36.0 degrees about axis t,
and no rotation about the other two axes. This generates a straight
helix in direction N.

The genome model is made up of a sequence of segments between bases..
Each segment causes the (t,w,l) axes as if the aircraft was flying at
constant speed with setting of the ailerons, elevator and rudder
depending on the previous and current base letters of the segment. The
(t,w,l) axes of the first segment of the genome are aligned with
(N,W,H). Note that both (N,W,H) and (t,w,l) are right handed axes.

The first segment is in the direction of the N axis and it starts at
(N,W,H)=(0,0,0).  Unit distance in the model corresponds to a real
world distance of 1 nm.


Properties of B-type DNA

It is a right handed double helix.
The distance between bases is  0.34 nm.
The rate of rotation is 10 bases per 360 degrees ie 36 degrees per base.
The diameter of the genome   1.9 nm.


The creation of the model.

The model can be thought of as the path taken by an aircraft
travelling at constant speed. The axes as t (the direction of thrust),
w (the direction of the left wing) and (l the direction of lift). The
bending of the path is assumed to bend at the junctions between each
pair of adjacent bases. For each base pair the bend is specified by
angles of rotation about each of the axes t, w and l.

The genome is represented by a sequence of triangles in 3D pointing
along the sequence and its orientation depends on the orientation of
the previous triangle and the angles of rotation for the two bases
involved. Each triangle is given a colour that identifies the first
base of the pair involved.  The genome is displayed using the OpenGL
library.

The user can control the motion of the camera platform and can also
adjust the angles of rotation for any base pair. If an interesting
genome shape is found its 48 base pair angles can be written to file.

The commands available to the user are as follows.

  ?        Output this help info
  Q        Quit
  AA AC AG AT CA CC etc to TG TT  Select base pair
  N        Select next axis
  F f      Push the camera platform forward orback
  L l      Push the camera platform left or right
  U u      Push the camera platform up or down
  # ~      Increase/Decrease the currently selected angle
  | \      Double or Halve the increment angle
  Y        Randomly increment to the w and l angles of
             all 16 base pairs
  H        Rotate the camera platform to point to the model origin
  Z        Set w and l angles to zero for all 16 base pairs
  < >      Increase ordecrease rotation rate about l
  Up Down arrow      Increase or decrease rotation rate about w
  Left Right arrow   Increase or decrease rotation rate about t
  S        Start or Stop the motion of the camera paltform
  0 to 9   Select a camera direction relative to the platform
  P        Output the current orientation and other values
  W name   Write the angles data to file



*/

GET "libhdr"
GET "gl.h"
GET "gl.b"          // Insert the library source code
.
GET "libhdr"
GET "gl.h"

GLOBAL {
  stdin:ug
  stdout

  FLT pi
  FLT f0 // To hold 0.0
  FLT f1 // To hold 1.0
  
  fromfilename
  anglesfilename

  tracing
  debug

  tstanglet  // For the platform rotation only used
  tstanglew  // when debugging.
  tstanglel
  tstdirection
  
  spacev; spacep; spacet  // Used by newvec

  blklist  // List of blocks of work space
  blkp
  blkt
 
  done
  stepping
  
  glprog
  Vshader
  Fshader

  // Attribute variable locations
  VertexLoc
  ColourNoLoc

  // Uniform variable locations
  MatrixLoc

  // There is a platform with axes t, w and l that holds the camera.
  // It behaves somewhat like an aircraft that moves in the direction
  // of thrust (t). Unlike an aircraft, there is no gravity and it can
  // accelerate forward and backward using F and f, and can rotate
  // about its axes using th joystick and rudder peddles. The camera
  // sits on the platform normally pointing in direction t, but the
  // user can specify 10 orientations the camera can have relative to
  // the platform rather like the pilot turning his/her head. The
  // posible orientations are:
  //   0 Forard
  //   1 forward right
  //   2 Right
  //   3 Back right
  //   4 Back
  //   5 Back left
  //   6 Left
  //   7 Forard left
  //   8 Forward up
  //   9 Forward down
  
  FLT platformn                // Coordinates of the camera platform
  FLT platformw
  FLT platformh

  FLT platformtdot             // Platform velocity in direction t
  FLT platformwdot             // Platform velocity in direction w
  FLT platformldot             // Platform velocity in direction l


  FLT prtdot                   // Rates of platform rotation
  FLT prwdot
  FLT prldot

  FLT ptn; FLT ptw; FLT pth    // Platform orientation matrix
  FLT pwn; FLT pww; FLT pwh
  FLT pln; FLT plw; FLT plh

  heading                      // Angles of the platform in degrees
  climbangle
  bankangle
  
  direction                    // Camera direction 0 to 9
  
  FLT ctn; FLT ctw; FLT cth    // Camera orientation matrix
  FLT cwn; FLT cww; FLT cwh    // with t being the direction of view.
  FLT cln; FLT clw; FLT clh


  VertexBuffer// =0 or the name of the buffer that holds all the vertex
              // data we ever need.
  IndexBuffer // =0 or the name of the buffer that holds all the index
              // data we ever need.

  projectionMatrix // is the matrix used by the vertex shader
                   // to transform the vertex coordinates to
                   // screen coordinates.
  workMatrix       // is used when constructing the projection matrix.

  // Globals used to construct the model

  // The orientation of the current base in the genome, used by
  // mkgenomemodel.
  FLT gtn; FLT gtw; FLT gth        // Direction cosines of direction t, col 1
  FLT gwn; FLT gww; FLT gwh        // Direction cosines of direction w, col 2
  FLT gln; FLT glw; FLT glh        // Direction cosines of direction l, col 3

  // Note that [ a0,a1,a3, a3,a4 a5, a6,a7,a8 ] represents the matrix
  //
  //    { a0  a3  a6 )
  //    { a1  a4  a7 )
  //    { a2  a5  a8 )


  anglesv     // Points to 16 angle triples
  matrixv     // Vector of the 3x3 rotation matrices for the 16 base pairs
  
  genomev     // This will hold the genome base numbers in
              // genomev!1 to genomev!upb where upb is genomev!0.
              // eg -> [ 4, 1,2,3,4 ] for sequence A C G T


  // Variables are used by mkGenomeModel to represent the genome model.

  vertexv      // This will hold the vertex data in vertexv!1 to vertexv!upb
               // where upb is vertexv!0

  indexv       // This will hold the indices in indexv!1 to indexv!upb
               // where upb is indexv!0

  displayv     // This will hold the vertex data in vertexv!1 to vertexv!upb
               // where upb is vertexv!0

  indexp       // The index number of the next vertex to be created.
               // The vertex with index number 0 will start at
	       // position 1 in vvec.
  preveventch  // Variables used by F, f, L, l, U and u
	       // Other characters set preveventch to zero
  FLT prevspeed// eg first F sets prevspeed to 0.05, multiple Fs
               //    double prevspeed each time.
	       
  // The global functions
  newvec
  pushval
  concatstr
  letno2ch
  setangles
  mat3set
  mat3mulmmm
  mat3mulmvv
  prmat3
  mat4set
  prmat4
  prpairs
  prpair
  setrotationmatrix
  drawTriangle
  rdGenome
  mkGenomeModel
  rdangles
  standardiseV3
  axis2ch
  doincrement
  drawmodel
  initImageWindow
  deg2rad
  rad2deg

  XY2str
  prv4
  rotate3
  setcurraxis
  setcurrpair
  setinitstate
  wrcurrangle
  
  Compileshader
  processevents
  
  // The user can adjust DNA segment angles interactively.
  
  FLT incrementdegrees
  FLT incrementradians
  
  incrementstate    // =-1 or 0 to 3 after first letter of a pair
  incrementing      // =TRUE after command AA,AC,...,TG,TT

  currpair          // = 0 to 15 for AA to TT
  curraxis          // 0=t 1=w 2=l
  modelchanged      // =TRUE if the model needs to be rebuilt and
                    // sent to GL

  setrandomangles
  
  FLT genomeradius  // Typically = 0.95 nms (=1.90/2)
  FLT genomesep     // Typically = 0.34 nms

  FLT col_white
  FLT col_black


}

MANIFEST {
  blkupb = 1000000
  
  // Pair subscripts
  AA=0; AC; AG; AT; CA; CC; CG; CT; GA; GC; GG; GT; TA; TC; TG; TT
}

LET newvec(n) = VALOF
{ LET p = blkp
  blkp := p+n+1
  
  IF blkp>=blkt DO
  { LET v = getvec(blkupb) // Get some more space
//writef("newvec: allocation block %n upb %n*n", v, blkupb)
    UNLESS v & n<blkupb DO
    { LET out = output()
      selectoutput(stdout)
      writef("*nSystem error: newvec failure*n")
      selectoutput(out)
      abort(999)
    }
    
    v!0 := blklist
    blklist := v
    blkt := v+blkupb
    p    := v+1
    blkp := p+n+1
  }
//writef("newvec: allocated p=%n n=%i4 blklist=%n*n",
//         p, n, blklist)
  RESULTIS p
}

LET pushval(sxv, val) BE
{ // Push a value into a self expanding vector
  LET upb = sxv!0      // =0 or the upb of v
  LET v   = sxv!1      // =0 or a getvec'd vector
  LET p = v -> v!0, 0 // Position of the previous element, if any.
  // Initially upb, v, and p are all zero
  // If v is not zero, v!0 will be the subscript of its latest element in v.
  // If the vector is full, pushval will allocate another larger vector
  // and copy the existing elements into it before pushing x.

  IF p>=upb DO
  { LET newupb = 3*upb/2 + 10 // upb of the new larger vector
    LET newv = getvec(newupb)
    UNLESS newv DO
    { writef("More memory needed for pushval*n")
      abort(999)
      RETURN
    }
    sxv!0 := newupb // Update the control block
    sxv!1 := newv

    FOR i = 0 TO upb DO newv!i := v!i      // Copy the existing elements
    FOR i = upb+1 TO newupb DO newv!i := 0 // Pad with zeroes

    IF v DO freevec(v) // Free the old vector if it existed.

    v := newv
  }
  p := p+1
  v!0, v!p := p, val
}

LET concatstr(s1, s2, s3) = VALOF
{ LET len1, len2 = s1%0, s2%0
  LET pos = 0
  FOR i = 1 TO len1 DO { pos := pos+1; s3%pos := s1%i }
  FOR i = 1 TO len2 DO { pos := pos+1; s3%pos := s2%i }
  s3%0 := pos
  RESULTIS s3
  
}

AND letno2ch(letno) = VALOF SWITCHON letno INTO
{ DEFAULT: RESULTIS '?'
  CASE 0:  RESULTIS 'A'
  CASE 1:  RESULTIS 'C'
  CASE 2:  RESULTIS 'G'
  CASE 3:  RESULTIS 'T'
  CASE 4:  RESULTIS 'W' // For White
  CASE 5:  RESULTIS 'B' // For Black
}

AND letno2colno(letno) = VALOF SWITCHON letno INTO
{ DEFAULT: RESULTIS 0.0
  CASE 0:  RESULTIS 0.0 // Colour number for base A
  CASE 1:  RESULTIS 1.0 // Colour number for base C
  CASE 2:  RESULTIS 2.0 // Colour number for base G
  CASE 3:  RESULTIS 3.0 // Colour number for base T
  CASE 4:  RESULTIS 4.0 // White
  CASE 5:  RESULTIS 5.0 // Black
}

AND mat3set(m, a0, a1, a2, a3, a4, a5, a6, a7, a8) BE
{ // Set the elements of matrix m.
  m!0, m!3, m!6 := a0, a3, a6
  m!1, m!4, m!7 := a1, a4, a7
  m!2, m!5, m!8 := a2, a5, a8
}

AND mat3mulmmm(m, p, q) BE
{ // m -> [ m0,m1,m2,  m3,m4,m5,  m6,m7,m8 ]
  // p -> [ p0,p1,p2,  p3,p4,p5,  p6,p7,p8 ]
  // r -> [ r0,r1,r2,  r3,r4,r5,  r6,r7,r8 ]
  //
  // This multiplies matrices m and p together placing the result in q
  // m, p and q need not be distinct.
  //
  //  ( m0  m3  m6)     ( p0, p3, p6 )      ( q0  q3  q6 )
  //  ( m1  m4  m7)  x  ( p1, p4, p7 )  =>  ( q1  q4  q7 )
  //  ( m2  m5  m8)     ( p2, p5, p8 )      ( q2  q5  q8 )
  
  LET FLT m0, FLT m1, FLT m2 = m!0, m!1, m!2
  LET FLT m3, FLT m4, FLT m5 = m!3, m!4, m!5
  LET FLT m6, FLT m7, FLT m8 = m!6, m!7, m!8
  
  LET FLT p0, FLT p1, FLT p2 = p!0, p!1, p!2
  LET FLT p3, FLT p4, FLT p5 = p!3, p!4, p!5
  LET FLT p6, FLT p7, FLT p8 = p!6, p!7, p!8

  q!0 := m0*p0 + m3*p1 + m6*p2
  q!1 := m1*p0 + m4*p1 + m7*p2
  q!2 := m2*p0 + m5*p1 + m8*p2
  
  q!3 := m0*p3 + m3*p4 + m6*p5
  q!4 := m1*p3 + m4*p4 + m7*p5
  q!5 := m2*p3 + m5*p4 + m8*p5
  
  q!6 := m0*p6 + m3*p7 + m6*p8
  q!7 := m1*p6 + m4*p7 + m7*p8
  q!8 := m2*p6 + m5*p7 + m8*p8
}

AND mat3mulmvv(m, p, q) BE
{ // m -> [ m0,m1,m2,  m3,m4,m5,  m6,m7,m8 ]
  // p -> [ p0,p1,p2 ]
  // q -> [ q0,q1,q2 ]
  //
  // This multiply matrix m by vector p placing the result in vector q
  // m, p and q need not be distinct.
  //
  //  ( m0  m3  m6 )     ( p0 )      ( q0  )
  //  ( m1  m4  m7 )  x  ( p1 )  =>  ( q1  )
  //  ( m2  m5  m8 )     ( p2 )      ( q2  )
  
  LET FLT m0, FLT m1, FLT m2 = m!0, m!1, m!2
  LET FLT m3, FLT m4, FLT m5 = m!3, m!4, m!5
  LET FLT m6, FLT m7, FLT m8 = m!6, m!7, m!8
  
  LET FLT p0, FLT p1, FLT p2 = p!0, p!1, p!2

  q!0 := m0*p0 + m3*p1 + m6*p2
  q!1 := m1*p0 + m4*p1 + m7*p2
  q!2 := m2*p0 + m7*p1 + m8*p2
}

AND mat4mulmvv(m, p, q) BE
{ // m -> [ m00 m01 m02 m03   // Column 1
  //        m04 m05 m06 m07   // Column 2
  //        m08 m09 m10 m11   // Column 3
  //        m12 m13 m13 m15]  // Column 4
  // p -> [ p0 p1 p2 p3 ]
  // q -> [ q0 q1 q2 q3 ]
  //
  // This multiply matrix m by vector p placing the result in vector q
  // m, p and q need not be distinct.
  //
  //  ( m00  m04  m08  m12 )     ( p0 )      ( q0 )
  //  ( m01  m05  m09  m13 )  x  ( p1 )  =>  ( q1 )
  //  ( m02  m06  m10  m14 )     ( p2 )      ( q2 )
  //  ( m03  m07  m11  m15 )     ( p3 )      ( q3 )
  
  LET FLT m00, FLT m01, FLT m02, FLT m03 = m!00, m!01, m!02, m!03
  LET FLT m04, FLT m05, FLT m06, FLT m07 = m!04, m!05, m!06, m!07
  LET FLT m08, FLT m09, FLT m10, FLT m11 = m!08, m!09, m!10, m!11
  LET FLT m12, FLT m13, FLT m14, FLT m15 = m!12, m!13, m!14, m!15
  
  LET FLT p0, FLT p1, FLT p2, FLT p3 = p!0, p!1, p!2, p!3

  q!0 := m00*p0 + m04*p1 + m08*p2 + m12*p3
  q!1 := m01*p0 + m05*p1 + m09*p2 + m13*p3
  q!2 := m02*p0 + m06*p1 + m10*p2 + m14*p3
  q!3 := m03*p0 + m07*p1 + m11*p2 + m15*p3
}

AND setangles(XY, FLT an, FLT aw, FLT ah) BE
{ // Set the angles for the pair XY and also compute the angles
  // for pair YX. The angles are given in radians.
  LET X  = XY>>2 & 3
  LET Y  = XY & 3
  
  LET XYav = anglesv!XY
  LET XYm  = matrixv!XY

//writef("setangles: %s %7.1f %7.1f %7.1f*n",
//        XY2str(XY), rad2deg(an), rad2deg(aw), rad2deg(ah))
  XYav!0 := an
  XYav!1 := aw
  XYav!2 := ah

  setrotationmatrix(XY)
  RETURN
  
  // Attempt to make the inverse rotation
  UNLESS X=Y DO
  { LET YX = Y<<2 | X   // In case we wish to set the angles for YX as well.

    LET YXav = anglesv!YX
    LET YXm  = matrixv!YX
  
    LET FLT bn, FLT bw, FLT bh = f0, f0, f0
    LET FLT c,  FLT s = ?, ?
  
    LET FLT mtn, FLT mtw, FLT mth = f0, f0, f0
    LET FLT mwn, FLT mww, FLT mwh = f0, f0, f0
    LET FLT mln, FLT mlw, FLT mlh = f0, f0, f0
  
    LET m  = @mtn  // A temp rotation matrix
    LET tm = VEC 8 // Another temp matrix
  
    // X and Y are different bases so we must calculate the rotation
    // angles for YX.


    // The rotation matrix for the transition from base X to Y depends
    // on it three axis rotation angles. The order in which these
    // axis rotations are done in the order N, W and H. If the t, w and
    // l axes of the segment for X are aligned in directions  N, W and
    // H the the columns of the rotation matrix will give the
    // directions of these axes for segment Y. Clearly the rotation
    // caused by the transition from segment Y to X will be the inverse
    // of the rotation from X to Y. The inverse matrix is easy to calculate
    // since for rotation matrices the inverse is just the transpose.
    // However what we need are the rotation angles bn, bw and bh of the
    // inverse transformation.  Assuming columns of the XY rotation matrix
    // are:
    //         ( mtn )   ( mwn )   ( mln )
    //         ( mtw )   ( mww )   ( mlw )
    //         ( mth )   ( mwh )   ( mlh )
    // These are unit vector representing the t, w and l directions of
    // the Y segment. We can easily find the angle of rotation about the
    // N axis to cause the l axis to lie in the NOH plane. This angle is
    // arctan(mlw/mlh). To avoid overflow this angle is taken to be zero
    // if mlw is very small. If c and s are the cosine and sine of this
    // angle the rotation aboyt N is represented by
    //      ( 1   0   0 )
    //      ( 0   c   s )
    //      ( 0  -s   c )
    // We can now multiply the orientation matrix by this one to obtain
    // one in which axis t is in the plane NOH. The angles of rotation
    // for the axes W and L are easily obtained by the same method.
    //abort(9186)
    FOR i = 0 TO 8 DO m!i := XYm!i  // Copy the XY rotation matrix
//writef("Rotation matrix for %s is:*n", XY2str(XY))
    //prmat3(m)

    // Find the rotation angle about axis N
    //writef("Rotate about N*n")
    UNLESS -1e-8 < mlw < 1e-8 DO
    { bn := sys(Sys_flt, fl_atan2, mlw, mlh)
      c, s := sys(Sys_flt, fl_cos, bn), sys(Sys_flt, fl_sin, bn)
      //writef("Angle about N is %6.1f => c=%7.4f  s= %7.4f*n", rad2deg(bn), c, s)
      // Rotate clockwise about axis N by bn radians
      //writef("mlw=%8.5f mlh=%8.5f => bn=%7.1f*n", mlw, mlh, rad2deg(bn))
      mat3set(tm,        f1,    f0,    f0,     f0,  c,  s,    f0, -s,  c)
      mat3mulmmm(tm,m,m)
    }
        
    //prmat3(m)

    // Find the rotation angle about axis W
    //writef("Rotate about W*n")
    UNLESS -1e-8 < mln < 1e-8 DO
    { bw := sys(Sys_flt, fl_atan2, -mln, mlh)
      c, s := sys(Sys_flt, fl_cos, bw), sys(Sys_flt, fl_sin, bw)
      //writef("Angle about W is %6.1f => c=%7.4f  s= %7.4f*n", rad2deg(bw), c, s)
      // Rotate clockwise about axis W by bw radians
      //writef("mln=%8.5f mlh=%8.5f => bn=%7.1f*n", mln, mlh, rad2deg(bw))
      mat3set(tm,       c,    f0, -s,     f0,    f1,    f0,  s,    f0,  c)
      mat3mulmmm(tm,m,m)
    }

    //prmat3(m)

    // Find the rotation angle about axis H
    //writef("Rotate about H*n")
    UNLESS -1e-8 < mtw < 1e-8 DO
    { bh := sys(Sys_flt, fl_atan2, -mtw, mtn)
      c, s := sys(Sys_flt, fl_cos, bh), sys(Sys_flt, fl_sin, bh)    
      //writef("Angle about H is %6.1f => c=%7.4f  s= %7.4f*n", rad2deg(bh), c, s)
      // Rotate clockwise about axis N by bn radians
      //writef("mtw=%8.5f mtn=%8.5f => bn=%7.1f*n", mtw, mtn, rad2deg(bh))
      mat3set(tm,       c,  s,    f0,  -s,  c,    f0,    f0,    f0,    f1)
      mat3mulmmm(tm,m,m)
    }

    //prmat3(m)

    YXav!0 := bn
    YXav!1 := bw
    YXav!2 := bh

    setrotationmatrix(YX)
    // Check that the product of the XY and YX rotation matrices
    // is approximatly the identy matrix.
    //writef("*nCheck product of XYm and YXm*n")
    //mat3mulmmm(XYm, YXm, m)
    //prmat3(m)
    //abort(2233)
  }
}

AND setrotationmatrix(XY) BE
{ LET av = anglesv!XY  // The rotation angles for pair XY
  LET mv = matrixv!XY  // The 3x3 rotation matrix for pair XY
  
  LET FLT at, FLT aw, FLT al = av!0, av!1, av!2 // The angles for pair XY
  
  LET m = VEC 8   // A temp 3x3 matrix
   
  LET FLT cost, FLT sint = sys(Sys_flt, fl_cos, at), sys(Sys_flt, fl_sin, at)
  LET FLT cosw, FLT sinw = sys(Sys_flt, fl_cos, aw), sys(Sys_flt, fl_sin, aw)
  LET FLT cosl, FLT sinl = sys(Sys_flt, fl_cos, al), sys(Sys_flt, fl_sin, al)
  
 // Rotate clockwise about axis N by t radians
  mat3set(mv,      f1,  f0,    f0,     f0, cost, sint,   f0, -sint, cost)

  // Rotate clockwise about axis W by w radians
  mat3set(m,     cosw,  f0, -sinw,     f0,    f1,  f0, sinw,    f0, cosw)
  mat3mulmmm(m,mv,mv)
 
  // Rotate clockwise about axis H by l radians
  mat3set(m,     cosl, sinl,   f0,  -sinl,  cosl,  f0,   f0,    f0,   f1)
  mat3mulmmm(m,mv,mv)

  //writef("setrotationmatrix: at=%9.5f aw=%9.5f al=%9.5f =>*n", at, aw, al)
  //prmat3(mv)
  modelchanged := TRUE
}


AND rotate3t(m, FLT angle) BE
{ // m is a 3x3 orientation matrix whose columns are the direction
  // cosines of its axes, t, w and l. The orientation is rotated
  // clockwise by the specified angle in radians about axis t.
  LET tm = VEC 8  // Temp matrix
   
  LET FLT c = sys(Sys_flt, fl_cos, angle)
  LET FLT s = sys(Sys_flt, fl_sin, angle)
  
  // Rotate clockwise about axis w by angle radians
  // The unit point (1,0,0) on the the t axis must move to   ( 1,  0,  0) 
  // The unit point (0,1,0) on the the w axis must remain at ( 0,  c,  s)
  // The unit point (0,0,1) on the the l axis must remain at ( 0, -s,  c) 
  // So the required rotation matrix is
  //     (  1  0  0 )
  //     (  0  c  s )
  //     (  0 -s  c )
  mat3set(tm, f1, f0, f0,    // Column 1
              f0,  c,  s,    // Column 2
	      f0, -s,  c)    // Column 3
  mat3mulmmm(tm, m, m)
}

AND rotate3w(m, FLT angle) BE
{ // m is a 3x3 orientation matrix whose columns are the direction
  // cosines of its axes, t, w and l. The orientation is rotated
  // clockwise by the specified angle in radians about axis w.
  LET tm = VEC 8  // Temp matrix
   
  LET FLT c = sys(Sys_flt, fl_cos, angle)
  LET FLT s = sys(Sys_flt, fl_sin, angle)
  
  // Rotate clockwise about axis w by angle radians
  // The unit point (1,0,0) on the the t axis must move to   ( c, 0, -s) 
  // The unit point (0,1,0) on the the w axis must remain at ( 0, 1,  0)
  // The unit point (0,0,1) on the the l axis must remain at ( s, 0,  c) 
  // So the required rotation matrix is
  //     (  c  0  s )
  //     (  0  1  0 )
  //     ( -s  0  c )
  mat3set(tm,  c, f0, -s,    // Column 1  ie the transformation of (1,0,0)
              f0, f1, f0,    // Column 2  ie the transformation of (0,1,0)
	       s, f0,  c)    // Column 3  ie the transformation of (0,0,1)
//prmat3(tm)
//writef("times*n")
//prmat3(m)
//writef("equals*n")
  mat3mulmmm(tm, m, m)
//prmat3(m)
//newline()
}

AND rotate3l(m, FLT angle) BE
{ // m is a 3x3 orientation matrix whose columns are the direction
  // cosines of its axes, t, w and l. The orientation is rotated
  // clockwise by the specified angle in radians about axis l.
  LET tm = VEC 8  // Temp matrix
   
  LET FLT c = sys(Sys_flt, fl_cos, angle)
  LET FLT s = sys(Sys_flt, fl_sin, angle)
  
  // Rotate clockwise about axis l by angle radians
  // The unit point (1,0,0) on the the t axis must move to   ( c,-s, 0) 
  // The unit point (0,1,0) on the the w axis must move to   ( s, c, 0) 
  // The unit point (0,0,1) on the the l axis must remain at ( 0, 0, 1)
  // So the required rotation matrix is
  //     (  c -s  0 )
  //     (  s  c  0 )
  //     (  0  0  1 )
  mat3set(tm,  c,  s, f0,    // Column 1
              -s,  c, f0,    // Column 2
	      f0, f0, f1)    // Column 3
  mat3mulmmm(tm, m, m)
}

AND rotate3(m, FLT at, FLT aw, FLT al) BE
{ // m is a 3x3 orientation matrix whose columns are the direction
  // cosines of its axes, t, w and l. The orientation is rotated
  // by at radians about axis t, then by aw about w and finally al
  // about l.

  rotate3t(m, at)
  rotate3w(m, aw)
  rotate3l(m, al)
}


AND drawTriangle(vsxv, isxv, a, b, c, colno) BE
{ // vsxv is the self expanding vector for the vertex data
  // isxv is the self expanding vector for the indices
//abort(1001)
//writef("%i5: %12.5f  %12.5f  %12.5f %3.1f*n",
//        indexp, a!0, a!1, a!2, colno)
  pushval(vsxv, a!0)
  pushval(vsxv, a!1)
  pushval(vsxv, a!2)
  pushval(vsxv, colno)
  pushval(isxv, indexp)
  indexp := indexp+1
  
//writef("%i5: %12.5f  %12.5f  %12.5f %3.1f*n",
//        indexp, b!0, b!1, b!2, colno)
  pushval(vsxv, b!0)
  pushval(vsxv, b!1)
  pushval(vsxv, b!2)
  pushval(vsxv, colno)
  pushval(isxv, indexp)
  indexp := indexp+1
  
//writef("%i5: %12.5f  %12.5f  %12.5f %3.1f*n",
//        indexp, c!0, c!1, c!2, colno)
  pushval(vsxv, c!0)
  pushval(vsxv, c!1)
  pushval(vsxv, c!2)
  pushval(vsxv, colno)
  pushval(isxv, indexp)
  indexp := indexp+1
}


AND rdGenome(filename) = VALOF
{ // Read the genome into a self expanding vector and, if successful,
  // returns its getvec'd vector of letter numbers 1, 2, 3 and 4. The
  // zeroth element is the subscript of the last base of the genome.
  // The result is zero on failure.

  LET vupb, v = 0, 0 // The self expanding vector for the letter numbers.
  LET sxv = @vupb
  
  LET instream = filename -> findinput(filename), 0

  UNLESS instream RESULTIS 0

  selectinput(instream)

  { LET ch = capitalch(rdch())
    SWITCHON ch INTO
    { DEFAULT:  LOOP

      CASE endstreamch:
        pushval(sxv, -1)
	endstream(instream)
	//abort(1004)
	RESULTIS v // v is zero or a getvec's vector holding the genome.

      CASE '#': // Ignore a comment line
        writef("Loading Genome sequence: ")
        UNTIL ch='*n' | ch=endstreamch DO
	{ ch := rdch()
	  wrch(ch)      // Copy the comment to standard output
	}
	newline()
	LOOP

      CASE 'A': pushval(sxv, 0); LOOP
      CASE 'C': pushval(sxv, 1); LOOP
      CASE 'G': pushval(sxv, 2); LOOP
      CASE 'T': pushval(sxv, 3); LOOP
    }
  } REPEAT
}


AND mkGenomeModel() = VALOF
{ // This function contructs the genome model from the sequence in genomev
  // It sets the following
  // vertexv  to hold the vertex data, vertexv!0 holds its upb
  // iindexv  to hold the indices, indexv!0 holds its upb
  // displayv to hold the display items, displayv!0 holds its upb

  LET vupb, vvec = 0, 0 // Self expanding vectors for vertices
  LET iupb, ivec = 0, 0 //   for index values
  LET dupb, dvec = 0, 0 //   for display values

  LET vsxv = @vupb
  LET isxv = @iupb
  LET dsxv = @dupb

  // vsxv is a self expanding vector for the vertices [x y z colourno].
  // isxv  is a self expanding array for the triangles.
  // dsxv  is a self expanding array for the display elements

  // Coords of the mid point of the leading edge of the first segment
  LET FLT  x, FLT  y, FLT  z = f0, f0, f0
  // Coords of the mid point of the leading edge of the next segment, if any
  LET FLT nx, FLT ny, FLT nz =  ?,  ?,  ?

  // Declare the matrix representing the orientation of the next
  // segment of the genome.
  LET FLT stx, FLT sty, FLT stz = f1, f0, f0  // Direction t
  LET FLT swx, FLT swy, FLT swz = f0, f1, f0  // Direction w
  LET FLT slx, FLT sly, FLT slz = f0, f0, f1  // Direction l
  LET m = @stx  // The 3x3 orientation matrix
  
  indexp := 0
  
  IF FALSE DO // Normally commented out if debugging model required
  { // Construct a simple 3D shape as a debugging  aid
    // The shape is a hollow 2x2 coloured cube centres at (0,0,0)
    // with a pyramid of length 2 attatched to the front of the cube
    // pointing in the N direction.
    // Variables used for drawing each triangle
    LET FLT ax, FLT ay, FLT az = 0.0, 0.0, 0.0
    LET FLT bx, FLT by, FLT bz = 0.0, 0.0, 0.0
    LET FLT cx, FLT cy, FLT cz = 0.0, 0.0, 0.0
    LET FLT dx, FLT dy, FLT dz = 0.0, 0.0, 0.0
    LET FLT ex, FLT ey, FLT ez = 0.0, 0.0, 0.0

    ax, ay, az := -1.0,  1.0,  1.0   // Left cube face upper triangle
    bx, by, bz :=  1.0,  1.0,  1.0
    cx, cy, cz := -1.0,  1.0, -1.0
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 0.0)   // Red
    ax, ay, az := -1.0,  1.0, -1.0   // Left cube face lower triangle
    bx, by, bz :=  1.0,  1.0, -1.0
    cx, cy, cz :=  1.0,  1.0,  1.0
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 0.0)   // Red

    ax, ay, az := -1.0, -1.0,  1.0   // Right cube face upper triangle
    bx, by, bz :=  1.0, -1.0,  1.0
    cx, cy, cz := -1.0, -1.0, -1.0
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 1.0)   // Green
    ax, ay, az := -1.0, -1.0, -1.0   // Right cube face lower triangle
    bx, by, bz :=  1.0, -1.0, -1.0
    cx, cy, cz :=  1.0, -1.0,  1.0
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 1.0)   // Green

    ax, ay, az := -1.0,  1.0,  1.0   // Top cube face left triangle
    bx, by, bz :=  1.0,  1.0,  1.0
    cx, cy, cz :=  1.0, -1.0,  1.0
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 4.0)   // White
    ax, ay, az := -1.0,  1.0,  1.0   // Top cube face right triangle
    bx, by, bz := -1.0, -1.0,  1.0
    cx, cy, cz :=  1.0, -1.0,  1.0
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 4.0)   // White

    ax, ay, az := -1.0,  1.0, -1.0   // Top cube face left triangle
    bx, by, bz :=  1.0,  1.0, -1.0
    cx, cy, cz :=  1.0, -1.0, -1.0
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 5.0)   // Black
    ax, ay, az := -1.0,  1.0, -1.0   // Top cube face right triangle
    bx, by, bz := -1.0, -1.0, -1.0
    cx, cy, cz :=  1.0, -1.0, -1.0
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 5.0)   // Black

    ax, ay, az :=  1.0,  1.0,  1.0   // The pyramic
    bx, by, bz :=  1.0, -1.0,  1.0
    cx, cy, cz :=  1.0, -1.0, -1.0
    dx, dy, dz :=  1.0,  1.0, -1.0
    ex, ey, ez :=  3.0,  0.0,  0.0   // The point
    drawTriangle(vsxv, isxv, @ax, @bx, @ex, 0.0)   // Cyan
    drawTriangle(vsxv, isxv, @bx, @cx, @ex, 2.0)   // Cyan
    drawTriangle(vsxv, isxv, @cx, @dx, @ex, 1.0)   // Cyan
    drawTriangle(vsxv, isxv, @dx, @ax, @ex, 3.0)   // Cyan
  }

  IF FALSE DO // Normally not commented out
  { // Draw triangle in the NDC cube as a test.
    LET FLT ax, FLT ay, FLT az = -0.9,  +0.9, +0.9
    LET FLT bx, FLT by, FLT bz = +0.9,   0.8,  0.9
    LET FLT cx, FLT cy, FLT cz =  0.5,   0.2, -0.9

    LET FLT px, FLT py, FLT pz =  0.0000,  +0.9999, +0.9999
    LET FLT qx, FLT qy, FLT rz = +0.9999,  -0.9999, +0.9999
    LET FLT rx, FLT ry, FLT rz = -0.9999,  -0.9999, +0.9999

    // Draw the rectangle as two triangles
    drawTriangle(vsxv, isxv, @ax, @bx, @cx, 1.0)
    drawTriangle(vsxv, isxv, @px, @qx, @rx, 2.0)
  }
  
  // Draw the initial rectangle to mark the start of the genome.
  //IF FALSE DO // Normally commented out
  { LET FLT lx0, FLT ly0, FLT lz0 = -2.0,  genomeradius, f0 // For a rectangle
    LET FLT rx0, FLT ry0, FLT rz0 = -2.0, -genomeradius, f0 // in plane x-y
    LET FLT lx1, FLT ly1, FLT lz1 =   f0,  genomeradius, f0
    LET FLT rx1, FLT ry1, FLT rz1 =   f0, -genomeradius, f0

    LET FLT tx0, FLT ty0, FLT tz0 = -2.0,  0.5, 0.3 // For a black triangles to
    LET FLT tx1, FLT ty1, FLT tz1 = -2.0, -0.5, 0.3 // help debug the projection.

    // Draw the rectangle as two triangles
    drawTriangle(vsxv, isxv, @rx0, @lx1, @lx0, 4.0)        // Left red triangle
    drawTriangle(vsxv, isxv, @rx0, @lx1, @rx1, 4.0)        // Right green triangle

    //drawTriangle(vsxv, isxv, @lx0, @lx1, @tx0, col_white)  // Left white triangle
    //drawTriangle(vsxv, isxv, @rx0, @rx1, @tx1, col_white)  // Right white triangle
  }
  
  // Now draw coloured triangles for the bases of the dna sequence
  //IF FALSE DO // Normally commented out
  FOR i = 1 TO genomev!0 DO
  { // x, y, z are the coords of the mid point of the leading edge
    // of this triangle and its orientation is given by matrix m.
    LET FLT lx = x + genomeradius*swx  // Left vertex of the triangle 
    LET FLT ly = y + genomeradius*swy 
    LET FLT lz = z + genomeradius*swz 

    LET FLT rx = x - genomeradius*swx  // Right vertex of the triangle 
    LET FLT ry = y - genomeradius*swy 
    LET FLT rz = z - genomeradius*swz 

    LET X = genomev!i // The base number of the current base
    LET Y = ?         // Next base number, if any
    LET XY = ?
    
    nx := x + genomesep*stx     // The mid point of the leading 
    ny := y + genomesep*sty     // edge of the next triangle
    nz := z + genomesep*stz

    drawTriangle(vsxv, isxv, @lx, @rx, @nx, letno2colno(X))

    x, y, z := nx, ny, nz

    IF i = genomev!0 BREAK // Just drawn the last triangle
    //IF i >= 10 BREAK // For debugging only draw a few triangles

    // Update the the orientation matrix for the next triangle

    Y  := genomev!(i+1)
    XY := X<<2 | Y

    // Multiply the orientation matrix by the XY rotation matrix.
    mat3mulmmm(matrixv!XY, m, m)
  }

  // Draw the final black rectangle
  //IF FALSE DO   // Normally commented out
  { LET FLT lx0 =  x + genomeradius*swx  // (swx,swy,swz) is the unit
    LET FLT ly0 =  y + genomeradius*swy  // vector in direction w.
    LET FLT lz0 =  z + genomeradius*swz

    LET FLT rx0 =  x - genomeradius*swx
    LET FLT ry0 =  y - genomeradius*swy
    LET FLT rz0 =  z - genomeradius*swz

    LET FLT x1 = x + 2.0*stx     // The mid point of the trailing
    LET FLT y1 = y + 2.0*sty     // edge of the final rectangle.
    LET FLT z1 = z + 2.0*stz

    LET FLT lx1 = x1 + genomeradius*swx
    LET FLT ly1 = y1 + genomeradius*swy
    LET FLT lz1 = z1 + genomeradius*swz

    LET FLT rx1 = x1 - genomeradius*swx
    LET FLT ry1 = y1 - genomeradius*swy
    LET FLT rz1 = z1 - genomeradius*swz

    // Draw the rectangle as two triangles
    drawTriangle(vsxv, isxv, @rx0, @lx1, @lx0, col_black)  // Left  triangle
    drawTriangle(vsxv, isxv, @rx0, @lx1, @rx1, col_black)  // Right triangle
  }
  

  pushval(@dupb, 4)      // Draw triangles
  pushval(@dupb, ivec!0) // The number of indices to process
  pushval(@dupb, 0)      // Position of the first index

  
  vertexv  := vvec
  indexv   := ivec
  displayv := dvec

  //IF FALSE DO
  IF debug DO
  { // Output the vertex and index data
    // as a debugging aid
    writef("*nVertex data*n")
    FOR i = 1 TO vertexv!0 DO
    { IF i MOD 4 = 1 DO writef("*n%i3: ", i-1)
      writef(" %8.3f", vertexv!i)
    }
    writef("*n*nIndex data*n")
    FOR i = 1 TO indexv!0 DO
    { IF i MOD 10 = 1 DO writef("*n%i6: ", i-1)
      writef(" %i5", indexv!i)
    }
    writef("*n*nDisplay data items*n")
    FOR i = 1 TO displayv!0 - 1 BY 3 DO
      writef(" %i5  %i5  %i5*n",
             displayv!i, displayv!(i+1), displayv!(i+2)) // mode n offset
    newline()
    abort(1278)
  }

  RESULTIS TRUE
}

AND rdangles(filename) BE
{ LET FLT an, FLT aw, FLT ah = 0, 0, 0
  LET XYav = ?
  LET instream = findinput(filename)
  UNLESS instream RETURN
///writef("rdangles: reading file %s*n", filename)
  selectinput(instream)

  { LET ch = rdch()
    LET XY = -1
    LET FLT t, FLT w, FLT l = 1.0, 1.0, 1.0

    SWITCHON ch INTO
    { DEFAULT: LOOP

      CASE '#':
         ch := rdch() REPEATUNTIL ch='*n' |
	                          ch=endstreamch
         UNLESS ch=endstreamch LOOP
	 // Fall through
      CASE endstreamch:
        endstream(instream)
	RETURN

      CASE 'A': CASE 'a': XY :=  0; ENDCASE
      CASE 'C': CASE 'c': XY :=  4; ENDCASE
      CASE 'G': CASE 'g': XY :=  8; ENDCASE
      CASE 'T': CASE 't': XY := 12; ENDCASE
    }
    ch := rdch()
    SWITCHON ch INTO
    { DEFAULT: LOOP

      CASE endstreamch:
        endstream(instream)
	RETURN


      CASE 'A': CASE 'a':               ENDCASE
      CASE 'C': CASE 'c': XY := XY + 1; ENDCASE
      CASE 'G': CASE 'g': XY := XY + 2; ENDCASE
      CASE 'T': CASE 't': XY := XY + 3; ENDCASE
    }
    XYav := anglesv!XY
    
//abort(7166)
    
    an := deg2rad(readflt()) // Angle about N
    aw := deg2rad(readflt()) // Angle about W
    ah := deg2rad(readflt()) // Angle about H
    //writef("rdangles: %s %7.1f, %7.1f, %7.1f*n",
    //        XY2str(XY), rad2deg(an), rad2deg(aw), rad2deg(ah))
    setangles(XY, an, aw, ah)
    //abort(9166)
  } REPEAT
}

LET prpairs() BE FOR XY = 0 TO 15 DO prpair(XY)

AND prpair(XY) BE
{ LET av = anglesv!XY
  LET mv = matrixv!XY
  writef("%s angles %7.1f %7.1f %7.1f*n",
          XY2str(XY), rad2deg(av!0), rad2deg(av!1), rad2deg(av!2))
  writef("%s matrix", XY2str(XY))
  FOR i = 0 TO 8 DO writef(" %5.3f", mv!i)
  newline()
}

LET setinitstate() BE
{ blklist, blkp, blkt := 0, 0, 0

  pi := 3.141592653
  f0 := 0.0
  f1 := 1.0
  col_white := 4.0
  col_black := 5.0


  anglesv := newvec(15) // anglesv!XY-> [an,aw,ah ]  Rotation angles
  matrixv := newvec(15) // matrixv!XY-> [m0,m1,m2,  m3,m4,m5, m6,m7,m8 ]
//writef("anglesv=%n matrixv=%n*n", anglesv, matrixv)
	
  projectionMatrix := newvec(15)
  workMatrix       := newvec(15)

//writef("projectionMatrix=%n workMatrix=%n*n",
//        projectionMatrix, workMatrix)

  // Setup the initial values of anglesv and matrixv.
  FOR XY = 0 TO 15 DO
  { LET av = newvec(2) // -> [3 rotation angles]
    LET mv = newvec(8) // -> [9 rotation matrix elements]
    anglesv!XY, matrixv!XY := av, mv
    av!0, av!1, av!2 := f0, f0, f0
    mat3set(mv, f1,f0,f0, f0,f1,f0, f0,f0,f1)
    //writef("setinitstate: %s av=%n mv=%n*n", XY2str(XY), av, mv)
  }

  //abort(1000)
  // Set the default segment angles

  setangles(AA, deg2rad(0.0),   f0,   f0)
  setangles(AC, deg2rad(0.0),   f0,   f0)
  setangles(AG, deg2rad(0.0),   f0,   f0)
  setangles(AT, deg2rad(0.0),   f0,   f0)

  setangles(CA, deg2rad(0.0),   f0,   f0)
  setangles(CC, deg2rad(0.0),   f0,   f0)
  setangles(CG, deg2rad(0.0),   f0,   f0)
  setangles(CT, deg2rad(0.0),   f0,   f0)

  setangles(GA, deg2rad(0.0),   f0,   f0)
  setangles(GC, deg2rad(0.0),   f0,   f0)
  setangles(GG, deg2rad(0.0),   f0,   f0)
  setangles(GT, deg2rad(0.0),   f0,   f0)

  setangles(TA, deg2rad(0.0),   f0,   f0)
  setangles(TC, deg2rad(0.0),   f0,   f0)
  setangles(TG, deg2rad(0.0),   f0,   f0)
  setangles(TT, deg2rad(0.0),   f0,   f0)
//prpairs()
//abort(4478)
  genomeradius := 0.95 // In nms (=1.90/2)
  genomesep    := 0.34 // In nms
  genomeradius := 0.25
  genomesep    := 1.000

  currpair := AA             // Current base pair is AA
  curraxis := 0              // Current axis is t
  
  incrementdegrees := 0.0625    // The amount in degrees that angles are changed
                             // by the # and ~ commands.
  incrementradians := deg2rad(incrementdegrees)
  
  incrementstate  := -1      // =0 to 3 for the first letter of a segment pair
                             // has just been pressed.

  VertexBuffer := 0          // No vertex buffer yet.
  IndexBuffer  := 0          // No index buffer yet.
}

LET start() = VALOF
{ LET argv = VEC 50

  stdin  := input()
  stdout := output()

  UNLESS rdargs("FROM,ANGLES,SEED=X/n, Y/s,-d/s", argv, 50) DO
  { writef("Bad arguments for dnaplot*n")
    RESULTIS 0
  }

  fromfilename   := "seq1"
  fromfilename   := "seq045512"
  anglesfilename := "angles"
  
  IF argv!0 DO                    // FROM     Read the genome from file
    fromfilename := argv!0
  IF argv!1 DO                    // ANGLES   Read file of angle settings
    anglesfilename := argv!1
  setseed(9)
  IF argv!2 DO setseed(!(argv!2)) // SEED=X/n Seed for choosing randome angles
  debug   := argv!4               // -d/s     Output debugging info

  wrhelp()

  setinitstate()
   
  IF anglesfilename DO rdangles(anglesfilename)
  //prpairs()
  IF argv!3 DO setrandomangles()     // Y/s


  FOR i = 1 TO 10 DO setrandomangles() // Choose an interesting shape for seq1

  genomev := rdGenome(fromfilename)

  UNLESS genomev DO
  { LET v = TABLE 17,
    // A test sequence containing every base pair just once.
    // A A C C G G T T A T C T G A G C A
       0,0,1,1,2,2,3,3,0,3,1,3,2,0,2,1,0
    genomev := getvec(v!0) // So that freevec can be called later
    UNLESS genomev DO
    { writef("Failed to read the genome file %s*n", fromfilename)
      GOTO fin
    }
    FOR i = 0 TO v!0 DO genomev!i := v!i
 }	 

  // genomev!1 to genomev!upb now hold the base numbers where
  // upb = genomev!0

  tstanglet := 0.01  // Only used when debugging
  tstanglew := 0.02
  tstanglel := 0.03
  tstanglet := 0.00  // Only used when debugging
  tstanglew := 0.00
  tstanglel := 0.00
  tstdirection := 0

  IF FALSE DO
  IF debug DO
  { LET len = genomev!0               // Subscript of the last element in v
    IF len>500 DO len := 500
    
    writef("Start of genome: %s*n",
           fromfilename=0 -> "test data", fromfilename)
    FOR i = 1 TO len DO
    { LET letno = genomev!i
      IF letno<0 BREAK
      wrch(letno2ch(letno))
      IF i MOD 50 = 0 DO newline()
    }
    newline()
//    abort(6664)
  }

  initImageWindow()

  // Initialise the state

  done     := FALSE
  stepping := TRUE

  // Set the initial platform location some distance due south of
  // the first base of the genome.
  //platformn, platformw, platformh := -20.00, 2.0, 1.0
  //platformn, platformw, platformh :=  -5.00, 0.0, 0.0
  
  platformn, platformw, platformh :=   -2822.00, 0.0, 0.0
  platformn, platformw, platformh :=   -0700.00, 5.0, 0.0
    
  platformtdot := 100.0 // Initial t speed of the platform.
  platformwdot :=   0.0 // Initial w speed of the platform.
  platformldot :=   0.0 // Initial l speed of the platform.
  
  // Set the initial orientation of the platform.

  ptn, ptw, pth :=  1.0, 0.0, 0.0 // The first genome base is represented
  pwn, pww, pwh :=  0.0, 1.0, 0.0 // by a triangle in the N-W plane pointing
  pln, plw, plh :=  0.0, 0.0, 1.0 // in the northerly direction.

  heading    := 0    // Platform angles in integer degrees
  climbangle := 0
  bankangle  := 0
  
  direction := 0 // The camera initially points in the same direction
                 // as the platform.

  // Set the initial platform rotation rates
  prtdot, prwdot, prldot := 0.0000, 0.0000, 0.0000

  newline()

  UNTIL done DO
  { //writef("Calling processevents()*n")
    processevents()
    //writef("Returned from processevents()*n")
    //abort(8761, modelchanged)

    // If any genome angles have changed we must re-construct the model
    // and send it to GL.
    WHILE modelchanged DO
    { writef("Building the model with new angles*n")
 //abort(1724)
      modelchanged := FALSE

      // Create the modified genome model in user (non GL) space
      mkGenomeModel()

      // vertexv holds the vertex data, vertev!0 holds its upb
      // indexv holds the indices, indexv!0 holds its upb
      // displayv holds the display triples, displayv!0 holds its upb

      // Copy the vertices [x, y, z, colno] to GL space

      // Delete previous GL vertex and index buffers if they exist.

      IF VertexBuffer DO
      { writef("Deleting buffer %n*n", VertexBuffer)
        sys(Sys_gl, gl_DeleteBuffer, VertexBuffer)
      }
      IF IndexBuffer  DO
      { writef("Deleting buffer %n*n", IndexBuffer)
        sys(Sys_gl, gl_DeleteBuffer, IndexBuffer)
      }

      // Create a new Vertex buffer in GL space and fill it with data.
      VertexBuffer := sys(Sys_gl, gl_GenVertexBuffer, vertexv!0, vertexv+1)

      // VertexBuffer is the name (a positive integer) of the vertex buffer.
      writef("dnaplot: VertexBuffer=%n*n", VertexBuffer)
      //abort(1001)

      // Tell GL the positions in the vertex data of the xyz fields,
      // ie the first 3 words of each 4 word vertex item.
      //writef("start: Calling gl_EnableVertexAttribArray  VertexLoc==%n*n",
      //        VertexLoc)
      // VertexLoc is the location of the variable g_vVertex used
      // by the vertex shader.
      sys(Sys_gl, gl_EnableVertexAttribArray, VertexLoc);
      sys(Sys_gl, gl_VertexData,
                  VertexLoc,     // Attribute number for xyz data
                  3,             // 3 floats for xyz
                  4,             // 4 floats per vertex item in vertexData
                  0)             // Offset in 32 bit words of the xyz data

      //writef("start: VertexData xyz data copied to graphics object %n*n",
      //       VertexBuffer)

      // Tell GL the position in vvec of the colno field,
      // ie word at offset 3 of each 4 word vertex item.
      sys(Sys_gl, gl_EnableVertexAttribArray, ColourNoLoc);
      sys(Sys_gl, gl_VertexData,
                  ColourNoLoc,    // Attribute number of the colour nnumber field
                  1,             // 1 floats for colour data
                  4,             // 4 floats per vertex item
                  3)             // Offset in words of the colour nnumber field

      freevec(vertexv) // Free vertexv since all its elements have
                       // been copied to the graphics server.
      vertexv := 0

      // Send the indices to GL.

      //writef("start: Number of indices is %n*n", indexv!0)
      //writef("start: Calling GenIndexBuffer*n")
      IndexBuffer  := sys(Sys_gl, gl_GenIndexBuffer, indexv!0, indexv+1)

      //writef("start: IndexData copied to GL object %n*n", IndexBuffer)

      freevec(indexv) // Free ivec since all its elements have
                      // been copied to the graphics server.
      indexv := 0
    }
    standardiseM3(@ptn)

    // Only move and rotate the platform if stepping is TRUE
    //IF stepping DO
    { // Rotate the platform based on the rotation rates set
      // by the aileron, elevator and rudder controls.
      //writef("Calling rotate3(%9.5f, %9.5f, %9.5f)*n",
      //        prtdot, prwdot, prldot)

      IF debug DO
        prtdot, prwdot, prldot := tstanglet, tstanglew, tstanglel
//prtdot, prwdot, prldot := 0.1, 0.0, 0.0
//prmat3(@ptn)
//writef("rotate3(%9.6f %9.6f %9.6f*n", prtdot, prwdot, prldot)
      rotate3(@ptn, prtdot, prwdot, prldot)
//prmat3(@ptn)
//newline()
//abort(8765)

      IF stepping DO
      { // Move the camera platform based on its current velocity
        // vector (platformtdot, platformwdot, platformldot).
        platformn := platformn + (ptn*platformtdot +
                                  pwn*platformwdot +
	  		 	  pln*platformldot ) / 100.0
        platformw := platformw + (ptw*platformtdot +
                                  pww*platformwdot +
				  plw*platformldot ) / 100.0
        platformh := platformh + (pth*platformtdot +
                                  pwh*platformwdot +
				  plh*platformldot ) / 100.0
      }
    }

    // Change the model coords so that they are relative to the
    // platform (ie camera) position.

    mat4set( projectionMatrix,
              1.0,        0.0,        0.0,         0.0,  // column 1
              0.0,        1.0,        0.0,         0.0,  // column 2
              0.0,        0.0,        1.0,         0.0,  // column 3
             -platformn, -platformw, -platformh,   1.0)  // column 4

    // Note that
    //  ( 1  0  0  -platformn )  x  ( n )  =>  ( n-platformn )
    //  ( 0  1  0  -platformw )  x  ( w )  =>  ( w-platformw )
    //  ( 0  0  1  -platformh )  x  ( h )  =>  ( h-platformh )
    //  ( 0  0  0       1     )  x  ( 1 )  =>  (     1       )
    // ie projectionMatris makes the origin of world NWH coordinates
    // the position of the camera platform.

IF debug DO
    { writef("The platform location (%7.4f %7.4f %7.4f)*n",
              platformn, platformw, platformh)
      //prmat4(projectionMatrix)
      tstprojectionmatrix(projectionMatrix) // Seems OK
      direction := tstdirection
    }

    { LET tm = VEC 8  // Temp matrix
      // Make the camera orientation the same as that of the platform.
      // Then rotate it based on the value of direction.

      ctn, ctw, cth := ptn, ptw, pth  // t axis, column 1
      cwn, cww, cwh := pwn, pww, pwh  // w axis, column 2
      cln, clw, clh := pln, plw, plh  // l axis, column 3

      IF debug DO
      { writef("The platform orientation matrix is:*n")
        prmat3(@ptn)
      }
      
      // Unless direction=0 rotate the camera appropriately.

      IF 1 <= direction <= 7 DO
        // Clockwise the camera clockwize about axis l
	// by direction*45 degrees.
        rotate3l(@ctn, (FLOAT direction) * pi / 4.0)

      IF direction=8 DO
        // Clockwise the camera clockwize about axis w
	// by 22.5 degrees.
	rotate3w(@ctn,  pi/8.0)  // ie point camera down a bit.

      IF direction=9 DO
        // Clockwise the camera anti-clockwize about axis w
	// by 22.5 degrees.
	rotate3w(@ctn, -pi/8.0)  // ie point camera up a bit.

      // Note that direction=0 causes no rotation.

      // Apply the inverse rotation matrix.
      // Note that the inverse of a rotation matrix is just its transpose.
      // ie the transpose of
      //    ( ctn  cwn  cln )        ( ctn  ctw  cth )
      //    ( ctw  cww  cwh ) namely ( cwn  cww  clw )
      //    ( cth  clw  clh )        ( cln  cwh  clh )
      
      mat4set( workMatrix,
                      ctn,   cwn,  cln,  0.0,  // column 1
                      ctw,   cww,  clw,  0.0,  // column 2
                      cth,   cwh,  clh,  0.0,  // column 3
                      0.0,   0.0,  0.0,  1.0)  // column 4

      sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)
      // projectionMatrix now will transform model NWH coordinates
      // to camera twl coordinates. If P is a point (n,w,l) of the genome
      // Then
      
      // ( ctn  ctw  cth  0 ) x ( 1  0  0  -platformn ) x ( n )  =
      // ( cwn  cww  cwh  0 ) x ( 0  1  0  -platformw ) x ( w )
      // ( cln  clw  clh  0 ) x ( 0  0  1  -platformh ) x ( h )
      // (  0    0    0   1 ) x ( 0  0  0      1      ) x ( 1 )

      // ( ctn  ctw  cth  0 ) x ( n-platformn ) =
      // ( cwn  cww  cwh  0 ) x ( w-platformw )
      // ( cln  clw  clh  0 ) x ( h-platformh )
      // (  0    0    0   1 ) x (      1      )

      // ( pt ) =
      // ( pw )
      // ( pl )
      // (  1 )

      // where, for instance,
      // pt = ctn x (n-platformn) + ctw x (w-platformw) + cth x (h-platformh)

      // This is the inner product of the unit vector in direction t and the
      // vector from the camera to P. It thus equals the t component of P.
      // 
      // 

      IF debug DO
      { writef("Camera direction %n*n", direction)
        //writef("So the camera orientation matrix:*n")
        //prmat3(@ctn)
        tstprojectionmatrix(projectionMatrix)
      }

      // Modify the projection matrix to cause it to transform real
      // world (N,W,H) coordinates to GL camera coordinates (x,y,z).
      //
      //    t corresponds to -z     
      //    w corresponds to -x
      //    l corresponds to  y

      // The matriz for this transformation is

      // ( x ) = (  0 -1  0  0 ) x ( t )  =  ( -w )  
      // ( y )   (  0  0  1  0 )   ( w )     (  l )
      // ( z )   ( -1  0  0  0 )   ( l )     ( -t )
      // ( w )   (  0  0  0  1 )   ( 1 )     (  1 )

      mat4set( workMatrix,
                      0.0,   0.0, -1.0,  0.0,  // column 1
                     -1.0,   0.0,  0.0,  0.0,  // column 2
                      0.0,   1.0,  0.0,  0.0,  // column 3
                      0.0,   0.0,  0.0,  1.0)  // column 4
      sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)

      IF debug DO
      { writef("After adjusting to GL camera (x,y,z) coordinates*n")
        //prmat4(projectionMatrix)
        tstprojectionmatrix(projectionMatrix)
      }
      
      // Note that the frustum transformation reverses the direction
      // of the z axis making increasing z corresponding to greater
      // distance from the camera.

      // Define the truncated pyramid for the view projection
      // using the frustrum transformation.

      //tstfrustrum()  // Debugging call

      glSetPerspective(workMatrix,
                              1.0,   // Field of view at unit distance
                     screenaspect,   // Aspect ratio
                              0.2,   // Distance to near limit
                           5000.0)   // Distance to far limit
      sys(Sys_gl, gl_M4mulM4, workMatrix, projectionMatrix, projectionMatrix)

      IF debug DO
      { writef("projectionMatrix now transforms model (N,W,H) coordinates*n")
        writef("to GL camera standardised coordinates (x,y,z)*n")
        tstprojectionmatrix(projectionMatrix)
      }
    }

    // Send the resulting projection matrix to the uniform variable "matrix"
    // for use by the vertex shader.
    sys(Sys_gl, gl_UniformMatrix4fv, MatrixLoc, glprog, projectionMatrix)
    //writef("projectionMatrix is now defined and sent to GL*n")

    // Draw a new image

    sys(Sys_gl, gl_ClearColour,
        0.1, 0.2, 0.2, 1.0)     // Light gray
    sys(Sys_gl, gl_ClearBuffer) // Clear colour and depth buffers
//sys(Sys_gl, gl_SwapBuffers)
//delay(1000)

    sys(Sys_gl, gl_Enable, GL_DEPTH_TEST) // This call is neccessary
    // The next call is unnecessary since LESS is the default setting.
    sys(Sys_gl, gl_DepthFunc, GL_LESS)    // This is the default

    // Positive Z is into the screen, so a pixel is written with
    // depth < buffer depth takes precedence.
    // Remember that the depth test is performed after all other
    // transformations have been done.


    // Tell OpenGL to draw all the 3D coloured triangles representing the
    // genome using the recently computed projection matrix.
    drawmodel()

    sys(Sys_gl, gl_SwapBuffers)

    wrplatformangles()
    
//delay(1_000) // Delay for a while.
    IF debug DO
    { //writef("The projection matrix for this frame was:*n")
      //prmat4(projectionMatrix)
      abort(1007)
    }
  }

  sys(Sys_gl, gl_DisableVertexAttribArray, VertexLoc)
  sys(Sys_gl, gl_DisableVertexAttribArray, ColourNoLoc)

fin:
  IF genomev DO freevec(genomev)

  // Free the self expanding vector space.
  IF vertexv  DO freevec(vertexv)
  IF indexv   DO freevec(indexv)
  IF displayv DO freevec(displayv)
  
  // Free all the block in the blklist.
  WHILE blklist DO
  { LET blk = blklist
    blklist := !blk
//writef("start: freeing blklist blk %n*n", blk)
    freevec(blk)
  }

//  freevec(dvec) // Free the display items vector.
  delay(0_050)
  sys(Sys_gl, gl_Quit)
  newline()
  RESULTIS 0
}//xxx

AND wrplatformangles() BE
{ // We calculate the bank angle by aligning the t axis of the
  // platform with direction N by rotating it about axis l and
  // then w. This causes the w axis to lie in in the W-H plane.
  // Since rotations about axes l and w do no affect the angle
  // of bank and since axis w is in the W-H plane the angle
  // between the w axis and the W axis is the angle of bank.
  // The coordinates of the unit point on the t axis is
  // (ptn,ptw,pth). We can find a rotation about Hthat causes
  // this axis to lie in the N-H plane and then find a rotation
  // about W to cause it to be aligned with the N axis. If we
  // apply the same rotations to the w axis we will get (0,c,s)
  // where c and s are the cosine and sine of the angle of bank.
  LET FLT ct, FLT st = 0.0, 0.0 // For the rotation about H.
  LET FLT cw, FLT sw = 0.0, 0.0 // For the rotation about W

  LET FLT tn1 = ptn    // For the t axis unit vector
  LET FLT tw1 = ptw    // after the rotation about axis H.
  LET FLT th1 = pth

  LET FLT tn2 = ptn    // For the t axis unit vector
  LET FLT tw2 = ptw    // after the rotation about axis W.
  LET FLT th2 = pth

  LET FLT wn1 = pwn    // For the w axis unit vector
  LET FLT ww1 = pww    // after the rotation about axis H.
  LET FLT wh1 = pwh

  LET FLT wn2 = pwn    // For the w axis unit vector
  LET FLT ww2 = pww    // after the rotation about axis W.
  LET FLT wh2 = pwh

  LET FLT r = sys(Sys_flt, fl_sqrt, ptn*ptn+ptw*ptw) 
  // r is the length of the projection of the t axis vector
  // onto the NOW plane.
  // ie r = sqrt(ptn^2+ptw^2)

  // When viewed from a distant point on the H axis the t unit
  // vector looks as follows:
  //          W
  //          ^    t(ptn,ptw,pth)
  //          |   /
  //          |  /              The length of Ot is r
  //          | /-
  //          |/ a \
  //          O------------------------->N

  // To cause the t axis vector to lie in the NOH plane, we must rotate
  // the twl coordinates anti-clockwise by the angle a about axis H.
  // The matrix to do this is:
	
  //    (  cos a  sin a   0 )
  //    ( -sin a  cos a   0 )
  //    (    0      0     1 )

  // where cos a = ptn/r and sin a = ptw/r
  // The required matrix is thus:
	
  //    (  ptn/r  ptw/r   0 )       Matrix M1
  //    ( -ptw/r  ptn/r   0 )
  //    (     0      0    1 )
	
  // Applying M1 to ( ptn ) gives (   (ptn^2+ptw^2)/r   ) = (  r  ) 
  //                ( ptw )       ((-ptw*ptn+ptn*ptw)/r )   (  0  )
  //                ( pth )       (        pth          )   ( pth )

  // Applying M1 to ( pwn ) gives (( ptn*pwn+ptw*pww)/r )
  //                ( pww )       ((-ptw*pwn+ptn*pww)/r )
  //                ( pwh )       (        pwh          )

  // The above calculation can be inaccurate if r is small and may
  // even cause a division by zero exception. This can be avoided
  // by not performing the rotations about H and W if r<1e-6.
  // The t axis, in this case, is sufficiently well aligned with
  // direction N.

  // When viewed from a distant point on the W axis the rotated t vector
  // is as follows:
  //          N
  //          ^    t(r,0,pth)
  //          |   /
  //          |--/        The length of Ot is 1
  //          |b/
  //          |/
  //          O------------------------->H

  // To cause the vector Ot to lie in the NOW plane we must rotate
  // twl coordinates clockwise about the W axis by an angle b.
  // The matrix to do this is
	
  //    (  cos b   0   sin b )
  //    (    0     1     0   )
  //    ( -sin b   0   cos b )

  // where cos b = r and sin b = pth.
  // The required matrix is:
	
  //    (   r   0  pth )       Matrix M2
  //    (   0   1   0  )
  //    ( -pth  0   r  )

  // Applying M2 to (  r  ) gives (   r^2+pth^2  ) = ( 1 ) 
  //                (  0  )       (       0      ) = ( 0 )
  //                ( pth )       ( -pth*r+r*pth ) = ( 0 )
  // as expected.

  // If we apply M1 and M2 to the w axis unit vector, it will be
  // transformed to lie ine the WOH plane and its W and H coordinates
  // will be the cosine and sine of the angle of bank. The angle can
  // then obtained using arctan.

  IF r > 1e-6 DO
  { // The rotations are only needed if r is large enough.
	
    // The result of applying M1 to (tn,tw,th) is
    //tn1 := r        // Not needed
    //tw1 := 0.0      // Not needed
    //th1 := pth      // Not changed
    // The result of applying M1 to (pwn,pww,pwh) is

    wn1 := ( ptn*pwn+ptw*pww)/r // w unit vector after the rotation about H
    ww1 := (-ptw*pwn+ptn*pww)/r
    wh1 := pwh
  }

  // Now M2 to the new version of (wn1,ww1,wh1)
  //wn2 := r*wn1+th*wh1   // Not needed 
  ww2 := ww1              // cosine of the angle of bank
  wh2 := -pth*wn1+r*wh1   // sine of the angle of bank

  //writef("*nsin and cos bank angle %9.6f %9.6f*n", wh2, ww2)
  bankangle := FIX rad2deg(sys(Sys_flt, fl_atan2, wh2, ww2))

  climbangle := FIX rad2deg(sys(Sys_flt, fl_atan2, pth, ptn))
  heading    := FIX rad2deg(sys(Sys_flt, fl_atan2, ptw, ptn))

  IF heading < 0 DO heading := 360 + heading

IF FALSE DO
  writef("*c%4.2fN %4.2fW Alt %4.2f Hdg %i3 Climb %i4 Bank %i3 *
         *spd %4.2fF %4.2fL %4.2fU        *c",
          platformn,    platformw,    platformh,
          heading,      climbangle,   bankangle,
          platformtdot, platformwdot, platformldot
        )
  //newline()
  //abort(4450)
}

AND mat4tst(m, FLT x, FLT y, FLT z, FLT w) BE
{ // Test a 4x4 matrix
  LET FLT mx, FLT my, FLT mz, FLT mw = f0, f0, f0, f0
  mat4mulmvv(m, @x, @mx)
  //prmat4(m)
  writef("(%7.4f %7.4f %7.4f) => (%7.4f %7.4f %7.4f)*n",
           x,y,z,                 mx/mw,my/mw,mz/mw)
}

AND tstfrustrum() BE
{ // Test the frustrum function
  LET m = VEC 15
  LET FLT fov    = 0.5  // Field of view at unit distance
  LET FLT aspect = 2.0  // Aspect ratio, assume screen width/height = 2
  LET FLT n      = 1.0  // Positive distance to near plane
  LET FLT f      = 10.0 // Positive distance to far plane
  
  writef("Testing glSetPerspective*n")
  writef("fov=%6.4f  aspect=%6.4f n=%6.4f n=%6.4f*n", fov, aspect, n, f)
  glSetPerspective(m, fov, aspect, n, f)
  prmat4(m)

  //                                    ------*----
  //                           ---------      |
  //                 -----*----               |
  //      camera  *-------|-------------------|------> Negative z direction
  //              |  -----*----               |
  //              |       ^    ---------      |
  //              |       |             ------*----
  //              |<--n-->|                   |  n = near distance
  //              |<-------------f----------->|  f = far distance
  //                      |                   |
  //                 near plane           far plane

 
  // Vertices of the near rectangle
  tstpoint(m,  0.5,  0.25,  -1.0)  // near top right
  tstpoint(m, -0.5,  0.25,  -1.0)  // near top left
  tstpoint(m, -0.5, -0.25,  -1.0)  // near bottom leftt
  tstpoint(m,  0.5, -0.25,  -1.0)  // near bottom right

  // Vertices of the far rectangle
  tstpoint(m,  5.0,  2.50, -10.0)  // far top right
  tstpoint(m, -5.0,  2.50, -10.0)  // far top left
  tstpoint(m, -5.0, -2.50, -10.0)  // far bottom leftt
  tstpoint(m,  5.0, -2.50, -10.0)  // far bottom right

  // Test points out of range
  tstpoint(m,  0.0,  0.0, -11.0)  // Too far
  tstpoint(m,  0.0,  0.0,  -0.3)  // Too near
  tstpoint(m,  5.0,  0.0,  -9.0)  // Too far to the right
  tstpoint(m, -5.0,  0.0,  -9.0)  // Too far to the left
  tstpoint(m,  0.0,  2.5,  -9.0)  // Too high
  tstpoint(m,  0.0, -2.5,  -9.0)  // Too low
  abort(1009)
}

AND tstprojectionmatrix(m) BE
{ //tstpoint(m, 30.0, 0.0, 0.0)

  ///tstpoint(m, 30.0,  10.0,  10.0)
  ///tstpoint(m, 30.0, -10.0,  10.0)
  ///tstpoint(m, 30.0, -10.0, -10.0)
  ///tstpoint(m, 30.0,  10.0, -10.0)

  ///tstpoint(m, 60.0,  10.0,  10.0)
  ///tstpoint(m, 60.0, -10.0,  10.0)
  ///tstpoint(m, 60.0, -10.0, -10.0)
  //tstpoint(m, 60.0,  10.0, -10.0)

  tstpoint(m,  -1.0,   1.0,   1.0)  // 2x2 square at N=-1
  tstpoint(m,  -1.0,  -1.0,   1.0)
  tstpoint(m,  -1.0,  -1.0,  -1.0)
  tstpoint(m,  -1.0,   1.0,  -1.0)
  
  tstpoint(m,   1.0,   1.0,   1.0)  // 2x2 square at N=-1
  tstpoint(m,   1.0,  -1.0,   1.0)
  tstpoint(m,   1.0,  -1.0,  -1.0)
  tstpoint(m,   1.0,   1.0,  -1.0)
  
newline()
}

AND tstpoint(m, FLT x, FLT y, FLT z) BE
{ LET FLT x1, FLT y1, FLT z1, FLT w1 = x, y, z, 1.0
  LET FLT x2, FLT y2, FLT z2, FLT w2 = ?, ?, ?,   ?
  LET p1 = @x1
  LET p2 = @x2
  mat4mulmvv(m, p1, p2)
  x2 := x2/w2
  y2 := y2/w2
  z2 := z2/w2
  writef("%6.2f %6.2f %6.2f => %9.6f %9.6f %9.6f*n",
           x1,   y1,   z1,     x2,   y2,   z2)
}



AND initImageWindow() = VALOF
{ LET str  = VEC 20 // For the window title
  glAPIno := glInit()
  writef("glAPIno=%n*n", glAPIno)
  UNLESS glAPIno DO
  { writef("*nOpenGL not available*n")
    GOTO fin
  }

  //writef("start: calling glMkScreen*n")
  // Create an OpenGL window
  screenxsize := glMkScreen(concatstr("Genome from file: ",
                                       fromfilename -> fromfilename,
				                       "Test Genome",
				                       
				       str),
//                            1200, 800)
//                            800, 680)
                           1500, 700)
  screenysize := result2
  UNLESS screenxsize DO
  { writef("*nUnable to create an OpenGL window*n")
    RESULTIS 0
  }
  //writef("Screen Size is %n x %n*n", screenxsize, screenysize)
//abort(1000)
  glprog := sys(Sys_gl, gl_MkProg)
  //writef("dnaplot: glprog=%n*n", glprog);

  IF glprog<0 DO
  { writef("*nUnable to create a GL program*n")
    RESULTIS 0
  }

  // Read and Compile the vertex shader
  //writef("glstst: calling Compileshader(%n,TRUE,*"dnaplotVshader.sdr*")*n",glprog)
  Vshader := Compileshader(glprog, TRUE, "dnaplotVshader.sdr")
  //writef("dnaplot: Vshader=%n*n", Vshader)
//abort(8344)

// Read and Compile the fragment shader
  //writef("dnaplot: calling Compileshader(%n,FALSE,dnaplotFshader.sdr)*n",glprog)
  Fshader := Compileshader(glprog, FALSE, "dnaplotFshader.sdr")
  //writef("dnaplot: Fshader=%n*n", Fshader)

  // Link the program
  //writef("dnaplot: calling glLinkProg(%n)*n", glprog)
  UNLESS sys(Sys_gl, gl_LinkProgram, glprog) DO
  { writef("*ndnaplot: Unable to link a GL program*n")
    RESULTIS 0
  }
//abort(8345)

  //writef("dnaplot: calling glUseProgram(%n)*n", glprog)
  sys(Sys_gl, gl_UseProgram, glprog)

  // Get attribute locations after linking 
  VertexLoc  := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vVertex")
  ColourNoLoc := sys(Sys_gl, gl_GetAttribLocation, glprog, "g_vColourNo")

  //writef("dnaplot: VertexLoc=%n*n",   VertexLoc)
  //writef("dnaplot: ColourNoLoc=%n*n",  ColourNoLoc)

  // Get uniform locations after linking
  MatrixLoc := sys(Sys_gl, gl_GetUniformLocation, glprog, "matrix")
  //writef("dnaplot: MatrixLoc=%n*n",  MatrixLoc)

fin:
  RESULTIS 0
}

AND Compileshader(prog, isVshader, filename) = VALOF
{ // Create and compile a shader whose source code is
  // in a given file.
  // isVshader=TRUE  if compiling a vertex shader
  // isVshader=FALSE if compiling a fragment shader
  LET oldin = input()
  LET oldout = output()
  LET buf = 0
  LET shader = 0
  LET ramstream = findinoutput("RAM:")
  LET instream = findinput(filename)
  UNLESS ramstream DO
  { writef("Compileshader: Unable to open file %s*n*n", "RAM:")
    RESULTIS -1
  }
  UNLESS instream DO
  { writef("Compileshader: Unable to open file %s*n*n", filename)
    RESULTIS -1
  }

  //Copy shader program to RAM:
  selectoutput(ramstream)
  selectinput(instream)

  { LET ch = rdch()
    IF ch=endstreamch BREAK
    wrch(ch)
  } REPEAT

  wrch(0) // Place the terminating byte

  selectoutput(oldout)
  endstream(instream)
  selectinput(oldin)

  buf := ramstream!scb_buf

  shader := sys(Sys_gl,
                (isVshader -> gl_CompileVshader, gl_CompileFshader),
                prog,
                buf)

  endstream(ramstream)
  RESULTIS shader
}

AND deg2rad(FLT degrees) = degrees*pi/180.0

AND rad2deg(FLT radians) = radians*180.0/pi

AND drawTriangle(vsxv, isxv, a, b, c, colno) BE
{ // vsxv is the self expanding vector for the vertex data
  // isxv is the self expanding vector for the indices
//abort(1001)
//writef("%i5: %12.5f  %12.5f  %12.5f %3.1f*n",
//        indexp, a!0, a!1, a!2, colno)
  pushval(vsxv, a!0)
  pushval(vsxv, a!1)
  pushval(vsxv, a!2)
  pushval(vsxv, colno)
  pushval(isxv, indexp)
  indexp := indexp+1
  
//writef("%i5: %12.5f  %12.5f  %12.5f %3.1f*n",
//        indexp, b!0, b!1, b!2, colno)
  pushval(vsxv, b!0)
  pushval(vsxv, b!1)
  pushval(vsxv, b!2)
  pushval(vsxv, colno)
  pushval(isxv, indexp)
  indexp := indexp+1
  
//writef("%i5: %12.5f  %12.5f  %12.5f %3.1f*n",
//        indexp, c!0, c!1, c!2, colno)
  pushval(vsxv, c!0)
  pushval(vsxv, c!1)
  pushval(vsxv, c!2)
  pushval(vsxv, colno)
  pushval(isxv, indexp)
  indexp := indexp+1
}



AND drawmodel() BE
{ // Draw the primitives using vertex and index data held in
  // graphics objects as specified by the display items in dvec.
  FOR p = 1 TO displayv!0 BY 3 DO
  { LET d = @displayv!p
    LET mode   = d!0  // Points, Lines, Linestrip, etc.
    LET size   = d!1  // Number of index elements.
    LET offset = d!2  // Offset in the index vector.

//writef("drawmodel: p=%n mode=%n, size=%n offset=%n*n", p, mode, size, offset)

    sys(Sys_gl, gl_DrawElements,
                mode,     // 1=points, 2=lines, 3=linestrip, etc
                size,     // Number of index elements to use.
                4*offset) // The start position (bytes) in the index vector.
  }
}

AND wrhelp() BE
{ writef("*nCommand summary*n*n")
  writef("?          Output this help info*n")
  writef("Q          Quit*n")
  writef("S          Start/Stop stepping the platform*n")

 writef("=XYA val    Set an angle to val/1000*n")
  writef("             where X and Y are bases A, C, G or T*n")
  writef("             and A is the axis T, l or W*n")
  writef("Z          Set w and l angles to zero for all 16 base pairs*n")
  writef("Y          Randomly increment to the w and l angles of *n")
  writef("             all 16 base pairs*n")

  writef("F f        Increase the platform speed in direction t*n")
  writef("             by a large or small amount*n")
  writef("B b        Decrease the platform speed in direction t*n")
  writef("             by a large or small amount*n")
  writef("             by a large or small amount*n")
  writef("Right Left Increase/decrease the platform rate of rotation*n")
  writef("T t          about axis t*n")
  writef("< >        Increase/decrease the platform rate of rotation*n")
  writef("L l          about axis l*n")
  writef("Up Down    Increase/decrease the platform rate of rotation*n")
  writef("W w          about axis w*n")
  
  writef("H          Rotate the platform to point to the origin*n")
  writef("0 to 9     Select a camera direction relative to the platform*n")
  writef("P          Output the current orientation and other values*n")
  writef("W name     Write the angles data to file*n")
  newline()	
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ 
  DEFAULT:
    //writef("processevents: Unknown event type = %n*n", eventtype)
    LOOP

  CASE sdle_quit:             // 12
    writef("QUIT*n")
    sys(Sys_gl, gl_Quit)
    LOOP

  CASE sdle_keydown:
  { LET ch = eventa2
    //writef("*n%i3 ", eventa2)
    //IF 32 <= eventa2 < 127 DO writef("'%c'*n", eventa2)
    //UNLESS ch='F' | ch='f' |
    //       ch='L' | ch='l' |
    //       ch='U' | ch='u' DO preveventch := 0

    SWITCHON ch INTO
    { DEFAULT:  LOOP

      CASE '*n':
      CASE '*c':
                //writef("*nENTER pressed*n")
		incrementstate := -1
                LOOP

      CASE '?': wrhelp()
	        LOOP

      CASE 'q':
      CASE 'Q': done := TRUE
                LOOP

      CASE '=': // =XYA val  set a base pair angle
      { LET XY = 0
        LET ch1 = rdch()
        LET ch2 = rdch()
	LET XY = findXY(ch1, ch2)
	LET dir = finddir(rdch())
	LET FLT val = rdval()
	IF result2 | XY<0 | dir<0 DO
	{ writef("Bad =XYA val command*n")
	  LOOP
	}
	anglesv!(3*XY+dir) := val
	LOOP
      }

      //CASE 'F': // Adjust the camera platform speed in direction t
      //CASE 'f':
        TEST preveventch=ch
	THEN TEST prevspeed=0
	     THEN prevspeed := ch='F' -> 0.1, -0.1
	     ELSE prevspeed := prevspeed * 1.5
	ELSE platformtdot, prevspeed := 0.0, 0.0
	preveventch := ch
        platformtdot := platformtdot + prevspeed
	IF platformtdot >  500 DO platformtdot :=  500
	IF platformtdot < -500 DO platformtdot := -500
	writef("platformtdot=%7.4f prevspeed=%7.4f*n",
	        platformtdot, prevspeed)
        LOOP

      CASE 'F': // Increase platform rotation rate about direction t
	platformtdot := platformtdot + 0.05
	writef("platformtdot=%7.4f*n", platformtdot)
        LOOP

      CASE 'f': // Decrease platform rotation rate direction t
	platformtdot := platformtdot - 0.05
	writef("platformtdot=%7.4f*n", platformtdot)
        LOOP

      CASE 'L': // Increase platform rotation rate about direction w
	platformwdot := platformwdot + 0.05
	writef("platformwdot=%7.4f*n", platformwdot)
        LOOP

      CASE 'l': // Deaccelerate the camera platform in direction -w
	platformwdot := platformwdot - 0.05
	writef("platformwdot=%7.4f*n", platformwdot)
        LOOP

      CASE 'U': // Accelerate the camera platform in direction l
	platformldot := platformldot + 0.05
	writef("platformldot=%7.4f*n", platformldot)
        LOOP

      CASE 'u': // Deaccelerate the camera platform in direction -l
	platformldot := platformldot - 0.05
	writef("platformldot=%7.4f*n", platformldot)
        LOOP

      CASE '0':
        direction := 0
	writef("Looking forward*n")
	LOOP
      CASE '1':
        direction := 1
	writef("Looking forward left*n")
	LOOP
      CASE '2':
        direction := 2
	writef("Looking left*n")
	LOOP
      CASE '3':
        direction := 3
	writef("Looking back left*n")
	LOOP
      CASE '4': 
        direction := 4
	writef("Looking back*n")
	LOOP
      CASE '5':
        direction := 5
	writef("Looking back right*n")
	LOOP
      CASE '6':
        direction := 6
	writef("Looking right*n")
	LOOP
      CASE '7':
        direction := 7
	writef("Looking forward right*n")
	LOOP
      CASE '8':
        direction := 8
	writef("Looking forward down*n")
	LOOP
      CASE '9': 
        direction := 9
	writef("Looking forward up*n")
	LOOP
	
      CASE 'd':
      CASE 'D':
        TEST debug THEN debug := FALSE
	           ELSE debug := TRUE
	LOOP
		
      CASE 'p':
      CASE 'P':
        writef("*nPlatform direction cosines*n")
        writef("ptn=%9.6f ptw=%9.6f pth=%9.6f prtdot=%6.4f*n",
                ptn,      ptw,      pth,      prtdot)
        writef("pwn=%9.6f pww=%9.6f pwh=%9.6f prwdot=%6.4f*n",
                pwn,      pww,      pwh,      prwdot)
        writef("pln=%9.6f plw=%9.6f plh=%9.6f prldot=%6.4f*n",
                pln,      plw,      plh,      prldot)
	//writef("Platform t w l lengths %9.6f %9.6f %9.6f*n",
	//        glRadius3(ptn,ptw,pth),
        //        glRadius3(pwn,pww,pwh),
	//        glRadius3(pln,plw,plh))
	//writef("Cosines of angles: tw=%9.6f tl=%9.6f wl=%9.6f*n",
	//        ptn*pwn+ptw*pww+pth*pwh,
	//        ptn*pln+ptw*plw+pth*plh,
	//        pwn*pln+pww*plw+pwh*plh)

        newline()
        writef("Platform position: N=%7.4f W=%7.4f H=%7.4f*n",
	        platformn, platformw, platformh)
        writef("Platform speed in direction t is %7.4f*n", platformtdot)
	writef("Camera looking in direction %nrelative to the platform*n", direction)
	writef("*nBase pair angles about direction t, l and w in degrees*n")
	writef("Positive angles are clockwise rotations*n")
        FOR XY = AA TO TT DO
        { LET av = anglesv!XY
          writef("%s %9.6f %9.6f %9.6f*n",
                 XY2str(XY), rad2deg(av!0), rad2deg(av!1), rad2deg(av!2))
        }
	//abort(2919)
        LOOP

      CASE 'w':
      CASE 'W':
        wranglesdata()
        LOOP
	
      CASE 's':
      CASE 'S':
        stepping := ~stepping
	writef("Stepping %s*n", stepping -> "started", "stopped")
        LOOP

      CASE '#':
        doincrement(incrementradians)
        LOOP
      CASE '~':
        doincrement(-incrementradians)
        LOOP

      CASE '\':
        incrementdegrees := 0.5*incrementdegrees
        incrementradians := deg2rad(incrementdegrees)
        writef("*nincrementvdegrees=%9.6f*n", incrementdegrees)
        LOOP
	
      CASE '|':
        incrementdegrees := 2.0*incrementdegrees
        incrementradians := deg2rad(incrementdegrees)
        writef("*nincrementvdegrees=%9.6f*n", incrementdegrees)
        LOOP
			    
      CASE '>':CASE '.': // rotate anti-clockwise about l
        prldot := prldot - 0.0002
        IF prldot<-0.10 DO prldot := -0.10
	//writef("prldot= %9.4f  *n", prldot)
        LOOP

      CASE '<':CASE ',': // rotate clockwise about l
        prldot := prldot + 0.0002
	IF prldot> 0.10 DO prldot :=  0.10
	//writef("prldot= %9.4f  *n", prldot)
        LOOP

      CASE sdle_arrowdown: // rotate anti-clockwise about w
        prwdot := prwdot - 0.0002
        IF prwdot<-0.10 DO prwdot := -0.10
	//writef("prwdot= %9.4f  *n", prwdot)
        LOOP
	
      CASE sdle_arrowup: // rotate clockwise about w
        prwdot := prwdot + 0.0002
        IF prwdot> 0.10 DO prwdot :=  0.10
	//writef("prwdot= %9.4f  *n", prwdot)
        LOOP

      CASE sdle_arrowleft: // rotate anti-clockwise about t
        prtdot := prtdot - 0.0002
        IF prtdot<-0.10 DO prtdot := -0.10
	//writef("prtdot= %9.4f  *n", prtdot)
        LOOP

      CASE sdle_arrowright: // rotate clockwise about t
        prtdot := prtdot + 0.0002
        IF prtdot> 0.10 DO prtdot :=  0.10
	//writef("prtdot= %9.4f  *n", prtdot)
        LOOP

      CASE 'h':
      CASE 'H':
        // Point the camera platform towards the model origin with
	// direction w level.
        ptn := -platformn
        ptw := -platformw
        pth := -platformh

        IF ptn*ptn+ptw*ptw+pth*pth < 1.0 DO
	{ // The camera platform is already close to the model origin.
	  ptn, ptw, pth := 1.0, 0.0, 0.0
	  pwn, pww, pwh := 0.0, 1.0, 0.0
	  pln, plw, plh := 0.0, 0.0, 1.0
	  LOOP
	}
	
        pwn := -ptw
        pww :=  ptn
        pwh :=  0.0
	IF pwn*pwn+pww*pww < 1.0 DO
	{ // The camera platform is already close to the model origin.
	  ptn, ptw, pth := 0.0, 1.0, 0.0
	  pwn, pww, pwh := 1.0, 0.0, 0.0
	  pln, plw, plh := 0.0, 0.0, 1.0
	  LOOP
	}
	
	standardiseM3(@ptn) // Standardise axes based on t and w
        LOOP

      CASE 'y':
      CASE 'Y':
        // Add random values in the range -incrementdegrees to +incrementdegrees
	// to the w and l angles of every base pair.
        setrandomangles()
        LOOP

      CASE 'z':
      CASE 'Z':
        // Set w and l angles to zero for all XY pairs.
        FOR XY = AA TO TT DO
	{ LET av = anglesv!XY
	  av!1, av!2 := 0.1, 0.1
	  setrotationmatrix(XY)
	}
	platformn :=   0.0
	platformw :=   0.0
	platformh :=   0.0
        LOOP
    }
  }
}

AND wranglesdata() BE
{ LET filename = VEC 100
  LET len = 0
  LET outstream = 0
  LET ch = 0
  writef("*nWrite angles data to file: ")
  deplete(cos)
  selectinput(stdin) // Select input from (typically) the keyboard.
  
  ch := rdch() REPEATWHILE ch='*s' // Skip over spaces.

  WHILE 'A' <= ch <= 'Z' |
        'a' <= ch <= 'z' |
	'0' <= ch <= '9' |
	ch='/' | ch='.' | ch='**'DO
	{ len := len+1
	  filename%len := ch
	  ch := rdch()
	}

  filename%0 := len

  UNLESS ch='*n' DO
  { writef("*nThe file name can only contain letters, digits and*n")
    writef("'/', '.' or '**'*n")
    RETURN
  }
  outstream := findoutput(filename)
  UNLESS outstream DO
  { writef("*nUnable to write to file: %s*n", filename)
    RETURN
  }

  selectoutput(outstream)
  FOR XY = AA TO TT DO
  { LET av = anglesv!XY
    writef("%s %9.6f %9.6f %9.6f*n",
            XY2str(XY), rad2deg(av!0), rad2deg(av!1), rad2deg(av!2))
  }
  endstream(outstream)
  selectoutput(stdout)
  writef("*nAngle data written to file: %s*n", filename)
  RETURN
}

AND angle(FLT x, FLT y) = x=0 & y=0 -> 0, VALOF
{ // Calculate the angle in degrees between point (x,y) and the
  // x axis using atan2.
  // If (x,y) is above the x-axis the result is betweem 0 and +180
  // If (x,y) is below the x-axis the result is betweem 0 and -180
  LET degrees = FIX rad2deg(sys(Sys_flt, fl_atan2, y, x))
  RESULTIS degrees
}

AND setrandomangles() BE FOR XY = AA TO TT DO
{ // Change the w and l angles of each base pair by adding random
  // values in the range -incrementdegrees to +incrementdegrees.
  // Note that incrementdegrees can be doubled or halved by the
  // commands '|' and '\'. 
  LET av = anglesv!XY
  FOR a = 1 TO 2 DO
  { LET FLT r = FLOAT(randno(2000001) - 1000000)
    LET FLT angle = av!a + incrementdegrees * r / 1000000.0
    // Limit the maximum angle size.
    IF angle >  90.0 DO angle :=  90.0
    IF angle < -90.0 DO angle := -90.0
    av!a := angle
  }
  setrotationmatrix(XY)

}

AND setcurrpair(letno) BE
{ currpair := incrementstate*4 + letno
  incrementstate := -1
  // Use the same axis as before.
  wrcurrangle()
}

AND setcurraxis(axis) BE
{ curraxis := axis
  newline()
  wrcurrangle()
}

AND doincrement(FLT val) BE
{ LET av = anglesv!currpair
  av!curraxis := av!curraxis + val
  writef("doincrement: val=%7.3f currpair=%s axis=%c  new angle=%7.3f*n",
          val, XY2str(currpair), axis2ch(curraxis), av!curraxis)
  modelchanged := TRUE
  wrcurrangle()
  setrotationmatrix(currpair)
  prpair(currpair)
}

AND wrcurrangle() BE
{ LET v = anglesv!currpair
  writef("*c%s%c=%6.3f  incrementdegrees=%6.3f   *n",
           XY2str(currpair), axis2ch(curraxis), v!curraxis, incrementdegrees)
  deplete(cos)
}

AND XY2str(XY) = VALOF SWITCHON XY INTO
{ DEFAULT:  RESULTIS "??"

  CASE  0:  RESULTIS "AA"
  CASE  1:  RESULTIS "AC"
  CASE  2:  RESULTIS "AG"
  CASE  3:  RESULTIS "AT"

  CASE  4:  RESULTIS "CA"
  CASE  5:  RESULTIS "CC"
  CASE  6:  RESULTIS "CG"
  CASE  7:  RESULTIS "CT"

  CASE  8:  RESULTIS "GA"
  CASE  9:  RESULTIS "GC"
  CASE 10:  RESULTIS "GG"
  CASE 11:  RESULTIS "GT"

  CASE 12:  RESULTIS "TA"
  CASE 13:  RESULTIS "TC"
  CASE 14:  RESULTIS "TG"
  CASE 15:  RESULTIS "TT"
}

AND axis2ch(axis) = "twl"%(axis+1)

AND standardiseV3(v) BE
{ // Make v a vector of unit length
  LET FLT r = glRadius3(v!0, v!1, v!2)
  IF debug DO writef("standardiseV3: %9.6f %9.6f %9.6f radius  %9.6f*n",
                      v!0, v!1, v!2, r)
  TEST r > 1.0e-8
  THEN { v!0 := v!0 / r
         v!1 := v!1 / r
         v!2 := v!2 / r
       }
  ELSE { v!0 := 1.0
         v!1 := 0.0
         v!2 := 0.0
	 abort(5578)
       }
}

AND standardiseM3(m) BE
{ // Make the three vectors in matrix m of unit length
  // and mutually orthogonal. This is typically used on rotation
  // matrices to ensure they remain valid even after multiple
  // transformations.
  LET p, q, r = @m!0, @m!3, @m!6

  IF debug DO
  { writef("standardiseM3:*n")
    prmat3(m)
    newline()
    //abort(5567)
  }
  standardiseV3(p)       // Make the first axis, p, unit length.
  IF debug DO
  { writef("standardiseM3: After standaiseV3*n")
    prmat3(m)
    newline()
    //abort(5568)
  }
  crossprodV3(p, q, r)   // Make r orthogonal to p and q
  IF debug DO
  { writef("standardiseM3: After crossprod*n")
    prmat3(m)
    newline()
    //abort(5569)
  }
  standardiseV3(r)       // Make r unit length
  IF debug DO
  { writef("standardiseM3: After second standardiseV3*n")
    prmat3(m)
    newline()
  }
  crossprodV3(r, p, q)   // Make q unit length orthogonal to p and r
  IF debug DO
  { writef("standardiseM3: After final crossprod*n")
    prmat3(m)
    newline()
    //abort(5566)
  }
}

AND crossprodV3(p, q, r) BE
{ LET FLT x, FLT y, FLT z = p!0, p!1, p!2
  LET FLT a, FLT b, FLT c = q!0, q!1, q!2
  r!0 := y*c - b*z
  r!1 := z*a - c*x
  r!2 := x*b - a*y
}

AND mat4set(m,  a0,  a1,  a2,  a3,
                a4,  a5,  a6,  a7,
                a8,  a9, a10, a11,
               a12, a13, a14, a15) BE
{ // Set the elements of matrix m.
  m!0, m!4,  m!8, m!12 := a0, a4,  a8, a12
  m!1, m!5,  m!9, m!13 := a1, a5,  a9, a13
  m!2, m!6, m!10, m!14 := a2, a6, a10, a14
  m!3, m!7, m!11, m!15 := a3, a7, a11, a15
}


AND prmat3(m) BE
{ // m is a 3x3 matrix as a sequence of columns.
  writef("( %9.5f %9.5f %9.5f )*n", m!0, m!3, m!6)
  writef("( %9.5f %9.5f %9.5f )*n", m!1, m!4, m!7)
  writef("( %9.5f %9.5f %9.5f )*n", m!2, m!5, m!8)
}

AND prmat4(m) BE
{ // m is a 4x4 matrix as a sequence of columns.
  writef("( %8.4f %8.4f %8.4f %8.4f )*n", m!0, m!4, m! 8, m!12)
  writef("( %8.4f %8.4f %8.4f %8.4f )*n", m!1, m!5, m! 9, m!13)
  writef("( %8.4f %8.4f %8.4f %8.4f )*n", m!2, m!6, m!10, m!14)
  writef("( %8.4f %8.4f %8.4f %8.4f )*n", m!3, m!7, m!11, m!15)
}

AND prv4(v) BE
{ // v is a vector of four elements.
  writef(" %8.4f %8.4f %8.4f %8.4f*n", v!0, v!1, v!2, v!3)
}



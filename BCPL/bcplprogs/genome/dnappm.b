
/*

A genome is a sequence of bases specified by the letters A, C, G and
T.  This progam converts a given sequence into a 3D coloured path
following a route based on the sequence of bases and a set of angle
specifying how the genome twists and bends at each base pair
junction. The program in interactive allowing the angles and viewing
parameters to be varied. It current works with various versions of the
Covid-19 virus whose sequences have a length of about 30000 bases. The
images produced are surprising.

This version (unlike dnaplot.b) generates a sequence of .ppm image
files of frames ready to be combined to form a MPEG video using the
ffmpeg command.

Implemented by Martin Richards (c) 18 April 2021

It is still under development.

Usage

dnappm  "FROM,ANGLES,SEED/n,
         IN/n,IW/n,IH/n,TN/n,TW/n,TH/n,FT/n,F1/n,F2/b,S/s,PPM=P/s,BUILD=B/s,BN/n,D/s"

FROM            gives the filename of the file of genome letters. If FROM is
                not given the genome sequence is taken from file seq045512.
                The only significant letters used are A, C, G and T not appearing
                in comments between # and the end of the line.
                By convention, the first commant identifies the genome sequence.

ANGLES          This specifies a file that replaces the default setting of the
                rotational angles of each possible base pair. There are
                16 pair AA, AC,..., TG, TT and each has 3 angles. These are
                floating point numbers giving angles in degrees. If this argument
                is not given random small angles are chosen based on the seed
                of the random number generatpr.

SEED/n          Set the seed value for the random number generator a set
                of small random angles. The default value is 9.

IN/n,IW/n,IH/n  The integer coordinates of the initial position of the camera.

TN/n,TW/n,TH/n  The integer coordinates of the target position of the camera.

FT/n            This argument is only used when BUILD is not specified. It
                gives the frame number of the image generated when the camera
                is at the trarget position. The image resulting from the
                camera at the initial position always has frame number zero.

 specifies the number of frames and possibly .ppm
                image files to generate. If BUILD is specified these frames
                represent the genome model as it is being built viewed from
                the initial camera position.
                    If BUILD is not specified the images are views from
                the equally spaced camera positions along the line from
                the initial to the target position .he frame number is
                zero when the camera is at the initial position and FT
                gives the frame number when the camera reaches the target
                position.
                    The default FT value is 10 mainly as a debugging aid
                but if the frames are going to be converted into a video
                FT should the total number of frames needed.

F1/n,F2/n       The frame numbers of the first and last frame image files
                to be displayed.  The default values are 0 and FT.
                If BUILD is not specified these these values can be outside
                the range 0 to FT with obvious effect.
 
S/s             Start the program with stepping not being enabled.

PPM/s           Output displayed frames as .ppm files.
                Depending on whether the frames are a fly through of the
                model or generated while the model is being built, the
                file names are of the form Fddddd.ppm or Bddddd.ppm
                where ddddd is the 5 digit frame number. 

BUILD=B/s       Display and possibly generate .ppm files representing the
                genome model as it is built as viewd from the initial
                camera position.

BN/n            This argument is only used when BUILD is specified. It gives
                the number of bases to be added to the genome model before
                the next frame is displayed. All images are views of the
                model taken with the camera at the initial position. The
                frame numbers start at zero and a subset of them may be
                selected using the F1 an F2 arguments. These image files can
                later be combined to form a video file.

D/s             Turns on some debugging output.


The world axes are:

      N      Direction north
      W      Direction west
      H      Direction up

The current plan is to display an image of the genome as a sequence as
coloured triangles using the BCPL SDL interface. The path along the
genome will rotate by an amount depending on the pair of bases
involved. These rotations can be thought of as adjustments made by
elevator, aileron and rudder controls of an aircraft flying along the
genome. The 16 sets of three angles can be adjusted interactively
causing the resulting genome image to change its shape. The genome
sequence of bases is provided by the FROM file.  Only the characters
A, C, G and T not comments are used. If the FROM file is not given
a default filename is used.

The initial setting of the rotational angles is given by a data file.
The current set of angles can be saved to file using the W command.

Although the program normally just displays a sequence of views of the
genome while writing PPM image files, the user can control the
behaviour interactively. The possible user commands are as follows.

?        Output this help info.
Q        Quit.
AA AC AG AT CA CC etc to TG TT  Select base pair to adjust.
N        Select next rotation axis of the selected base pair.
* /      Multiply or divide angle increment to be used by 2.
+ -      Add or subtract the increment to the currently selected axis of
         the currently selected base pair.
R        Change the w and l angles of all 16 base pairs by random amounts.
Z        Set the t angle to 36 and the w and l angles to zero for all 16 base pairs.
         This causes the genome to be a straight line rotating 360 degrees every
         10 bases. 
S        Start/Stop the stepping of the camera position.
<n> F    Select frame number n and display it.
<n> V    Set the camera's field of view. The angle between the left and right
         edge of the image will be <n> degrees.
W name   Write the angles data to file.
D        Enable or disable debugging output.


History

12/04/2021

Started the modification of dnaplot.b to use SDL rather than OpenGL
library, and to make it generate .ppm files representing a selection
of images of the fly throgh.


Notes about this program

The orientation of the camera is specified by three unit vectors t, w
and l.

Vector t is the camera's direction of view and its components in world
coordinate are (ctn,ctw,cth). These are direction cosines since t is
of unit length.

Vector w points in the direction to the left of the camera. It is
orthogonal to t and parallel to the horizontal plane (h=0). Its
direction cosines are (cwn,cww,cwh).

Vector l is in the upwards direction of the camera.  It is orthogonal
to both t and w. Its direction cosines are (cln,clw,clh).


The axes used while constructing the genome model are

      t      The forward direction along the genome
             (the direction of thrust if the path of the genome is
             thought of as an aircraft flying along the genome)
      w      Left direction (The direction of the left wing of
             the aircraft)
      l      The direction orthogonal to t and w (the direction of lift
             of the aircraft)

The genome model is a sequence of 3D triangles pointing along the path
from each base to the next. The orientation of the next triangle
depends on the prientation of the previous triangle and rotations about
each axis specified for the pair of bases involved.

The resulting 3D sequence of triangle follows a path that might be
taken by an aircraft was flying at constant speed with setting of the
ailerons, elevator and rudder determined by the two base letter
involved as it passes from one base to the next.

The (t,w,l) axes of the first segment of the genome are aligned with
(N,W,H). Both the (N,W,H) and (t,w,l) are right handed axes.

The first triangle lies in the N-W plane and points in the direction
N.  The centre of its base is at the origin, namely (N,W,H)=(0,0,0).

Distances in the model corresponds to a real world distances of 1 nm.
However the distance between consecutive bases is taken to be 1 nm
rather than the more accurate value of 0.34nm. The coordinates of the
first two bases are therefore (0,0,0) and (1,0,0). The coordinates of
the third base depends on the twisting and bending corresponding to
the first two base letters.


Properties of a B-type DNA double helix.

It is a right handed double helix.
The distance between bases is  0.34 nm.
The rate of rotation is about 10 bases per 360 degrees ie 36 degrees per base.
The diameter of the genome   1.9 nm.


The creation of the model.

As the model is being constructed using the function
mkGenomeModel. This function uses (stx,sty,stz), (swx,swy,swz) and
(slx,sly,stlz) to orientation of the next triangle to draw as the
direction cosines of the axes (t,w,l). The mid point of the base of
the next triangle is (x,y,z)

The orientation of the triangle linking two bases depends on the
orientation of the previous triangle and the angles of rotations about
directions t, w and l for the two bases involved. Each triangle has a
colour depending on its base letter. The real world coordinates and
colour number of each triangle vertice is placed in a vector called
vertexv. The individual triangles are identified by triplets in the
vector indexv.

Images of the genome.

This program displays a selection of images of the genome as viewed by
a camera stepping along the line from an initial point towards a
target point. The images have frame numbers with number zero
corresponding to the initial point. The frame number of the target
point can be specified by the user using the FT argument. Its default
value is 1000 which would cause the program display and possibly
generarate .ppm files for 1001 frames. The frames actually displayed
can be specified using the F1 and F2 arguments.

If the argument PPM is given, each displayed frame will be written as
a .ppm file with name Fnnnnn.ppm where nnnnn is the five digit frame
number.

*/

GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the SDL library source code.
.
GET "libhdr"
GET "sdl.h"

GLOBAL {
  stdin:ug
  stdout

  col_black   // To hold a selection of colours
  col_blue
  col_green
  col_yellow
  col_red
  col_majenta
  col_cyan
  col_white
  col_darkred
  col_gray
  col_lightyellow
  col_lightred

  FLT pi
  FLT f0 // To hold 0.0
  FLT f1 // To hold 1.0
  
  fromfilename      // The name of the file holding the genome sequence.
  anglesfilename    // 

  ppming       // If TRUE a .ppm file is written every time a frame is
               // displayed.
  building     // Display frames of the model as it is being built.

  buildN       // The number of the last frame to display when building is TRUE.
               // These frames are equally spaced along the genome sequence.
  buildB1
  buildB2
  buildstep    // Number of added bases per build image.
  
  debug        // If TRUE output debugging information as the program runs.

  spacev; spacep; spacet  // Used by newvec

  blklist  // List of blocks of work space
  blkp
  blkt
 
  done
  stepping
  
  
  FLT ctn; FLT ctw; FLT cth // Direction cosines of the Camera t axis
                            // ie camera direction of view,
			    // These are in consecutive memory locations
			    // so @ctn points to a vector of three elements
			    // which can be used as an argument of inprod.
			    
  FLT cwn; FLT cww; FLT cwh // Direction cosines of the Camera w axis
                            // ie camera direction left which is
			    // always horizontal.
			    
  FLT cln; FLT clw; FLT clh // Direction cosines of the Camera l axis
                            // ie camera direction up orthogonal
			    // to t and w.

  FLT screencentrex // The coordinates of the centre of the screen
  FLT screencentrey

  FLT fieldofview   // The field of view between 5.0 and 120.0 degrees
  
  FLT scalefactor
  FLT depthfactor
  
  intarg  // To hold the integer argument value used the F and V commands.

  initialPosNo; targetPosNo
  firstPosNo; lastPosNo         // Can be set by the F1 and F2 arguments
                                // These specify the first and last frame
				// numbers of the video.
  cameraPosNo  // If building is FALSE this steps through all position
               // numbers from firstPosNo to lastPosNo.
	       // If building is TRUE this is set to firstPosNo and images
	       // are displayed of the genome as it is being built.

  frameno      // If building is TRUE this has the same value as cameraPosNo.
               // If building is FALSE is steps through the values from
	       // buibB1 to buildB2.

  // Locations in world coordinates of the initial and target positions
  // of the camera, and the its current position.
  FLT initialN; FLT initialW; FLT initialH
  FLT targetN;  FLT targetW;  FLT targetH
  FLT cameraN;  FLT cameraW;  FLT cameraH


  anglesv     // Points to 16 angle triples
  matrixv     // Vector of the 3x3 rotation matrices for the 16 base pairs
  
  genomev     // This holds the genome base numbers in
              // genomev!1 to genomev!upb where upb is genomev!0.
              // eg -> [ 4, 1,2,3,4 ] for sequence A C G T

  // Variables are used by mkGenomeModel to represent the genome model.

  vertexv      // This will hold the vertex data in vertexv!1 to vertexv!upn
               // where upb is vertexv!0

  indexv       // This will hold the indices in indexv!1 to indexv!upb
               // where upb is indexv!0

  displayv     // This will hold the vertex data in vertexv!1 to vertexv!upn
               // where upb is vertexv!0

  indexp       // The index number of the next vertex to be created. The vertex with
               // index number 0 will start at position 1 in vvec.

  currcolno
  ppmv         // To hold the ppm matrix of colour numbers
  
  // The global functions
  newvec
  pushval
  concatstr
  letno2ch
  setangles
  mat3set
  mat3mulmmm
  mat3mulmvv
  prvec3
  prmat3
  prpairs
  prpair
  setrotationmatrix
  mkTriangle
  rdGenomeSequence
  mkGenomeModel
  rdangles
  axis2ch
  doincrement
  drawGenomeModel
  deg2rad
  rad2deg

  XY2str
  rotate3
  setcurraxis
  setcurrpair
  setinitstate
  wrcurrangle
  
  //Compileshader
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

  world2screen     // Convert world floating point coordinates to
                   // integer scaled screen coordinated
  zscale
  setIterationFactors
  setCameraOrientation
  setCameraPos

  setCameraLens    // Depends on fieldofview
  displayPos       // Draw a the image from the current camera position.
  wrppmcolour      // Write 3 binary bytes
  fillscreen       // (colno)
  setcurrcolno     // (colno)
  sequenceTitle      // To hole the first 50 characters of the first comment
                   // in the genome sequence file.
  wrppmfile
  sdldrawpoint     // Saved version since a replacement of drawpoint
                   // is defined in this file.
  ppmdrawpoint     // Define in this file to replace drawpoint in the sdl library.
  
}

MANIFEST {
  blkupb = 1000000
  
  // Pair subscripts
  AA=0; AC; AG; AT; CA; CC; CG; CT; GA; GC; GG; GT; TA; TC; TG; TT

  colno_red = 0
  colno_green
  colno_blue
  colno_cyan
  colno_white
  colno_black
  colno_gray

  colno_a=     colno_red    // Colours used in the vertex vector
  colno_c=     colno_green
  colno_g=     colno_blue
  colno_t=     colno_cyan
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
{ // sxv is a self expanding vector
  LET upb = sxv!0      // =0 or the upb of v
  LET v   = sxv!1      // =0 or a getveced vector
  // If v is not zero, v!0 will be the subscript of its latest element in v.
  // If the vector is full, pushval will allocate another
  // larger and copy the existing elements into it before
  // pushing x.

  LET p = v -> v!0, 0 // Position of the previous element, if any.
//writef("pushval: %n*n", val)

  // The upb of v grows as needed.

  // Initially upb, v, and p are all zero

  IF p>=upb DO
  { // We must allocate a new larger vector
    LET newupb = 3*upb/2 + 10
    LET newv = getvec(newupb)
//writef("pushval: sxv=%n allocating new vector at %i6 oldupb %n newupb %n*n",
//        sxv, newv, upb, newupb)
//abort(2222)
    UNLESS newv DO
    { writef("More memory needed for pushval*n")
      abort(999)
      RETURN
    }
    sxv!0 := newupb
    sxv!1 := newv
    
    // Copy the existing elements
    FOR i = 0 TO upb DO newv!i := v!i
    // Pad with zeroes
    FOR i = upb+1 TO newupb DO newv!i := 0
    // Free the old vector if it existed.
    IF v DO freevec(v)

    IF FALSE & debug DO
    {  writef("pushval: replacing v=%i6 upb=%i6 with newv=%i7 upb=%i6 p=%n*n",
               v, upb, newv, newupb, p)
      abort(6666)
    }
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

AND colno2col(colno) = VALOF SWITCHON colno INTO
{ DEFAULT: writef("colno2col: Bad colour number %n", colno)
           abort(999)

  CASE colno_red:   RESULTIS col_red
  CASE colno_green: RESULTIS col_green
  CASE colno_blue:  RESULTIS col_blue
  CASE colno_cyan:  RESULTIS col_cyan
  CASE colno_white: RESULTIS col_white
  CASE colno_black: RESULTIS col_black
  CASE colno_gray:  RESULTIS col_gray
}

AND wrppmcolour(colno) BE  SWITCHON colno INTO
{ DEFAULT: sawritef("colno2col: Bad colour number %n*n", colno)
           abort(998)

  CASE colno_red:   binwrch(#xFF); binwrch(#x00); binwrch(#x00); RETURN // Red
  CASE colno_green: binwrch(#x00); binwrch(#xFF); binwrch(#x00); RETURN // Green
  CASE colno_blue:  binwrch(#x00); binwrch(#x00); binwrch(#xFF); RETURN // Blue
  CASE colno_cyan:  binwrch(#xFF); binwrch(#xFF); binwrch(#x00); RETURN // Cyan
  CASE colno_white: binwrch(#xFF); binwrch(#xFF); binwrch(#xFF); RETURN // White
  CASE colno_black: binwrch(#x00); binwrch(#x00); binwrch(#x00); RETURN // Black
  CASE colno_gray:  binwrch(#x46); binwrch(#x46); binwrch(#x46); RETURN // Gray
}

AND letno2colno(letno) = VALOF SWITCHON letno INTO
{ DEFAULT: RESULTIS 0

  CASE 0:  RESULTIS colno_red   // Colour number for base A
  CASE 1:  RESULTIS colno_green // Colour number for base C 
  CASE 2:  RESULTIS colno_blue  // Colour number for base G
  CASE 3:  RESULTIS colno_cyan  // Colour number for base T
  CASE 4:  RESULTIS colno_white // Colour number for White
  CASE 5:  RESULTIS colno_black // Colour number for Black
  CASE 6:  RESULTIS colno_gray  // Colour number for the background
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
  mat3set(mv,        f1,    f0,    f0,     f0,  cost,  sint,    f0, -sint,  cost)

  // Rotate clockwise about axis W by w radians
  mat3set(m,       cosw,    f0, -sinw,     f0,    f1,    f0,  sinw,    f0,  cosw)
  mat3mulmmm(m,mv,mv)
 
  // Rotate clockwise about axis H by l radians
  mat3set(m,       cosl,  sinl,    f0,  -sinl,  cosl,    f0,    f0,    f0,    f1)
  mat3mulmmm(m,mv,mv)

  //writef("setrotationmatrix: at=%9.5f aw=%9.5f al=%9.5f =>*n", at, aw, al)
  //prmat3(mv)
  modelchanged := TRUE
}


AND rotate3t(m, FLT angle) BE
{ // m is a 3x3 orientation matrix whose columns are the direction
  // cosines of its axes, t, w and l. The orientation is rotated
  // clockwise by angle radians about axis t.
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
  // clockwise by angle radians about axis w.
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
  // clockwise by angle radians about axis l.
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


AND mkTriangle(vsxv, isxv, a, b, c, colno) BE
{ // This is used by mkGenomeModel to create a 3D triangle
  // in the vectors vsxv and isxv.
  // vsxv is the self expanding vector for the vertex data
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

AND rdGenomeSequence(filename) = VALOF
{ // Read the genome into a self expanding vector and, if successful,
  // returns its getvec'd vector of letter numbers 1, 2, 3 and 4. The
  // zeroth element is the subscript of the last base of the genome.
  // The result is zero on failure.

  LET vupb, v = 0, 0 // The self expanding vector for the letter numbers.
  LET sxv = @vupb
  
  LET instream = filename -> findinput(filename), 0
  LET firstComment = TRUE

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
	{ LET len = 0
	  ch := rdch() REPEATWHILE ch='*s' // Skip leading spaces
	  UNTIL ch='*n' | ch=endstreamch DO
	  { IF firstComment & len<50 DO
	    { len := len+1
	      sequenceTitle%len := ch
	    }
	    ch := rdch()
	  }
	  IF firstComment DO
	  { sequenceTitle%0 := len
	    writef("Sequence Title: %s*n*n", sequenceTitle)
	  }
	}
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
  // It allocates and fills the following vectors after deallocateing the
  // previous versions if necessary.
  
  // vertexv  to hold the vertex data, vertexv!0 holds its upb.
  //          each entry is of the form [N, W, H, colno].
  // indexv   to hold the indices, indexv!0 holds its upb.
  //          These are in groups of 3 for each triangle.
  // displayv to hold the display items, displayv!0 holds its upb
  //          Each entry is of the form [mode, size, offset]
  //          The only mode used is 4 for triangle.
  //          size is the number of elements of indexv to use, and
  //          offset is the subscript of the first element of indexv to use.

  LET vupb, vvec = 0, 0 // Self expanding vectors for vertices
  LET iupb, ivec = 0, 0 //                        for index values
  LET dupb, dvec = 0, 0 //                        for display values

  LET vsxv = @vupb // Pointers to self expanding vector for use with push.
  LET isxv = @iupb
  LET dsxv = @dupb

  // vsxv is a self expanding array for the vertices [N W H colno].
  // isxv is a self expanding array for the index vector identifying the triangles.
  // dsxv is a self expanding array for the display elements

  // Coordinates of the position of thecurrent genome base, initially (0,0,0).
  LET FLT  x, FLT  y, FLT  z = f0, f0, f0
  // Coordinates of the position of the next genome base, if any.
  LET FLT nx, FLT ny, FLT nz =  ?,  ?,  ?

  // Declare the matrix representing the orientation of the next
  // segment of the genome, initially pointing in direction N.
  LET FLT stx, FLT sty, FLT stz = f1, f0, f0  // Direction t, first column
  LET FLT swx, FLT swy, FLT swz = f0, f1, f0  // Direction w, second column
  LET FLT slx, FLT sly, FLT slz = f0, f0, f1  // Direction l, third column

  LET m = @stx  // The 3x3 orientation matrix
  
  indexp := 0 // The position of the next entry in the index vector.
  
  // Draw the initial white rectangle to mark the start of the genome.

  // Note that genomeradius is currently 0.25
  { LET FLT lx0, FLT ly0, FLT lz0 = -2.0,  genomeradius, f0 // For a rectangle
    LET FLT rx0, FLT ry0, FLT rz0 = -2.0, -genomeradius, f0 // in plane x-y
    LET FLT lx1, FLT ly1, FLT lz1 =   f0,  genomeradius, f0
    LET FLT rx1, FLT ry1, FLT rz1 =   f0, -genomeradius, f0

    LET FLT ax, FLT ay, FLT az =   0.0,  100.0, 0.0
    LET FLT bx, FLT by, FLT bz =   0.0, -100.0, 0.0
    LET FLT cx, FLT cy, FLT cz = 100.0,    0.0, 0.0
    //mkTriangle(vsxv, isxv, @ax, @bx, @cx, colno_blue)

    ax, ay, az :=  0.0,   0.0,  100.0
    bx, by, bz :=  0.0,   0.0, -100.0
    cx, cy, cz := 100.0,   0.0,   0.0
    //mkTriangle(vsxv, isxv, @ax, @bx, @cx, colno_green)


    // Draw the rectangle as two triangles
    mkTriangle(vsxv, isxv, @lx0, @rx0, @rx1, colno_white) // Right triangle
    mkTriangle(vsxv, isxv, @lx0, @lx1, @rx1, colno_white) // Left triangle
  }
  
  // Now draw coloured triangles for the sequence of genome bases.

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

    mkTriangle(vsxv, isxv, @lx, @rx, @nx, letno2colno(X))

    x, y, z := nx, ny, nz

    IF i = genomev!0 BREAK // Just drawn the last triangle

    // Update the the orientation matrix for the next triangle

    Y  := genomev!(i+1)
    XY := X<<2 | Y        // = 0 to 15 representing the current base pair.

    // Multiply the orientation matrix m by the XY rotation matrix.
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
    mkTriangle(vsxv, isxv, @rx0, @lx1, @lx0, colno_black)  // Left  triangle
    mkTriangle(vsxv, isxv, @rx0, @lx1, @rx1, colno_black)  // Right triangle
  }

//vvec!0 :=  12 * 3 * 4   // Vertices for 2 triangles 
//ivec!0 :=  12 * 3       // 2 triangles

  pushval(@dupb, 4)      // Draw triangles
  pushval(@dupb, ivec!0) // The number of indices to process
  pushval(@dupb, 0)      // Position of the first index

  
  vertexv  := vvec
  indexv   := ivec
  displayv := dvec
  // Note that vertexv!0, indexv!0 and displayv!0 are the upper bounds
  // of their vectors.
writef("vertexv=%n indexv=%n displayv=%n*n", vertexv, indexv, displayv)

  IF FALSE DO
  //IF debug DO
  { // Output the vertex and index data
    // as a debugging aid
    writef("*nVertex data, vertex=%n*n", vertexv)
    FOR i = 1 TO vertexv!0>120 -> 120, vertexv!0 BY 4 DO
    { writef("*n%i3: ", (i-1)/4)
      writef(" %8.3f", vertexv!i)
      writef(" %8.3f", vertexv!(i+1))
      writef(" %8.3f", vertexv!(i+2))
      writef(" %8i",   vertexv!(i+3))
    }
    writef("*n*nIndex data*n")
    FOR i = 1 TO indexv!0>64 -> 64, indexv!0 DO
    { IF i MOD 10 = 1 DO writef("*n%i6: ", i-1)
      writef(" %i5", indexv!i)
    }
    writef("*n*nDisplay data items*n")
    FOR i = 1 TO displayv!0 - 1 BY 3 DO
      writef(" %i5  %i5  %i5*n",
             displayv!i, displayv!(i+1), displayv!(i+2)) // mode n offset
    newline()
    //abort(1278)
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
  //writef("%s matrix", XY2str(XY))
  //FOR i = 0 TO 8 DO writef(" %5.3f", mv!i)
  //newline()
}

LET setinitstate() BE
{ blklist, blkp, blkt := 0, 0, 0

  pi := 3.141592653
  f0 := 0.0
  f1 := 1.0
  col_white := 4.0
  col_black := 5.0

  sdldrawpoint := drawpoint
  drawpoint := ppmdrawpoint

  anglesv := newvec(15) // anglesv!XY-> [an,aw,ah ]  Rotation angles
  matrixv := newvec(15) // matrixv!XY-> [m0,m1,m2,  m3,m4,m5, m6,m7,m8 ]
//writef("anglesv=%n matrixv=%n*n", anglesv, matrixv)
	
  // Setup the initial values of anglesv and matrixv.
  FOR XY = 0 TO 15 DO
  { LET av = newvec(2) // -> [3 rotation angles]
    LET mv = newvec(8) // -> [9 rotation matrix elements]
    anglesv!XY, matrixv!XY := av, mv
    av!0, av!1, av!2 := 36.0, f0, f0
    mat3set(mv, f1,f0,f0, f0,f1,f0, f0,f0,f1)
    //writef("setinitstate: %s av=%n mv=%n*n", XY2str(XY), av, mv)
  }

  //abort(1000)
  // Set the default segment angles

  //setangles(AA, deg2rad(0.0),   f0,   f0)
  //setangles(AC, deg2rad(0.0),   f0,   f0)
  //setangles(AG, deg2rad(0.0),   f0,   f0)
  //setangles(AT, deg2rad(0.0),   f0,   f0)

  //setangles(CA, deg2rad(0.0),   f0,   f0)
  //setangles(CC, deg2rad(0.0),   f0,   f0)
  //setangles(CG, deg2rad(0.0),   f0,   f0)
  //setangles(CT, deg2rad(0.0),   f0,   f0)

  //setangles(GA, deg2rad(0.0),   f0,   f0)
  //setangles(GC, deg2rad(0.0),   f0,   f0)
  //setangles(GG, deg2rad(0.0),   f0,   f0)
  //setangles(GT, deg2rad(0.0),   f0,   f0)

  //setangles(TA, deg2rad(0.0),   f0,   f0)
  //setangles(TC, deg2rad(0.0),   f0,   f0)
  //setangles(TG, deg2rad(0.0),   f0,   f0)
  //setangles(TT, deg2rad(0.0),   f0,   f0)
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
}



LET start() = VALOF
{ LET argv = VEC 100
  AND str  = VEC 20
  AND titlev = VEC 50
  AND s = "No sequence yet"
  FOR i = 0 TO s%0 DO titlev%i := s%i
  sequenceTitle := titlev

  stdin  := input()
  stdout := output()

  // Initialise all global vectors.
  genomev := 0
  leftxv, rightxv := 0, 0
  leftzv, rightzv := 0, 0
  depthv := 0
  vertexv, indexv, displayv := 0, 0, 0
  blklist := 0
  
  UNLESS initsdl() DO
  { writef("ERROR: Unable to initialise the SDL library*n")
    RESULTIS 0
  }

  UNLESS rdargs("FROM,ANGLES,SEED/n,*
                *IN/n,IW/n,IH/n,TN/n,TW/n,TH/n,*
                *FT/n,F1/n,F2/n,*
                *S/s,*
                *PPM=P/s,*
                *BUILD=B/s,BN/n,B1/n,B2/n,*
                *D/s",
                 argv, 100) DO
  { writef("Bad arguments for dnabmp*n")
    RESULTIS 0
  }

  fromfilename   := "seq045512" // other possible files are seq1, seq2 or seq3
  anglesfilename := "angles"
  intarg, frameno := 0, 0

  // Set some default values.
  initialPosNo :=  0
  targetPosNo  := 10  // Can be changed by TN argument.
  
  fieldofview := 60.0 // Angle of view from left to right edge of the screen.
  
  firstPosNo, lastPosNo := -1, -1
  cameraPosNo := 0
  buildN := 10    // Building frames have numbers from 0 to buildN
  buildB1, buildB2 := -1, -1
  
  buildstep := 1  // Number of added bases per build frame.
  
  // Set the default initial and target camera positions.
  initialN, initialW, initialH :=  -1300.0,    0.0,   5.0
  //initialN, initialW, initialH :=      0.0,    200.0,   5000.0
  targetN,  targetW,  targetH  :=      0.0,    0.0,   5.0


  IF argv!0 DO                    // FROM     The genome sequence file
    fromfilename := argv!0
  IF argv!1 DO                    // ANGLES   The file of 48 angle settings
    anglesfilename := argv!1
  setseed(9)
  
  IF argv!2  DO setseed(!(argv!2)) // SEED/n Seed for choosing random angles
  IF argv!3  DO initialN      := FLOAT !(argv!3)   // IN/n 
  IF argv!4  DO initialW      := FLOAT !(argv!4)   // IW/n
  IF argv!5  DO initialH      := FLOAT !(argv!5)   // IH/n
  IF argv!6  DO targetN       := FLOAT !(argv!6)   // TN/n
  IF argv!7  DO targetW       := FLOAT !(argv!7)   // TW/n
  IF argv!8  DO targetH       := FLOAT !(argv!8)   // TH/n
  IF argv!9  DO targetPosNo   := !(argv!9)         // FT/n Camera position of the target 
  IF argv!10 DO firstPosNo    := !(argv!10)        // F1/n First camera position
  IF argv!11 DO lastPosNo     := !(argv!11)        // F2/n Last camera position
  stepping  := argv!12                             // S/s
  ppming    := argv!13                             // PPM/s Write ppm files
  building  := argv!14                             // BUILD=B/s display building
  IF argv!15 DO buildN  := !(argv!15)              // BN/n Number of build images
  IF argv!16 DO buildB1 := !(argv!16)              // B1/n Number of first build image
  IF argv!17 DO buildB2 := !(argv!17)              // B2/n Number of lasr build image
  debug     := argv!18                             // D/s   Output debugging info

  // Ensure that the initial and target positions are at least one unit apart.
  IF distance(@targetN, @initialN) < 1.0 DO initialN := targetN - 1.0

  IF firstPosNo<0 DO firstPosNo := 0
  IF lastPosNo <0 DO lastPosNo := targetPosNo

  setinitstate()

  // Create an SDL window
  UNLESS  mkscreen3d(concatstr("Genome from file: ",
                                fromfilename -> fromfilename,
	                                       "Test Genome",
				                       
		                str),
//                              1200, 800)
//                              800, 680)
                     1500, 700) DO
//                   800, 500) DO
//                     800, 300) DO
  { writef("*nUnable to create an SDL window*n")
    RESULTIS 0
  }
  writef("Screen Size is %n x %n*n", screenxsize, screenysize)

  screencentrex := (FLOAT screenxsize) / 2.0
  screencentrey := (FLOAT screenysize) / 2.0

  depthvupb := screenxsize * screenysize - 1
  depthv := getvec(depthvupb)
  ppmv   := getvec(depthvupb)  // To hold colour numbers
  
  col_black       := maprgb(  0,   0,   0)
  col_blue        := maprgb(  0,   0, 255)
  col_green       := maprgb(  0, 255,   0)
  col_yellow      := maprgb(  0, 255, 255)
  col_red         := maprgb(255,   0,   0)
  col_majenta     := maprgb(255,   0, 255)
  col_cyan        := maprgb(255, 255,   0)
  col_white       := maprgb(255, 255, 255)
  col_darkred     := maprgb(128,   0,   0)
  col_gray        := maprgb( 70,  70,  70)
  col_lightyellow := maprgb(128, 255, 255)
  col_lightred    := maprgb(255, 128, 128)

  //wrhelp()

  IF anglesfilename DO rdangles(anglesfilename)

  FOR i = 1 TO 10 DO setrandomangles() // Choose an interesting set of angles.

  //prpairs()  // Output the rotation angles

  genomev := rdGenomeSequence(fromfilename)

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

  // genomev!1 to genomev!upb now hold the genome base numbers where
  // upb is in genomev!0. The base codes are: A=0, C=1, G=2 and T=3.
  // Note that genomev was allocated using getvec and will be freed
  // at the end.

  //IF FALSE DO
  IF debug DO
  { LET len = genomev!0               // Subscript of the last element in v
    IF len>60 DO len := 60
    
    //writef("Start of genome: %s*n", fromfilename=0 -> "test data", fromfilename)
    FOR i = 1 TO len DO
    { LET letno = genomev!i
      IF letno<0 BREAK
      wrch(letno2ch(letno))
      IF i MOD 72 = 0 DO newline()
    }
    newline()
    IF len < genomev!0 DO writef("...*n")
    //abort(6664)
  }

IF FALSE DO
{ fillscreen(colno_blue) // Test the character drawing function drawf.
  selectcolno(1)
  drawf(charLmargin, screenysize/2, "ABCD*n1234567890")
  updatescreen()
  IF ppming DO wrppmfile()
  //abort(1004)
  delay(5_000)
  GOTO fin
}


  mkGenomeModel()
  modelchanged := FALSE

  // vertexv now holds the vertex data, vertev!0 holds its upb
  // Each vertex occupies 4 words [x, y, z, colno] starting at
  // position 1.
  // indexv holds the indices, indexv!0 holds its upb
  // In this program the indices always come in groups of three
  // starting at position 1. The represent triangles.
  // displayv holds the display triples, displayv!0 holds its upb
  // In this program displayv only hold one item [mode, size, offset]
  // representing the set of triangles to display. mode is always 4
  // offset is always 0.

  TEST building
  THEN displayBuild()
  ELSE displayFlight()
  
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

  delay(0_500)
  closesdl()
  newline()
  RESULTIS 0
}//xxx

AND displayFlight() BE
{ // Display the complete genome model from eaxh camera position
  // from firstPosNo to lastPosNo
  done := FALSE  // Only set to TRUE by the Q command.
  
  fillscreen(colno_gray)
  updatescreen()

writef("done=%n firstPosNo=%n lastPosNo=%n*n", done, firstPosNo, lastPosNo)

  frameno := firstPosNo
  UNTIL done | frameno > lastPosNo DO
  {  processevents()

    // If any genome angles have changed we must re-construct the model.

    WHILE modelchanged DO
    { writef("Building the model with a new set of angles*n")
      // Create the modified genome model.
      mkGenomeModel()
      modelchanged := FALSE
    }

writef("Calling displayPos for frame number %n*n", frameno)
    displayPos()

    IF stepping DO
      frameno := frameno+1 // Step to next camera position.
  }
}

AND displayBuild() BE
{ // Display the genome model as it is being built as viewed from
  // camera position firstPosNo. The model data is already in
  // vertexv, indexv and displayv.
  
  // In this program there is only one display item so displayv!0
  // is always 3.
  LET mode   = displayv!1    // Points, Lines, Linestrip, etc. =4 for triangles.
  LET size   = displayv!2    // Number of index elements. 3 per triangle.
  LET offset = displayv!3    // Offset in the index vector. =0 in this program.
  LET trianglecount = size/3 // The number of triangles in the model.

  IF buildB1<0 DO buildB1 := 0
  IF buildB2<0 DO buildB2 := buildN

  cameraPosNo := firstPosNo    // Either zero or the value set by argument F1/n
  
  // Setup the camera.

  setCameraPos()         // This depend on the initial and target point and
                         // initialPosNo, frameno and targetPosNo.

  setCameraOrientation() // This depends only on the initial and target point.
                         // The camera points along this line and is held level.

  setCameraLens()        // This uses fieldofview and screenxsize to set
                         // scalefactor.

  setdepthlimits(10, 1_000_000_000) // World integer coordinates

  fillscreen(colno_gray)
  selectcolno(colno_white)
  drawf(30, screenysize-30, "%s", sequenceTitle)
  updatescreen()

  writef("cameraPosNo=%n firstPosNo=%n lastPosNo=%n*n",
          cameraPosNo,   firstPosNo,   lastPosNo)
  writef("buildN=%n buildB1=%n buildB2=%n*n",
          buildN,   buildB1,   buildB2)

abort(1000)


//size := 1*3 // Just draw only a few triangles while debugging
writef("drawGenomeModel: display item: mode=%n size=%n offset=%n*n",
        mode, size, offset)

  UNLESS displayv!0=3 & mode=4 & offset=0 DO // Safety check.
  { writef("Model data error: displayv!0=%n mode=%n offset=*n",
                              displayv!0,   mode,   offset)
    abort(999)
  }

  buildstep := 1
  IF building DO buildstep := trianglecount / buildN

//writef("vertexv=%n indexv=%n*n", vertexv, indexv)
    
  done := FALSE  // Only set to TRUE by the Q command.
  
  FOR trino = 0 TO trianglecount DO // The triangle number of the next
                                      // triangle. The first has number zero.
  { // The vertices contain 3 floating point world coordinates
    // followed by one integer colour number.
    LET t  = @indexv!(offset+3*trino+1) // Pointer to the three indices of
                                        // the vertices of this triangle.
    LET v1 = @vertexv!(1+4*t!0) // Each vertex has 4 elements
    LET v2 = @vertexv!(1+4*t!1) // namely [n,w,h,colno]
    LET v3 = @vertexv!(1+4*t!2)
    LET colno = v1!3  // The colour of the triangle is the colour
                      // of the first vertex.
	
    LET sx1, sy1, sz1 = 0, 0, 0 // For the integer scaled screen coordinates
    LET sx2, sy2, sz2 = 0, 0, 0 // computed by calls of world2screen.
    LET sx3, sy3, sz3 = 0, 0, 0

    processevents()
    IF done BREAK
    
    // Calculate the 3D integer screen coordinates of each vertex
    // with the z coordinate scaled to improve the accuracy of
    // hidden surface removal of nearly parallel intersecting planes.
    //writef("v1= "); prvec3(v1)
    //writef("v2= "); prvec3(v2)
    //writef("v3= "); prvec3(v3)

    world2screen(v1, @sx1)
    world2screen(v2, @sx2)
    world2screen(v3, @sx3)

//writef("build trino=%n*n", trino)

    selectcolno(colno)         // Use the colour of the first vertex
    //writef("colno=%n => %x8*n", colno, colno2col(colno))
    //newline()
//writef("Triangle %5.2f %5.2f %5.2f  %5.2f %5.2f %5.2f  %5.2f %5.2f %5.2f*n",
//        v1!0,v1!1,v1!2, v2!0,v2!1,v2!2, v3!0,v3!1,v3!2)
//writef("Triangle %n %n %n  %n %n %n  %n %n %n*n",
//        sx1,sy1,sz1, sx2,sy2,sz2, sx3,sy3,sz3)
//writef("neardepth=%n fardepth=%n*n", neardepth, fardepth)
//abort(1001)
    drawtriangle3d(sx1, sy1, sz1,
                   sx2, sy2, sz2,
                   sx3, sy3, sz3)
//abort(1000)
    IF trino=trianglecount |
       building & (trino+1) MOD buildstep = 0 DO
    { // It is time to display the frame of the model, possibly while it is
      // being built.
      updatescreen()
      
      IF ppming DO
      { writef("Writing a ppm file*n")
        wrppmfile()
      }
      //abort(5596)
    }
  }
}

AND setCameraPos() BE
{ // This is called every time cameraPos changes.
  // It calculates the camera position by linear interpolation
  // along the line from the initial to target position.
  // The initial and target positions numbers are initialPosNo and targetPosNo,
  // and the camera position number is cameraPosNo.
  LET FLT ipno = FLOAT initialPosNo
  LET FLT tpno = FLOAT targetPosNo
  LET FLT pno  = FLOAT cameraPosNo
  LET FLT divisor = tpno - ipno
  // divisor is always >= 1.0
  LET FLT initialfac = (tpno - pno) / divisor
  AND FLT targetfac  = (pno - ipno) / divisor
  cameraN := initialN*initialfac + targetN*targetfac
  cameraW := initialW*initialfac + targetW*targetfac
  cameraH := initialH*initialfac + targetH*targetfac
  writef("Camerapos: CameraPosNo=%i3 %12.3f %12.3f %12.3f*n",
          cameraPosNo, cameraN, cameraW, cameraH)
  delay(5000)
}

AND setCameraLens() BE
{ // This is called every time fieldofview or the screen size changes.
  // It sets scalefactor to the number of pixels corresponding to a
  // unit distance in direction x or y at a depth of one. This is
  // held as a floating point number.
  // fieldofview is between 5.0 amd 120.0
  
  LET FLT fovby2 = deg2rad(fieldofview / 2.0) // Half the angle in radians
  scalefactor   := screencentrex / sys(Sys_flt, fl_tan, fovby2)

  writef("setCameraLens: screencentrex=%9.3f  screencentrey=%9.3f*n",
                         screencentrex,       screencentrey)
  writef("setCameralens: scalefactor=  %9.3f*n", scalefactor)
}

AND setCameraOrientation() BE
{ // This depend only on the initial and target positions.

  //writef("Initial position:  (%12.5f, %12.5f, %12.5f)*n",
  //        initialN, initialW, initialH)
  //writef("Target  position:  (%12.5f, %12.5f, %12.5f)*n",
  //        targetN,  targetW,  targetH)
  //newline()

  ctn := targetN - initialN // Form a vector in direction
  ctw := targetW - initialW // from initial to target.
  cth := targetH - initialH // ctn is guaranteed to be >= 1.0
  
  // (ctn,ctw,cth) is a vector pointing in the camera's forward direction.
  // After calling standardise it is of unit length and becomes the
  // direction cosines of the camera's t axis.
	  
  standardise(@ctn)
  writef("Direction t:  (%12.5f, %12.5f, %12.5f)*n", ctn, ctw, cth)

  // The camera's left axis (w) is orthogonal to t and is
  // parallel to the N-W plane (h=0).
  // The following code computes its direction cosines.
  
  cwn := -ctw // A non null vector orthogonal to t
  cww :=  ctn
  cwh :=  0.0
  
  // Note that the inner product of (ctn,ctw,cth) and (cwn,cww,cwh) is
  // ctn*cwn  + ctw*cww + cth*cwh = ctn*(-ctw) + ctw*ctn + cth*0.0 = 0.0
  // so w is orthogonal to t.
  
  standardise(@cwn)
  writef("Direction w:  (%12.5f, %12.5f, %12.5f)*n", cwn, cww, cwh)

  // The camera's third axis (l) is orthogonal to both t and w and so
  // can be computed as the crossproduct of t and w.
  crossprodV3(@ctn, @cwn, @cln)
  writef("Direction l:  (%12.5f, %12.5f, %12.5f)*n", cln, clw, clh)
  newline()
}

AND displayPos() BE
{ // IF building is TRUE this displays a sequence of frames as
  // the model is being built.
  // If building is FALSE it just displays the complete model
  // from the current camera position.
  writef("displayPos: entered*n")
  //abort(1001)
  // Setup the camera.

  setCameraPos()         // This depend on the initial and target point and
                         // initialPosNo, frameno and targetPosNo.

  setCameraOrientation() // This depends only on the initial and target point.
                         // The camera points along this line and is held level.

  setCameraLens()        // This uses fieldofview and screenxsize to set
                         // scalefactor.

  setdepthlimits(10, 1_000_000_000) // World integer coordinates

  fillscreen(colno_gray)
  //writef("ppmv now starts as follows:*n")
  //FOR i = 0 TO 20 DO writef("%i3: %n*n", i, ppmv!i)
//writef("displayPos: About to call drawGenomeModel*n")
//abort(1000)
  // Draw the complete model or
  // a sequence of frames as the model is being built.
  drawGenomeModel()
}

AND selectcolno(colno) BE
{ currcolour := colno2col(colno)
  currcolno := colno
}

AND fillscreen(colno) BE
{ // Fill the window with the given colour and
  // initialise the depth matrix.
  fillsurf(colno2col(colno))
  writef("fillscreen: colno=%n*n", colno)
  // Initialise the ppm matrix.
  FOR i = 0 TO depthvupb DO ppmv!i := colno
}

AND drawtext() BE
{ writef("drawtext calling drawf*n")
  selectcolno(colno_white)
  drawf(30, screenysize-30, "%s", sequenceTitle)
  
  drawf(screenxsize/2-50, 30, "Frame number: %i4", frameno)
}

AND deg2rad(FLT degrees) = degrees*pi/180.0 // Return the angle in radians

AND rad2deg(FLT radians) = radians*180.0/pi // Return the angle in degrees

AND drawGenomeModel() BE
{ // If building is TRUE this draws frames of the genome model as viewed from the
  // first camera position as the model is being constructed. It is displayed
  // every time the number of bases in the model is exactly divisible by buildstep.
  // The default setting of buildstep is one if ppming is FALSE, otherwise it is
  // the length of the genome divided by buildN which can be set by the BN
  // argument.
  // If building is FALSE the complete genome model is displayed as viewed by
  // the camera from successive positions on the line from the initial to
  // target positions. The range of positions generating images is firstPosNo
  // to lastPosNo which can be set by The F! and F2 arguments.

  // The model is represented by a set of coloured triangles based on data in
  // vertexv and indexv. Elements 1, 2 and 3 of diplayv hold the triplet
  // (mode,size,offset) where mode is 4 indicating that triangles will be
  // drawn using size indeces starting at subscript offset (=0) of indexv.
  
  // In this program there is only one display item so displayv!0
  // is always 3.
  LET mode   = displayv!1    // Points, Lines, Linestrip, etc. =4 for triangles.
  LET size   = displayv!2    // Number of index elements. 3 per triangle.
  LET offset = displayv!3    // Offset in the index vector. =0 in this program.
  LET trianglecount = size/3 // The number of triangles in the model.

//size := 1*3 // Just draw only a few triangles while debugging
writef("drawGenomeModel: display item: mode=%n size=%n offset=%n*n",
        mode, size, offset)

  UNLESS displayv!0=3 & mode=4 & offset=0 DO // Safety check.
  { writef("Model data error: displayv!0=%n mode=%n offset=*n",
                              displayv!0,   mode,   offset)
    abort(999)
  }

  buildstep := 1
  IF building DO buildstep := trianglecount / buildN

//writef("vertexv=%n indexv=%n*n", vertexv, indexv)
    
  FOR trino = 0 TO trianglecount DO // The triangle number of the next
                                      // triangle. The first has number zero.
  { // The vertices contain 3 floating point world coordinates
    // followed by one integer colour number.
    LET t  = @indexv!(offset+3*trino+1) // Pointer to the three indices of
                                        // the vertices of this triangle.
    LET v1 = @vertexv!(1+4*t!0) // Each vertex has 4 elements
    LET v2 = @vertexv!(1+4*t!1) // namely [n,w,h,colno]
    LET v3 = @vertexv!(1+4*t!2)
    LET colno = v1!3  // The colour of the triangle is the colour
                      // of the first vertex.
	
    LET sx1, sy1, sz1 = 0, 0, 0 // For the integer scaled screen coordinates
    LET sx2, sy2, sz2 = 0, 0, 0 // computed by calls of world2screen.
    LET sx3, sy3, sz3 = 0, 0, 0

    //IF frameno=0 |
    //   building & frameno MOD buildstep = 0 DO
    //{ // Initialise the frame.
    //  fillscreen(colno_gray) // Initialise the frame including the depth matrix
    //  drawtext()
    //}
      
    // Calculate the 3D integer screen coordinates of each vertex
    // with the z coordinate scaled to improve the accuracy of
    // hidden surface removal of nearly parallel intersecting planes.
    //writef("v1= "); prvec3(v1)
    //writef("v2= "); prvec3(v2)
    //writef("v3= "); prvec3(v3)

    world2screen(v1, @sx1)
    world2screen(v2, @sx2)
    world2screen(v3, @sx3)

//writef("trino=%n*n", trino)

    selectcolno(colno)         // Use the colour of the first vertex
    //writef("colno=%n => %x8*n", colno, colno2col(colno))
    //newline()
//writef("Triangle %5.2f %5.2f %5.2f  %5.2f %5.2f %5.2f  %5.2f %5.2f %5.2f*n",
//        v1!0,v1!1,v1!2, v2!0,v2!1,v2!2, v3!0,v3!1,v3!2)
//writef("Triangle %n %n %n  %n %n %n  %n %n %n*n",
//        sx1,sy1,sz1, sx2,sy2,sz2, sx3,sy3,sz3)
//writef("neardepth=%n fardepth=%n*n", neardepth, fardepth)
//abort(1001)
    drawtriangle3d(sx1, sy1, sz1,
                   sx2, sy2, sz2,
                   sx3, sy3, sz3)
//abort(1002)
    //drawtriangle3d(sx1, sy1,  30,
    //               sx2, sy2,  20,
    //               sx3, sy3,  10)
    IF building DO updatescreen()
//abort(1000)
    IF trino=trianglecount |
       building & (trino+1) MOD buildstep = 0 DO
    { // It is time to display the frame of the model, possibly while it is
      // being built.
      updatescreen()
      
      IF ppming DO
      { writef("Writing a ppm file*n")
        wrppmfile()
      }
      frameno := frameno+1
      //abort(5596)
    }
  }
}


AND zscale(FLT z) = VALOF
{ // Scale the given depth coordinate z to improve the accuracy of
  // hidden surface removal.
  // Return the scaled depth component as an integer, clamped to
  // the range -1_000_000_000 to +1_000_000_000
  z := z * 100.0 // Other values could be tried.
  IF z<-1_000_000_000.0 DO z := -1_000_000_000.0
  IF z>+1_000_000_000.0 DO z := +1_000_000_000.0
  RESULTIS FIX z
}

AND world2screen(v, s) = VALOF
{ // The elements of v (N,W,H) are in world floating point coordinates.
  // These are converted to scaled integer screen ccoordinates based on
  // the position and orientation of the camera and scaled
  // to the size of the screen taking account of fieldofview.
  // Returns TRUE if the point is not too far away or depth too small.
  
  LET FLT N, FLT W, FLT H = v!0, v!1, v!2
  LET FLT t, FLT w, FLT l = N-cameraN, W-cameraW, H-cameraH
  // (t,w,l) are the world coordinates of v relative to the camera.
  LET FLT depth = t*ctn + w*ctw + l*cth
  LET FLT x     = t*cwn + w*cww + l*cwh
  LET FLT y     = t*cln + w*clw + l*clh
  // (x,y,depth) are the world coordinates of v using the camera's axes.
  
  // Treat +/- 1_000_000 as +/- infinity and cull any triangle having
  // such a coordinate.

  UNLESS  -1_000_000.0 <   x   < 1_000_000.0 &
          -1_000_000.0 <   y   < 1_000_000.0 &
          -1_000_000.0 < depth < 1_000_000.0 DO
  {
//writef("world2screen: v   = (%12.3f, %12.3f, %12.3f)*n", N, W, H)
//writef("world2screen: xyd = (%12.3f, %12.3f, %12.3f)*n", x, y, depth)
//updatescreen()
//abort(2977)
    s!0 := screenxsize/2
    s!1 := screenysize/2
    s!2 := 0
    RESULTIS FALSE
  }

  IF -1.0 < depth < 1.0 DO depth := 1.0 // To avoid division by zero.

  s!0 := FIX(screencentrex - x*scalefactor / depth) // Screen x coordinate in pixels
  s!1 := FIX(screencentrey + y*scalefactor / depth) // Screen y coordinate in pixels
  s!2 := zscale(depth)                              // Screen z coordinate in pixels

//writef("world2screen: v   = (%12.3f, %12.3f, %12.3f)*n", N, W, H)
//writef("world2screen: pos = (%12.3f, %12.3f, %12.3f)*n", cameraN, cameraW, cameraH)
//writef("world2screen: twl = (%12.3f, %12.3f, %12.3f)*n", t, w, l)
//writef("world2screen: xyd = (%12.3f, %12.3f, %12.3f)*n", x, y, depth)
//writef("world2screen: s   = (%12i, %12i, %12i)*n", s!0, s!1, s!2)
//abort(1456)
  RESULTIS TRUE
}

AND wrhelp() BE
{ writef("*nCommand summary*n*n")
  writef("?        Output this help info.*n")
  writef("Q        Quit.*n")
  writef("AA AC AG AT CA CC etc to TG TT  Select the base pair to adjust.*n")
  writef("N        Select next rotation axis of the selected base pair.*n")
  writef("** /      Multiply or divide the increment by 2.*n")
  writef("+ -      Add or subtract the increment to the currently selected angle.*n")
  writef("R        Randomly adjust the w and l angles of all 16 base pairs.*n")
  writef("Z        Set the t, w and l angles to 36, 0, 0 for all 16 base pairs.*n")
  writef("S        Start or Stop the stepping of the camera position.*n")
  writef("P        Output the camera orientation and other values.*n")
  writef("<n> F    Select frame number n and display it.*n")
  writef("<n> V    Angle of view from left to right edge of the screen.*n")
  writef("W name   Write the angles data to file.*n")
  newline()	
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ DEFAULT:
    //writef("processevents: Unknown event type = %n*n", eventtype)
    LOOP

  CASE sdle_quit:             // 12
    writef("QUIT*n")
    closesdl()
    ///sys(Sys_gl, gl_Quit)
    LOOP

  CASE sdle_keydown:
  { LET ch = eventa2
    //writef("*n%i3 ", eventa2)
    //IF 32 <= eventa2 < 127 DO writef("'%c'*n", eventa2)
    SWITCHON ch INTO
    { DEFAULT:  LOOP

      CASE '*n':
      CASE '*c':
                //writef("*nENTER pressed*n")
		incrementstate := -1
                LOOP

      CASE '?': wrhelp()
                intarg := 0
	        LOOP


      CASE '0': CASE '1': CASE '2': CASE '3': CASE '4':
      CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
                intarg := 10*intarg + ch - '0'
		LOOP
      
      CASE 'q':
      CASE 'Q': done := TRUE
                intarg := 0
                LOOP
      CASE 'a':
      CASE 'A': wrch(ch); deplete(cos)
                TEST incrementstate<0
                THEN incrementstate := 0 // First letter of a pair
		ELSE setcurrpair(0)
                intarg := 0
		LOOP

      CASE 'c':
      CASE 'C': wrch(ch); deplete(cos)
                TEST incrementstate<0
                THEN incrementstate := 1 // First letter of a pair
		ELSE setcurrpair(1)
                intarg := 0
		LOOP

      CASE 'g':
      CASE 'G': wrch(ch); deplete(cos)
                TEST incrementstate<0
                THEN incrementstate := 2 // First letter of a pair
		ELSE setcurrpair(2)
                intarg := 0
		LOOP

      CASE 't':
      CASE 'T': wrch(ch); deplete(cos)
                TEST incrementstate<0
                THEN incrementstate := 3 // First letter of a pair
		ELSE setcurrpair(3)
                intarg := 0
		LOOP

      CASE 'n':
      CASE 'N': setcurraxis((curraxis+1) MOD 3) // Select axit 0, 1 or 2
                intarg := 0
                LOOP

      CASE 'd':
      CASE 'D':
                TEST debug THEN debug := FALSE
	                   ELSE debug := TRUE
                intarg := 0
	        LOOP
		
      CASE 'p':
      CASE 'P': // Print direction cosines and other data
        newline()
        writef("ctn=%9.6f ctw=%9.6f cth=%9.6f*n",
                ctn,      ctw,      cth)
        writef("cwn=%9.6f cww=%9.6f cwh=%9.6f*n",
                cwn,      cww,      cwh)
        writef("cln=%9.6f clw=%9.6f clh=%9.6f*n",
                cln,      clw,      clh)
	newline()
	//abort(2919)
                intarg := 0
        LOOP

      CASE 'w':
      CASE 'W': wranglesdata()
                intarg := 0
                LOOP
	
      CASE 's':
      CASE 'S': stepping := ~stepping
                intarg := 0
                LOOP

      CASE '+': doincrement(incrementradians)
                intarg := 0
                LOOP
      CASE '-': doincrement(-incrementradians)
                intarg := 0
                LOOP

      CASE '/': incrementdegrees := 0.5*incrementdegrees
                incrementradians := deg2rad(incrementdegrees)
                writef("*nincrementvdegrees=%9.6f*n", incrementdegrees)
                intarg := 0
                LOOP
	
      CASE '**':incrementdegrees := 2.0*incrementdegrees
                incrementradians := deg2rad(incrementdegrees)
                writef("*nincrementvdegrees=%9.6f*n", incrementdegrees)
                intarg := 0
                LOOP
			    

      CASE 'f':
      CASE 'F': // Set the frame number to intarg and display this frame.
                frameno := intarg
                writef("Frame number %n*n", frameno)
		intarg := 0
		LOOP
		
      CASE 'v':
      CASE 'V': // Set the frame number to intarg and display this frame.
                fieldofview := FLOAT intarg
                writef("Field of view set to %5.1f degress*n", fieldofview)
		intarg := 0
		LOOP
		
      CASE 'r':
      CASE 'R':
        // Add random values in the range -incrementdegrees to +incrementdegrees
	// to the w and l angles of every base pair.
        setrandomangles()
        LOOP

      CASE 'z':
      CASE 'Z':
        // Set w and l angles to zero for all XY pairs.
        FOR XY = AA TO TT DO
	{ LET av = anglesv!XY
	  av!0, av!1, av!2 := 36.0, 0.0, 0.0
	  setrotationmatrix(XY)
	}
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

AND standardise(v) BE
{ // Scale v to be of unit length, but if the length of v is tiny
  // set v to (1.0, 0.0, 0.0)
  LET FLT r = radius(v)
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
  standardise(p)       // Make the first axis, p, unit length.
  IF debug DO
  { writef("standardiseM3: After standaise*n")
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
  standardise(r)       // Make r unit length
  IF debug DO
  { writef("standardiseM3: After second standardise*n")
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

AND prvec3(v) BE
{ // v is a 3D vector.
  writef("( %12.5f %12.5f %12.5f )*n", v!0, v!1, v!2)
}

AND prmat3(m) BE
{ // m is a 3x3 matrix as a sequence of columns.
  writef("( %12.5f %12.5f %12.5f )*n", m!0, m!3, m!6)
  writef("( %12.5f %12.5f %12.5f )*n", m!1, m!4, m!7)
  writef("( %12.5f %12.5f %12.5f )*n", m!2, m!5, m!8)
}

/*
PPM format used is very simple and is as follows:

P6         The magic number
<xsize>    A decimal number in ASCII giving the image width
<ysize>    A decimal number in ASCII giving the image height
255        The red, green and blue colour each ha a range o to 255
Followed by <xsize> * < <ysize> pixels each represented by three
bytes in binary for the intensity of the red, green and blue components
in that order.
*/

AND wrppmfile() BE
{ LET f = building -> "B00000.ppm", "F00000.ppm"
  LET n = frameno
  LET ppmstream = 0
  LET filename = VEC 20
  FOR i = 0 TO f%0 DO filename%i := f%i
  IF n<0 DO n := 0
  FOR i = 6 TO 2 BY -1 DO
  { filename%i := n MOD 10 + '0'
    n := n/10
  }
  //FOR i = 0 TO 20 DO writef("%i3: %n*n", i, ppmv!i)
  //abort(1000)
  sawritef("Writing image to file: %s*n", filename)
  ppmstream := findoutput(filename)
  selectoutput(ppmstream)
  writef("P6*n")
  writef("%n*n%n*n255*n", screenxsize, screenysize)
  FOR i = 0 TO depthvupb DO wrppmcolour(ppmv!i)
  IF FALSE FOR row = 0 TO screenysize-1 DO
  { writef("*nRow %n ppmv=%n", row, ppmv)
    FOR x = 0 TO screenxsize-1 DO
    { IF x MOD 100 = 0 DO newline()
      writef("%n", ppmv!(x + row*screenxsize))
    }
    newline()
  }
  endstream(ppmstream)
  selectoutput(stdout)
  writef("File %s written*n", filename)
  //abort(1999)
}

AND ppmdrawpoint(x, y) BE
{ LET p = (screenysize-y)*screenxsize + x
  ppmv!p := currcolno
  //sawritef("ppmdrawpoint: (x,y) = (%n,%n) colnp=%n p=%n*n", x, y, currcolno, p)
  //abort(1000)
  //IF x = screenxsize DO abort(2999)
  sdldrawpoint(x, y)
  //updatescreen()
}


/*
This is a program designed to access memory in such a way as to
cause the commands

raster
rast2wav

to create a .wav file that plays an approximation of JS Bach's
Invention No 13 BWV 784.

Implemented by Martin Richards (c) 11 Feb 2020

History

11/02/2020
Just started the implementation.
*/

GET "libhdr"

MANIFEST {
  // Note numbers
  C2=0; Cis2; D2; Dis2; E2; F2; Fis2; G2; Gis2; A2; Ais2 B2 
  C3;   Cis3; D3; Dis3; E3; F3; Fis3; G3; Gis3; A3; Ais3 B3 
  C4;   Cis4; D4; Dis4; E4; F4; Fis4; G4; Gis4; A4; Ais4 B4 
  C5;   Cis5; D5; Dis5; E5; F5; Fis5; G5; Gis5; A5; Ais5 B5 
  C6;   Cis6; D6; Dis6; E6; F6; Fis6; G6; Gis6; A6; Ais6 B6 
  C7
  N   // Next chord marker
  Z   // End of piece marker
}

GLOBAL {
  maxaddress:ug
  pause
  playnote
  piecs
  note2str
}

LET start() = VALOF
{ // Allocate a large vector
  LET upb = 50_000
  LET v = getvec(upb)
  LET notev = piece()
  LET p = notev
  maxaddress := v + upb
  writef("bwv784 entered*n")
  //abort(1000)
  pause(8)    // Pause for 16 semi quavers

  { // Play the next chord
    LET note = !p
    p := p+1
    IF note=Z BREAK
    IF C2 <= note <= N DO playnote(note)
    IF note=N DO pause(1)
  } REPEAT

  pause(8)
  RESULTIS 0
}

AND pause(n) BE FOR i = 1 TO n DO
{ FOR j = 1 TO 50_000 LOOP
}

AND playnote(note) BE
{ IF C2 <= note <= C7 DO
  { LET d = maxaddress / (C7 + 1)
    // d is the boundary between notes C2 and Cis2
    LET a = maxaddress * note / (C7 + 1) + d/2
    // a is the byte address in the middle of the address range
    // for the note.
    writef("playnote: %i2 %i7 range %i7 to %i7 %s*n",
            note, a, a-d, a+d, note2str(note))
    //abort(1004)
    //FOR t = 1 TO 1000 DO
      d := a!0 // Access address a
  }
} 

AND note2str(note) = VALOF SWITCHON note INTO
{ DEFAULT:   RESULTIS "?"

  CASE C2:   RESULTIS "C2"
  CASE Cis2: RESULTIS "Cis2"
  CASE D2:   RESULTIS "D2"
  CASE Dis2: RESULTIS "Dis2"
  CASE E2:   RESULTIS "E2"
  CASE F2:   RESULTIS "F2"
  CASE Fis2: RESULTIS "Fis2"
  CASE G2:   RESULTIS "G2"
  CASE Gis2: RESULTIS "Gis2"
  CASE A2:   RESULTIS "A2"
  CASE Ais2: RESULTIS "Ais2"
  CASE B2:   RESULTIS "B2"

  CASE C3:   RESULTIS "C3"
  CASE Cis3: RESULTIS "Cis3"
  CASE D3:   RESULTIS "D3"
  CASE Dis3: RESULTIS "Dis3"
  CASE E3:   RESULTIS "E3"
  CASE F3:   RESULTIS "F3"
  CASE Fis3: RESULTIS "Fis3"
  CASE G3:   RESULTIS "G3"
  CASE Gis3: RESULTIS "Gis3"
  CASE A3:   RESULTIS "A3"
  CASE Ais3: RESULTIS "Ais3"
  CASE B3:   RESULTIS "B3"

  CASE C4:   RESULTIS "C4"
  CASE Cis4: RESULTIS "Cis4"
  CASE D4:   RESULTIS "D4"
  CASE Dis4: RESULTIS "Dis4"
  CASE E4:   RESULTIS "E4"
  CASE F4:   RESULTIS "F4"
  CASE Fis4: RESULTIS "Fis4"
  CASE G4:   RESULTIS "G4"
  CASE Gis4: RESULTIS "Gis4"
  CASE A4:   RESULTIS "A4"
  CASE Ais4: RESULTIS "Ais4"
  CASE B4:   RESULTIS "B4"

  CASE C5:   RESULTIS "C5"
  CASE Cis5: RESULTIS "Cis5"
  CASE D5:   RESULTIS "D5"
  CASE Dis5: RESULTIS "Dis5"
  CASE E5:   RESULTIS "E5"
  CASE F5:   RESULTIS "F5"
  CASE Fis5: RESULTIS "Fis5"
  CASE G5:   RESULTIS "G5"
  CASE Gis5: RESULTIS "Gis5"
  CASE A5:   RESULTIS "A5"
  CASE Ais5: RESULTIS "Ais5"
  CASE B5:   RESULTIS "B5"

  CASE C6:   RESULTIS "C6"
  CASE Cis6: RESULTIS "Cis6"
  CASE D6:   RESULTIS "D6"
  CASE Dis6: RESULTIS "Dis6"
  CASE E6:   RESULTIS "E6"
  CASE F6:   RESULTIS "F6"
  CASE Fis6: RESULTIS "Fis6"
  CASE G6:   RESULTIS "G6"
  CASE Gis6: RESULTIS "Gis6"
  CASE A6:   RESULTIS "A6"
  CASE Ais6: RESULTIS "Ais6"
  CASE B6:   RESULTIS "B6"

  CASE C7:   RESULTIS "C7"

  CASE N:    RESULTIS "N*n"

}

AND piece () = TABLE
// Every bar has 16 semiquaver chords
// Left   Right
// hand   hand

//C5, N, N, N,
//N,  N, N, N,


   A2,           N,  // Bar  1
          E4,    N,
   A3,    A4,    N,
          C5,    N,

          B4,    N,
          E4,    N,
   Gis3,  B4,    N,
          D5,    N,
				
   A3,    C5,    N,
   E3,           N,
   A3,    E5,    N,
   C4,           N,
				
   B3,    Gis4,  N,
   E3,           N,
   B3,    E5,    N,
   D4,           N,

   C4,    A4,    N, // Bar  2
          E4,    N,
   A3,    A4,    N,
          C5,    N,

   Gis3,  B4,    N,
          E4,    N,
   E3,    B4,    N,
          D5,    N,
				
   A3,    C5,    N,
   E3,           N,
   A3,    A4,    N,
   C4,           N,
				
   B3,           N,
   E3,           N,
   A3,           N,
   B3,           N,

   C4,           N, // Bar  3
          E5,    N,
   A3,    C5,    N,
          E5,    N,
				
   C4,    A4,    N,
          C5,    N,
   A3,    E4,    N,
          G4,    N,
				
   D5,    F4,    N,
   A3,           N,
   F3,    A4,    N,
   A3,           N,
				
   D3,    D5,    N,
   F3,           N,
   A2,    F5,    N,
   C3,           N,

   B2,           N, // Bar  4
          D5,    N,
   D3,    B4,    N,
          D5,    N,

   G3,    G4,    N,
          B4,    N,
   B3,    D4,    N,
          F4,    N,

          E4,    N,
   G3,           N,
   E3,    G4,    N,
   G3,           N,

   C3,    C5,    N,
   E3,           N,
   G2,    E5,    N,
   B2,           N,

Z  // End of piece

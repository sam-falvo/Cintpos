GET "mcpl.h"

STATIC    scorev, fnv

MANIFEST  Pbits = #x7FFF, SH = #X10000, Upb = Pbits,

// Peg bits
Pa = 1<<0,  Pb = 1<<1,  Pc = 1<<2,  Pd = 1<<3,  Pe = 1<<4,
Pf = 1<<5,  Pg = 1<<6,  Ph = 1<<7,  Pi = 1<<8,  Pj = 1<<9,
Pk = 1<<10, Pl = 1<<11, Pm = 1<<12, Pn = 1<<13, Po = 1<<14,

// Hole bits
Ha = Pa*SH, Hb = Pb*SH, Hc = Pc*SH, Hd = Pd*SH, He = Pe*SH,
Hf = Pf*SH, Hg = Pg*SH, Hh = Ph*SH, Hi = Pi*SH, Hj = Pj*SH,
Hk = Pk*SH, Hl = Pl*SH, Hm = Pm*SH, Hn = Pn*SH, Ho = Po*SH

FUN start : => 
  initvecs()

  scorev!Pa := 1       // Set the score for the final position

  LET ways = try(       Ha+
                      Pb+Pc+
                     Pd+Pe+Pf+
                   Pg+Ph+Pi+Pj+
                  Pk+Pl+Pm+Pn+Po  )
   
  writef("Number of solutions = %d\n", ways)

  LET k1=0, k2=0
  LET v1=VEC 15, v2=VEC 15

  FOR i = 0 TO 15 DO v1!i, v2!i := 0, 0

  FOR i = 0 TO Upb DO { IF scorev!i>=0 DO { k1++; (v1!(bits i))++ }
                        IF scorev!i> 0 DO { k2++; (v2!(bits i))++ }
                      }
  FOR i = 0 TO 15 DO writef("v1!%2d = %3d   v2!%2d = %3d\n",
                                i,    v1!i,    i,    v2!i)

  writef("%4d positions reachable from the initial position\n", k1)
  writef("%4d positions on paths to a solution\n",              k2)
  
  freevecs()
  RETURN 0

FUN bits 
: 0 => 0
: w => 1 + bits(w & (w-1))

FUN initvecs : => scorev, fnv := getvec Upb, getvec Upb
                  FOR i = 0 TO Upb DO scorev!i := -1

                  fnv!Pa := f1; fnv!Pb := f2; fnv!Pc := f3
                  fnv!Pd := f4; fnv!Pe := f5; fnv!Pf := f6
                  fnv!Pg := f7; fnv!Ph := f8; fnv!Pi := f9
                  fnv!Pj := fa; fnv!Pk := fb; fnv!Pl := fc
                  fnv!Pm := fd; fnv!Pn := fe; fnv!Po := ff

FUN freevecs : => freevec scorev 
                  freevec fnv 

FUN try : brd =>
  LET poss  = brd & Pbits
  LET score = scorev!poss
  IF score<0 DO // have we seen this board position before
  { score := 0  // No -- so calculate score for this position.
    WHILE poss DO { LET p = poss & -poss
                    poss  -:= p
                    score +:= (fnv!p) brd
                  }
    scorev!(brd&Pbits) := score  // Remember the score
  }
  RETURN score

FUN trymove
: brd, hhp, hpbits => brd&hhp -> 0,       // Can't make move
                      try(brd XOR hpbits) // Try new position

FUN f1 : brd => trymove(brd, Ha+Hb+Pd, Pa+Ha+Pb+Hb+Pd+Hd) +
                trymove(brd, Ha+Hc+Pf, Pa+Ha+Pc+Hc+Pf+Hf)
FUN f2 : brd => trymove(brd, Hb+Hd+Pg, Pb+Hb+Pd+Hd+Pg+Hg) +
                trymove(brd, Hb+He+Pi, Pb+Hb+Pe+He+Pi+Hi)
FUN f3 : brd => trymove(brd, Hc+He+Ph, Pc+Hc+Pe+He+Ph+Hh) +
                trymove(brd, Hc+Hf+Pj, Pc+Hc+Pf+Hf+Pj+Hj)
FUN f4 : brd => trymove(brd, Hd+Hb+Pa, Pd+Hd+Pb+Hb+Pa+Ha) +
                trymove(brd, Hd+He+Pf, Pd+Hd+Pe+He+Pf+Hf) +
                trymove(brd, Hd+Hg+Pk, Pd+Hd+Pg+Hg+Pk+Hk) +
                trymove(brd, Hd+Hh+Pm, Pd+Hd+Ph+Hh+Pm+Hm)
FUN f5 : brd => trymove(brd, He+Hh+Pl, Pe+He+Ph+Hh+Pl+Hl) +
                trymove(brd, He+Hi+Pn, Pe+He+Pi+Hi+Pn+Hn)
FUN f6 : brd => trymove(brd, Hf+Hc+Pa, Pf+Hf+Pc+Hc+Pa+Ha) +
                trymove(brd, Hf+He+Pd, Pf+Hf+Pe+He+Pd+Hd) +
                trymove(brd, Hf+Hi+Pm, Pf+Hf+Pi+Hi+Pm+Hm) +
                trymove(brd, Hf+Hj+Po, Pf+Hf+Pj+Hj+Po+Ho)
FUN f7 : brd => trymove(brd, Hg+Hd+Pb, Pg+Hg+Pd+Hd+Pb+Hb) +
                trymove(brd, Hg+Hh+Pi, Pg+Hg+Ph+Hh+Pi+Hi)
FUN f8 : brd => trymove(brd, Hh+He+Pc, Ph+Hh+Pe+He+Pc+Hc) +
                trymove(brd, Hh+Hi+Pj, Ph+Hh+Pi+Hi+Pj+Hj)
FUN f9 : brd => trymove(brd, Hi+He+Pb, Pi+Hi+Pe+He+Pb+Hb) +
                trymove(brd, Hi+Hh+Pg, Pi+Hi+Ph+Hh+Pg+Hg)
FUN fa : brd => trymove(brd, Hj+Hf+Pc, Pj+Hj+Pf+Hf+Pc+Hc) +
                trymove(brd, Hj+Hi+Ph, Pj+Hj+Pi+Hi+Ph+Hh)
FUN fb : brd => trymove(brd, Hk+Hg+Pd, Pk+Hk+Pg+Hg+Pd+Hd) +
                trymove(brd, Hk+Hl+Pm, Pk+Hk+Pl+Hl+Pm+Hm)
FUN fc : brd => trymove(brd, Hl+Hh+Pe, Pl+Hl+Ph+Hh+Pe+He) +
                trymove(brd, Hl+Hm+Pn, Pl+Hl+Pm+Hm+Pn+Hn)
FUN fd : brd => trymove(brd, Hm+Hh+Pd, Pm+Hm+Ph+Hh+Pd+Hd) +
                trymove(brd, Hm+Hi+Pf, Pm+Hm+Pi+Hi+Pf+Hf) +
                trymove(brd, Hm+Hl+Pk, Pm+Hm+Pl+Hl+Pk+Hk) +
                trymove(brd, Hm+Hn+Po, Pm+Hm+Pn+Hn+Po+Ho)
FUN fe : brd => trymove(brd, Hn+Hi+Pe, Pn+Hn+Pi+Hi+Pe+He) +
                trymove(brd, Hn+Hm+Pl, Pn+Hn+Pm+Hm+Pl+Hl)
FUN ff : brd => trymove(brd, Ho+Hj+Pf, Po+Ho+Pj+Hj+Pf+Hf) +
                trymove(brd, Ho+Hn+Pm, Po+Ho+Pn+Hn+Pm+Hm)



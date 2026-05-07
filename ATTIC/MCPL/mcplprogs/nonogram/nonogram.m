// This is another demonstration program to solve a backtracking
// problem using recursion and bit patterns. 
// See queens.m, solit.m  and penta3.m

// Copyright: Martin Richards,  March 1997

GET "mcpl.h"

STATIC
  cupb, rupb, spacev, spacet, spacep, boardv, knownv,
  cdatav, rdatav, cfreedomv, rfreedomv,
  rowbits, known, orsets, andsets,
  change, count, tracing


FUN start : =>
  LET argv = VEC 50
  LET datafile = "../mintcode/nonograms/demo"

  IF rdargs("DATA,TO/K,TRACE/S", argv, 50)=0 DO
  {  writef "Bad arguments for NONOGRAM\n"
     RETURN 20
  }

  UNLESS argv!0=0 DO datafile := argv!0
  UNLESS argv!1=0 DO
  { LET out = findoutput(argv!1)
    IF out=0 DO
    { writef("Cannot open file %s\n", argv!1)
      RETURN 20
    }
    selectoutput(out)
  }

  tracing := argv!2

  UNLESS initdata() DO 
  { writes "Cannot allocate workspace\n"
    UNLESS argv!1=0 DO endwrite()
    retspace()
    RETURN 20
  }

  UNLESS readdata datafile DO
  { writes "Cannot read the data\n"
    UNLESS argv!1=0 DO endwrite()
    retspace()
    RETURN 20
  }

  count := 0
  allsolutions()

  writef("%d solution%s found\n", count, count=1 -> "", "s")

  UNLESS argv!1=0 DO endwrite()
  retspace()
  RETURN 0

FUN initdata : =>            // returns TRUE if successful
  spacev   := getvec 100000 
  spacet   := @ spacev!100000
  spacep   := spacev
  cupb     := 0
  rupb     := 0
  boardv   := getvec 31
  knownv   := getvec 31
  cdatav   := getvec 31
  rdatav   := getvec 31
  cfreedomv:= getvec 31
  rfreedomv:= getvec 31

  IF spacev=0 OR boardv=0 OR knownv=0 OR cdatav=0 OR rdatav=0 OR
     cfreedomv=0 OR rfreedomv=0 RETURN FALSE

  FOR i = 0 TO 31 DO
  { boardv!i    := 0
    knownv!i    := 0
    cdatav!i    := 0
    rdatav!i    := 0
    cfreedomv!i := 0
    rfreedomv!i := 0
  }

  RETURN TRUE

FUN retspace : =>
  IF spacev    DO freevec spacev
  IF boardv    DO freevec boardv
  IF knownv    DO freevec knownv
  IF cdatav    DO freevec cdatav
  IF rdatav    DO freevec rdatav
  IF cfreedomv DO freevec cfreedomv
  IF rfreedomv DO freevec rfreedomv

FUN readdata : filename =>      // Returns TRUE if successful
  LET stdin = input()
  LET data  = findinput filename 

  IF data=0 DO
  { writef("Unable to open file %s\n", filename)
    RETURN FALSE
  }

  selectinput data

  LET argv  = VEC 200
  cupb, rupb := -1, -1

  { LET ch = rdch()
    WHILE ch='\s' OR ch='\n' DO ch := rdch()
    IF ch=Endstreamch BREAK
    unrdch()

    IF rdargs("ROW/S,COL/S,,,,,,,,,,,,,,,,,,,", argv, 200)=0 DO
    { writes("Bad data file\n")
      endread()
      selectinput stdin
      RETURN FALSE
    }

    IF argv!0 = argv!1 DO
    { writes "Expecting ROW or COL in data file\n"
      endread()
      selectinput stdin
      RETURN FALSE
    }

    IF argv!0 DO rdatav!++rupb := spacep
    IF argv!1 DO cdatav!++cupb := spacep

    FOR i = 2 TO 20 DO
    { IF argv!i = 0 BREAK
      !spacep+++ := str2numb(argv!i)
    }
    !spacep+++ := 0

  } REPEAT

  FOR x = 0 TO cupb DO cfreedomv!x := freedom(cdatav!x, rupb)
  FOR y = 0 TO rupb DO rfreedomv!y := freedom(rdatav!y, cupb)

  IF tracing DO
  { FOR x = 0 TO cupb DO writef("cfreedom!%2d = %2d\n", x, cfreedomv!x)
    FOR y = 0 TO rupb DO writef("rfreedom!%2d = %2d\n", y, rfreedomv!y)
  }

  endread()

  selectinput stdin

  UNLESS marks(cdatav, cupb)=marks(rdatav, rupb) DO
  { writes("Data sumcheck failure\n")
    writef("X marks = %d\n", marks(cdatav,cupb))
    writef("Y marks = %d\n", marks(rdatav,rupb))
    RETURN FALSE
  }

  RETURN TRUE

FUN marks : v, upb =>
  LET res = 0
  FOR i = 0 TO upb DO { LET p = v!i
                        UNTIL !p=0 DO res +:= !p+++
                      }
  RETURN res


FUN freedom : p, upb => IF !p=0 RETURN 0
                        LET free = upb+2
                        free -:= !p + 1 REPEATWHILE !+++p
                        RETURN free

FUN allsolutions : =>
{ UNLESS solve() RETURN // no solutions can be found from here

  LET row=0, bit=0

  FOR i = 0 TO rupb DO
  { LET unknown = ~ knownv!i
    IF unknown DO { row, bit := i, unknown & -unknown
                    BREAK
                  }
  }

  // test to see if a solution has been found
  IF bit=0 DO
  { writef("\nSolution %d\n\n", ++count)
    prboard()
    RETURN
  }

  // There may be a solution from here.
  // Try both possible settings of the unresolved square 
  // given by pos and bit.

  IF tracing DO
  { writes "\nNo more direct resolution available in the following:\n"
    prboard()
  }

  { LET bv = VEC 31
    LET kv = VEC 31

    // save current state
    FOR i = 0 TO 31 DO bv!i, kv!i := boardv!i, knownv!i

    knownv!row, boardv!row |:= bit, bit

    IF tracing DO
    { writes "So, try setting an unresolved square to mark\n"
      prboard()
    }

    allsolutions()

    // restore saved state
    FOR i = 0 TO 31 DO boardv!i, knownv!i := bv!i, kv!i
  }

  // Space for bv and kv is released at this point so that the
  // tail recursive call of allsolutions is more economical.

  knownv!row |:=  bit

  IF tracing DO
  { writes "Try setting a unresolved square to blank\n"
    prboard()
  }
} REPEAT

// solve returns FALSE is no solution possible from here
FUN solve : =>
  { change := FALSE
    UNLESS dorows() RETURN FALSE
    flip()
    UNLESS dorows() DO { flip(); RETURN FALSE }
    flip()
  } REPEATWHILE change

  RETURN TRUE

// dorows returns FALSE if no solution possible from current state
FUN dorows : =>
  FOR row = 0 TO rupb DO
  { orsets, andsets  := 0, #xFFFFFFFF
    rowbits, known := boardv!row, knownv!row
    try(rdatav!row, 0, 0, rfreedomv!row)
    UNLESS (andsets & orsets) = andsets RETURN FALSE
    rowbits, known |:= andsets, ~orsets | andsets
    IF known=knownv!row LOOP
    boardv!row, knownv!row, change := rowbits, known, TRUE
    IF tracing DO { newline(); prboard() }
  }
  RETURN TRUE

FUN try 
: [0], set, ?, ?  =>  // end of piece list

    IF ok(set, cupb+1) DO  // Have we found a valid setting  
    { IF tracing DO        // Yes, we have.
      { FOR col = 0 TO cupb DO
          writef(" %c",  set>>col & 1 -> '*', '.')
        writes "  possible line\n"
      }
      orsets  |:= set      // Accumulate the "or" and 
      andsets &:= set      // "and" sets.
    }

: [size, next], set, col, free =>
    LET piece = 1<<size - 1
    FOR i = 0 TO free DO
    { LET nset = set | piece<<(col+i)
      LET ncol = col+i+size+1
      IF ok(nset, ncol) DO try(@ next, nset, ncol, free-i)
    }

// ok returns TRUE if the given mark placement is
// compatible with the current known board settings
FUN ok : set, npos =>
  LET mask = known & (1<<npos - 1)
  RETURN (set XOR rowbits) & mask = 0

// flip will flip the nonogram about a diagonal axis from
// the top left of the picture. 
// Remember that the top left most position is represented
// by the least significant bits of boardv!0 and knownv!0

FUN flip : =>
  cdatav,    rdatav    := rdatav,    cdatav
  cfreedomv, rfreedomv := rfreedomv, cfreedomv
  cupb,      rupb      := rupb,      cupb
  
  flipbits boardv 
  flipbits knownv 

// flipbits swaps bit (i,j) with bit (j,i) for
// all bits in a 32x32 bitmap. It does it in 5 stages
// by swapping square areas of sizes 16, 8, 4, 2 and
// finally 1.

FUN flipbits : v => xchbits(v, 16, #x0000FFFF)
                    xchbits(v,  8, #x00FF00FF)
                    xchbits(v,  4, #x0F0F0F0F)
                    xchbits(v,  2, #x33333333)
                    xchbits(v,  1, #x55555555)

FUN xchbits
: v, n, mask => LET i = 0
                { FOR j = 0 TO n-1 DO
                  { LET q= @ v!(i+j)
                    LET a=q!0, b=q!n
                    q!0 := a & mask | b<<n &~mask
                    q!n := b &~mask | a>>n & mask
                  }
                  i +:= n+n
                } REPEATWHILE i<32

FUN  prboard : =>
  FOR y = 0 TO rupb DO
  { LET row=boardv!y, known=knownv!y
    FOR x = 0 TO cupb DO
      TEST (known>>x & 1)=0
      THEN writes(" ?")
      ELSE TEST (row>>x & 1)=0
           THEN writes(" .")
           ELSE writes(" M")
    newline()
  }
  newline()



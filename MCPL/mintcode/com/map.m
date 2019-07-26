// (C) Copyright 2000 Martin Richards

// 13/5/2000
// Corrected a minor bug in "map pic"

// Command to map the store allocation under the Mintcode system.
// It counts the store used for its own code, stack
// and workspace as free.
//
// It is based on the map command from the Tripos Operating System
//
// MAP [BLOCKS] [CODE] [NAMES] [MAPSTORE] [TO file]
//     [PIC]
//
//     BLOCKS gives size and address of every block
//     CODE   gives layout of loaded code moduless
//     NAMES  gives space used for routine names,
//            module names, and global initialisation info.
//     TO     specifies an output file
//     PIC    gives the allocation map in picture form

MODULE map

GET "mcpl.h"
GET "mcli.h"  // Just for cli_module

MANIFEST 
  Modword        = #xFDDF,   // MODULE and Entry marker words.
  Entryword      = #xDFDF,

  Modnamesize    = 8/Bpw,
  Routnamesize   = 8/Bpw,
  Nameoffset     = -Routnamesize,

  Vecupb         = 499

FUN start :  =>
  LET blocklist      = rootnode ! Rtn_blklist
  LET a              = blocklist
  LET topofstore     = rootnode ! Rtn_memsize
  LET free=0, used=0, n=0
  LET blocks         = getvec Vecupb
  LET argv           = VEC 50
  LET largest_free   = 0
  LET joinfree       = 0
  LET blocksopt      = FALSE
  LET namesopt       = FALSE
  LET codeopt        = FALSE
  LET mapstore       = FALSE
  LET picopt         = FALSE
  LET mapstack       = @ currco!-1
  LET mapcode        = @ cli_module!-1  // Module vector
  LET modnames=0, routnames=0, globinit=0
  LET constr         = output()
  LET outstr         = 0
  LET rdargs_string  = "BLOCKS/S,NAMES/S,CODE/S,MAPSTORE/S,TO/K,PIC/S"

  IF rdargs(rdargs_string, argv, 50) = 0 DO
  { writef("MAP: Bad args for key string \"%s\"\n", rdargs_string)
    freevec(blocks)
    RETURN 20
  }

  IF blocks = 0 DO
  { writes "MAP: Not enough store for workspace\n"
    RETURN 20
  }

  blocksopt := argv!0
  namesopt  := argv!1
  codeopt   := argv!2
  IF argv!3 DO mapstore, namesopt, codeopt := TRUE, TRUE, TRUE
  IF argv!4 DO
  { outstr := findoutput(argv!4)
    IF outstr = 0 DO
    { writef("Can't open %s\n",argv!4)
      freevec(blocks)
      RETURN 20
    }
    selectoutput outstr 
  }

  picopt := argv!5

  UNTIL !a = 0 DO
  { LET size = !a // in words

    IF a = mapstack OR
       a = mapcode  OR
       a = @ blocks!-1 DO size++ // Make it look free

    blocks!n++ := size
    TEST size&1
    THEN { size--
           free +:= size
           joinfree +:= size
         }
    ELSE { // Used block
           used +:= size
           IF joinfree > largest_free DO largest_free := joinfree
           joinfree := 0
         }

    LET next =  a + size*Bpw

    UNLESS size>=0 AND next<=topofstore*Bpw DO
    { writef("******Store chain corrupt!!\n\
             \Noticed at %d\n", a)
      BREAK
    }
    a := next
    IF n > Vecupb DO
    { writef("\n****** Too many blocks for MAP's workspace\n\
             \****** Only the first %d mapped\n", Vecupb+1)
      BREAK
    }
  }

  IF joinfree > largest_free DO largest_free := joinfree

  // Now print what we've found
  newline()

  IF blocksopt DO
  { writes "Map of free and allocated store\n\n"

    a := blocklist

    FOR i = 0 TO n-1 DO
    { LET size = blocks!i
      LET free = size&1

      IF intflag() DO
      { freevec blocks
        UNLESS outstr = 0 DO endwrite()
        selectoutput constr
        RETURN 0
      }

      writef("%8d: ", a)
      TEST free THEN { writes("free ") ; size-- }
                ELSE   writes("alloc")

      writef("%8d words ", size)
      IF i = 0            DO writes("Boot registers")
      IF i = 1            DO writes("Rootnode")
      IF i = 2            DO writes("Boot stack")
      IF i = 3            DO writes("Boot global vector")
      IF a = mapstack     DO writes("MAP's stack")
      IF a = mapcode      DO writes("MAP's code")
      IF a = @blocks!-1   DO writes("MAP's workspace")
      IF a!3 = Modword  AND good_name(@a!4) DO
               UNLESS a=mapcode DO writef("Module %s", @ a!4)
      newline()
      a +:= size*Bpw
    }

    writef("Top of block list = %d\n", a)
    topofstore := a/Bpw
  }

  writef("Largest contiguous free area: ")
  writeu(largest_free, 0); writes(" words\n")
  writes("Totals: "); writeu(used+free, 0)
  writes(" words available, "); writeu(used, 0)
  writes(" used, "); writeu(free, 0)
  writes(" free\n\n")

  IF picopt DO
  { // Print as a picture
    LET alloc = TRUE
    LET next_block = blocklist
    LET num = 0
    LET col = 0
    LET grainsize = topofstore/(20*64) + 1
    LET addr = 0

    WHILE addr < topofstore DO
    { LET some_alloc = alloc
      LET some_free  = NOT alloc

      UNTIL addr <= next_block OR num > n DO
      { // Move to next block
        alloc := ((blocks!num) & 1) = 0
        TEST alloc THEN some_alloc := TRUE
                   ELSE { some_free := TRUE
                          next_block--
                        }
        next_block +:= blocks!num
        num++
      }

      IF col MOD 64 = 0 DO writef("\n%8d  ", addr*Bpw)
      wrch (num > n    -> '?',  // No info
            some_alloc -> (some_free -> 'a', '@'),
                          '.')
      col++
      addr +:= grainsize
    }
    newline()
  }

  // Print the layout of the code modules, assuming
  // that they will not change under our feet!
  // Also, add up space used for module names, routine
  // names, and global initialisation information.

  a := blocklist

  IF codeopt OR namesopt DO
  { IF codeopt DO writes("Layout of loaded code\n\n")

    FOR i = 0 TO n-1 DO
    { LET s = blocks!i
      IF intflag() BREAK
      IF s&1 DO { a +:= (s-1)*Bpw; LOOP } // Skip free block

      IF a!3=Modword AND good_name(@a!4) DO
      { // We've found a MCPL Module
        LET goffset

        IF codeopt DO
          writef("\n%8d: Module %s - length %4d words\n",
                    @a!2,        @a!4,       a!2)


        modnames +:= Modnamesize + 1

        // Count space for global initialisation table
        goffset := @a!2 + a!2 * Bpw // Word after highest
                                    // referenced global
        { globinit +:= 2
          goffset  -:= 2*Bpw
        } REPEATWHILE !goffset    // End of list

        IF namesopt FOR j=5 /* skip some junk! */ TO a!2 DO
        { LET addr = @a!j // Avoid comparing signed addresses

          IF addr!(Nameoffset-1) = Entryword AND
             good_name(@addr!Nameoffset) DO
          { // MCPL entry sequence
            routnames +:= Routnamesize + 1
            IF mapstore DO
              writef("%8d: %s\n", addr, @addr!Nameoffset)
          }
        }
      }
      a +:= s*Bpw // Point to next block
    }

    IF namesopt DO writef("\nRoutine names: %5d words\n\
                            \Module  names: %5d words\n\
                            \Global initialisation info: %d words\n",
                             routnames, modnames, globinit)
  }

  freevec blocks
  UNLESS outstr = 0 DO endwrite()
  selectoutput constr
  RETURN 0
.

FUN good_name : string =>
   UNLESS string%7 = 0 RETURN FALSE

   FOR i = 0 TO 6 DO
   { LET ch = string%i

     UNLESS 'A'<=ch<='Z' OR 'a'<=ch<='z' OR '0'<=ch<='9' OR
            ch='_' OR ch=' ' RETURN FALSE
   }

   RETURN TRUE
.
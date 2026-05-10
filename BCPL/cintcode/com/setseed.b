SECTION "SETSEED"

GET "libhdr"

LET start() = VALOF 
{ LET argv = VEC 10
  AND seed = randseed
   
  UNLESS rdargs("SEED/N", argv, 10) DO
  { writef("Bad argument for setseed*n")
    RESULTIS 20
  }

  IF argv!0 DO { seed := !argv!0
                 setseed(seed)
               }
  writef("Randno seed: %n*n", seed)
  RESULTIS 0
}

SECTION "match"

GET "libhdr" 

//********************* Space Allocation ******************

GLOBAL {
  spacev:ug; spacep
}

LET mk_init : upb    BE
{ spacev := getvec(upb)
  UNLESS spacev DO
  { writef("More space needed*n")
    abort(999)
  }
  spacep := @ spacev!upb
}

AND mk_close :       BE freevec(spacev)

AND mk1 : x          => VALOF { spacep := spacep-1
                                !spacep := x
				RESULTIS spacep
                              }

AND mk2 : x, y       => VALOF { mk1(y); RESULTIS mk1(x) }

AND mk3 : x, y, z    => VALOF { mk1(z); mk1(y); RESULTIS mk1(x) }

AND mk4 : x, y, z, t => VALOF { mk1(t); mk1(z); mk1(y); RESULTIS mk1(x) }


// The algorithm

LET str2list : str => VALOF
{ LET list=0
  FOR i = str%0 TO 1 BY -1 list := mk2(str%i, list)
  RESULTIS list
}

AND match : pat, str => m(str2list(pat), str2list(str))

AND m
:             0,          0 => TRUE
:             0,          ? => FALSE
:    ['**', ps],          0 => m(ps, 0)
:             ?,          0 => FALSE
: pat['**', ps], str[?, cs] => m(ps, str) | m(pat, cs)
:       [ch,ps],   [=ch,cs] => m(ps, cs)
:             ?,          ? => FALSE

AND try : pat, str BE
{ writef("pat=%s str=%s  ", pat, str)
  TEST match(pat, str)
  THEN writes("matches*n")
  ELSE writes("no match*n")
}

AND start : => VALOF
{ mk_init(1000)
  try("A**B", "AXXB")
  try("A**B", "ABBB")
  try("A**B**", "AXXBYY")
  try("**X**", "abcdefghiXabcdefghi")
  try("**g**g**g**", "abcdefghiXabcdefghi") // doesn't match
  mk_close()
  RESULTIS 0
}


MODULE match

GET "mcpl.h" 

FUN str2list
: str => LET list=0, i=0
         UNTIL str%i=0 DO i++
         UNTIL i=0 DO list := mk2(str%--i, list)
         list
.
FUN match : pat, str => m(str2list pat, str2list str)
.
FUN m
:            0,          0 => TRUE
:            0,          ? => FALSE
:    ['*', ps],          0 => m(ps, 0)
:            ?,          0 => FALSE
: pat['*', ps], str[?, cs] => m(ps, str)
                              OR
                              m(pat, cs)
:      [ch,ps],   [=ch,cs] => m(ps, cs)
:            ?,          ? => FALSE
.
FUN try : pat, str => writef("pat=%s str=%s  ", pat, str)
                      TEST match(pat, str)
                      THEN writes "matches\n"
                      ELSE writes "no match\n"
.
FUN start : =>
  mk_init 1000
  try("A*B", "AXXB")
  try("A*B", "ABBB")
  try("A*B*", "AXXBYY")
  try("*X*", "abcdefghiXabcdefghi")
  try("*g*g*g*", "abcdefghiXabcdefghi") // doesn't match
  mk_close()
  RETURN 0
.
//********************* Space Allocation ******************

STATIC  spacev, spacep

MANIFEST E_space=100

FUN mk_init : upb    => spacev := getvec upb
                        UNLESS spacev RAISE E_space
                        spacep := @ spacev!upb
.
FUN mk_close :       => freevec spacev
.
FUN mk1 : x          => !---spacep := x; spacep
.
FUN mk2 : x, y       => mk1 y; mk1 x
.
FUN mk3 : x, y, z    => mk1 z; mk1 y; mk1 x
.
FUN mk4 : x, y, z, t => mk1 t; mk1 z; mk1 y; mk1 x
.

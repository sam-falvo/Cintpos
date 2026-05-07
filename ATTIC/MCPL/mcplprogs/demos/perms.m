GET "mcpl.h"

FUN start : => perms(0, #b1111)
               RETURN 0
.
STATIC s = CVEC 32

FUN perms
: i,    0 => s!i := 0; writef("%s\n", s)
: i, bits => LET poss = bits
             WHILE poss DO
             { LET bit = poss & -poss
               poss -:= bit
               s%i := "012935A8476"%(bit>>1 MOD 11)
               perms(i+1, bits-bit)
             }
.
/* Other possible divisors and strings are:
11 012935A8476
13 01253A6C49B87
19 012E3HF749IDG68C5BA
29 01263N7D4BOQ8JES5MCAPIRL9HKGF
37 012R3OSX4HPVTCYE58IaQNWGUBD7ZMFA6L9KJ
53 012I3mJF4Zn7KPGD5BacoW8eLhQqHlEY6OCAbVdgpkXN9UfjMTiSR
59 012p37qJ4h8QrkKv5fid9BRGsDlZLTwo6IgPjuecAFCYSnHOtbEXmNaWMVU
61 01273N8o4DOG9fpT5mERPuHwAjgJqaUy6MnCFeSlQtviIZxLBdkshYKcrXbWV
67 012e3GfO4DHygKPt5?EBI|zThVLqQjum6X?dFNCxJs?A{SUpilWcMwr9Rokbv8na7ZY
*/

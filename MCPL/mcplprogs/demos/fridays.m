GET "mcpl.h"

MANIFEST Mon = 0,  Sun = 6,
         Jan = 0,  Feb = 1,  Dec = 11

STATIC
count       = [0,0,0,0,0,0,0],
days        = 0,
daysinmonth = [31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
dayname     = ["Monday", "Tuesday", "Wednesday","Thursday",
               "Friday", "Saturday", "Sunday"]

FUN start : => 
  FOR year = 1973 TO 1973 + 399 DO
  { daysinmonth!Feb := febdays year
    FOR month = Jan TO Dec DO { LET day13 = (days+12) MOD 7
                                (count!day13) ++
                                days +:= daysinmonth!month
                              }
  }

  FOR day = Mon TO Sun DO writef(" %3d %ss\n", count!day, dayname!day)
  RETURN 0
.
FUN febdays : year => year MOD 400 = 0 -> 29,
                      year MOD 100 = 0 -> 28,
                      year MOD 4   = 0 -> 29,
                      28
.

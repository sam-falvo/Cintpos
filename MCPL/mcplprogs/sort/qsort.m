MODULE qsort

GET "mcpl.h"

MANIFEST Upb = 999

FUN start : =>
  writes "\nQuick Sort\n\n"
  RETURN 0

FUN partition
: ?,      p,     =p => p
: m, [<=m]p,      q => partition(m, +++p,    q)
: m,      p, [>=m]q => partition(m,    p, ---q)
: m,   [x]p,   [y]q => x, y := y, x
                       partition(m, +++p, ---q)




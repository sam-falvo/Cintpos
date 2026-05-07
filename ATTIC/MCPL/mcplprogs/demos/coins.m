MODULE coins

GET "mcpl.h"

FUN ways
: 0,         ?  => 1
: ?,       [0]  => 0
: s, coins[>s]  => ways(s, @ coins!1)
: s, coins[v ]  => ways(s, @ coins!1) + ways(s-v, coins)
.

FUN t
: sum => writef("Sum = %3d,   Ways = %4d\n",
                 sum,         ways(sum, [50, 20, 10, 5, 2, 1, 0])
               )
.
FUN start : => t 0; t 1; t 5; t 11; t 20; t 50
               RETURN 0
.

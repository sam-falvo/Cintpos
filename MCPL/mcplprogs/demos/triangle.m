GET "mcpl.h"

FUN start 
: => writef "Enter three lengths: "
     LET a=readn(), b=readn(), c=readn()
     writef("\nTriangle entered: %d %d %d", a, b, c)
     writef("\nThis is %s triangle\n",
               sort_of_triangle(a,b,c))
.

FUN sort_of_triangle
: a, b(<a), c     => sort_of_triangle(b, a, c)
: a, b,     c(<b) => sort_of_triangle(a, c, b)

// At this point we know that a <= b <= c
: a,  b,    >a+b => "not a"
: a,  b,    =a   => "an equalateral"
: a,  =a,   ?    => "an isosceles"
: ?,  =c,   c    => "an isosceles"
: a,  b,    c    =>  c*c=a*a+b*b -> "a right angled",
                                    "a scalene"
.

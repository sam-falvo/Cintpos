GET "libhdr"

LET start 
: => VALOF { LET a, b, c = 0, 0, 0
             writef("Enter three lengths: ")
	     deplete(cos)
             a, b, c := readn(), readn(), readn()
             writef("*nTriangle entered: %n %n %n*n", a, b, c)
	     //abort(1000)
             writef("This is %s triangle*n",
                    sort_of_triangle(a,b,c))
           }

AND sort_of_triangle
: a, b <a, c    => sort_of_triangle(b, a, c)
: a, b,    c <b => sort_of_triangle(a, c, b)

// At this point we know that a <= b <= c
: a,  b,  >(a+b) => "not a"
: a,  b,    =a   => "an equalateral"
: a,  =a,   ?    => "an isosceles"
: ?,  =c,   c    => "an isosceles"
: a,  b,    c    =>  c*c=a*a+b*b -> "a right angled",
                                    "a scalene"


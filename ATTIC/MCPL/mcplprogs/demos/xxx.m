// To demonstrate subtleties of the syntax of patterns.

// Try: mcpl xxx.m tree  to see the parse tree produced.

FUN f
: x (<=100)   => 1
: x <= 101    => 2
: x (100)     => 3
: (x (100))   => 4
: = (f (100)) => 5
: = f (100)   => 6
.

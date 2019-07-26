GET "mcpl.h"

STATIC 
  depth, p, board, count, trycount,
  p1,p2,p3,p4,p5,p6,p7,p8,p9,pA,pB,pC

FUN put
:[?,y], [square(=0)], [piece(=TRUE)] => square, piece := depth, FALSE
                                        TEST depth=12
                                        THEN { count++; pr() }
                                        ELSE try (@y)
                                        square, piece := 0, TRUE

:  => RETURN


FUN try

:    [              ~=0,a1      ]  => try(@a1)

: sq [           a,a1,a2,a3,a4,
       bz,by,bx, b,b1,b2,b3, ?,
        ?,cy,cx, c,c1,c2, ?, ?,
        ?, ?,dx, d,d1, ?, ?, ?,
        ?, ?, ?, e              ]  =>

  depth++

  a := depth

  IF a1=0 DO { a1 := depth
               IF a2=0 DO { a2 := depth
                            IF a3=0 DO { a3 := depth; put(sq,@a4,@p2)
                                                      put(sq, @b,@p3)
                                                      put(sq,@b1,@pB)
                                                      put(sq,@b2,@pB)
                                                      put(sq,@b3,@p3)
                                         a3 := 0
                                       }
                            IF  b=0 DO { b := depth;  put(sq,@bx,@p4)
                                                      put(sq,@b1,@p5)
                                                      put(sq,@b2,@p7)
                                                      put(sq, @c,@p8)
                                         b := 0
                                       }
                            IF b1=0 DO { b1 := depth; put(sq,@b2,@p5)
                                                      put(sq,@c1,@p6)
                                         b1 := 0
                                       }
                            IF b2=0 DO { b2 := depth; put(sq,@b3,@p4)
                                                      put(sq,@c2,@p8)
                                         b2 := 0
                                       }
                            a2 := 0
                          }
               IF  b=0 DO { b := depth
                            IF bx=0 DO { bx := depth; put(sq,@by,@p4)
                                                      put(sq,@cx,@p9)
                                                      put(sq,@b1,@p5)
                                                      put(sq, @c,@p1)
                                         bx := 0
                                       }
                            IF b1=0 DO { b1 := depth; put(sq,@b2,@p5)
                                                      put(sq, @c,@p5)
                                                      put(sq,@c1,@p5)
                                         b1 := 0
                                       }
                            IF  c=0 DO { c := depth;  put(sq,@cx,@pC)
                                                      put(sq,@c1,@p7)
                                                      put(sq, @d,@p3)
                                         c := 0
                                       }
                            b := 0
                          }
               IF b1=0 DO { b1 := depth
                            IF b2=0 DO { b2 := depth; put(sq,@b3,@p4)
                                                      put(sq,@c2,@p9)
                                                   // put(sq,@c1,@p1)
                                         b2 := 0
                                       }
                            IF c1=0 DO { c1 := depth; put(sq, @c,@p7)
                                                      put(sq,@c2,@pC)
                                                      put(sq,@d1,@p3)
                                         c1 := 0
                                       }
                            b1 := 0
                           }
               a1 := 0
             }
  IF  b=0 DO { b := depth
               IF bx=0 DO { bx := depth
                            IF by=0 DO { by := depth; put(sq,@bz,@p3)
                                                      put(sq,@cy,@pC)
                                                      put(sq,@b1,@pB)
                                                      put(sq, @c,@p6)
                                                      put(sq,@cx,@p1)
                                         by := 0
                                       }
                            IF cx=0 DO { cx := depth; put(sq,@cy,@p9)
                                                      put(sq, @c,@p5)
                                                      put(sq,@dx,@p4)
                                                   // put(sq,@b1,@p1)
                                         cx := 0
                                       }
                            IF b1=0 DO { b1 := depth; put(sq,@b2,@pB)
                                                      put(sq, @c,@pA)
                                                   // put(sq,@c1,@p1)
                                         b1 := 0
                                       }
                            IF  c=0 DO { c := depth;  put(sq, @d,@pB)
                                                   // put(sq,@c1,@p1)
                                         c := 0
                                       }
                            bx := 0
                          }
               IF b1=0 DO { b1 := depth
                            IF b2=0 DO { b2 := depth; put(sq,@a2,@p7)
                                                      put(sq,@b3,@p3)
                                                      put(sq, @c,@p6)
                                                      put(sq,@c2,@pC)
                                                      // put(sq,@c1,@p1)
                                         b2 := 0
                                       }
                            IF  c=0 DO { c := depth;  put(sq,@c1,@p5)
                                                      put(sq, @d,@pB)
                                                   // put(sq,@cx,@p1)
                                         c := 0
                                       }
                            IF c1=0 DO { c1 := depth; put(sq,@c2,@p9)
                                                      put(sq,@d1,@p4)
                                         c1 := 0
                                       }
                            b1 := 0
                          }
               IF  c=0 DO { c := depth
                            IF cx=0 DO { cx := depth; put(sq,@cy,@p8)
                                                      put(sq,@dx,@p4)
                                                      put(sq,@c1,@p6)
                                                      put(sq, @d,@pB)
                                         cx := 0
                                       }
                            IF c1=0 DO { c1 := depth; put(sq,@c2,@p8)
                                                      put(sq, @d,@pB)
                                                      put(sq,@d1,@p4)
                                         c1 := 0
                                       }
                            IF  d=0 DO { d := depth;  put(sq,@dx,@p3)
                                                      put(sq,@d1,@p3)
                                                      put(sq, @e,@p2)
                                         d := 0
                                       }
                            c := 0
                          }
               b := 0
             }
  a := 0
  depth--

FUN pr : =>
    writef("\nSolution number %d", count)
    FOR i = 0 TO 12*8-1 DO
    { LET n  = board!i
      LET ch = '*'
      IF 0<=n<=12 DO ch := ".ABCDEFGHIJKL"%n
      IF i MOD 8 = 0 DO newline()
      writef(" %c", ch)
    }
    newline()

FUN start : =>
    writef "Pento version 4 entered\n"

    LET x = -1
    board := [ x,x,x,x,x,x,x,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,0,0,0,0,0,0,x,
               x,x,x,x,x,x,x,x  ]

  // Set all pieces initially unused
  p1,p2,p3,p4,p5,p6,p7,p8,p9,pA,pB,pC ALL:= TRUE

  depth, count := 0, 0

  try board

  writef("\nThe total number of solutions is %d\n", count)
  RETURN 0






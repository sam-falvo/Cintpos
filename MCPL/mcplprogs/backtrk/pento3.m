GET "mcpl.h"

STATIC 
  board, count=0,
  p1=0, p2=0, p3=0, p4=0, p5=0, p6=0,
  p7=0, p8=0, p9=0, pA=0, pB=0, pC=0

FUN try

: 12, ?                             => count++
                                       pr board

:  n, [              ~=0,a1      ]  => try (n, @a1)

:  n, [           a,a1,a2,a3,a4,
        bz,by,bx, b,b1,b2,b3, ?,
         ?,cy,cx, c,c1,c2, ?, ?,
         ?, ?,dx, d,d1, ?, ?, ?,
         ?, ?, ?, e              ] p  =>

  n++

  EVERY
  (   0,  0,  0,  0,  0 )

  : =a1,=a2,=a3,=a4,=p2 => a,a1,a2,a3,a4,p2 ALL:= n; try (n, @a1)
                           a,a1,a2,a3,a4,p2 ALL:= 0
  : =a1,=a2,=a3, =b,=p3 => a,a1,a2,a3, b,p3 ALL:= n; try (n, @a1)
                           a,a1,a2,a3, b,p3 ALL:= 0
  : =a1,=a2,=a3,=b1,=pB => a,a1,a2,a3,b1,pB ALL:= n; try (n, @a1)
                           a,a1,a2,a3,b1,pB ALL:= 0
  : =a1,=a2,=a3,=b2,=pB => a,a1,a2,a3,b2,pB ALL:= n; try (n, @a1)
                           a,a1,a2,a3,b2,pB ALL:= 0
  : =a1,=a2,=a3,=b3,=p3 => a,a1,a2,a3,b3,p3 ALL:= n; try (n, @a1)
                           a,a1,a2,a3,b3,p3 ALL:= 0
  : =a1,=a2, =b,=bx,=p4 => a,a1,a2, b,bx,p4 ALL:= n; try (n, @a1)
                           a,a1,a2, b,bx,p4 ALL:= 0
  : =a1,=a2, =b,=b1,=p5 => a,a1,a2, b,b1,p5 ALL:= n; try (n, @a1)
                           a,a1,a2, b,b1,p5 ALL:= 0
  : =a1,=a2, =b,=b2,=p7 => a,a1,a2, b,b2,p7 ALL:= n; try (n, @a1)
                           a,a1,a2, b,b2,p7 ALL:= 0
  : =a1,=a2, =b, =c,=p8 => a,a1,a2, b, c,p8 ALL:= n; try (n, @a1)
                           a,a1,a2, b, c,p8 ALL:= 0
  : =a1,=a2,=b1,=b2,=p5 => a,a1,a2,b1,b2,p5 ALL:= n; try (n, @a1)
                           a,a1,a2,b1,b2,p5 ALL:= 0
  : =a1,=a2,=b1,=c1,=p6 => a,a1,a2,b1,c1,p6 ALL:= n; try (n, @a1)
                           a,a1,a2,b1,c1,p6 ALL:= 0
  : =a1,=a2,=b2,=b3,=p4 => a,a1,a2,b2,b3,p4 ALL:= n; try (n, @a1)
                           a,a1,a2,b2,b3,p4 ALL:= 0
  : =a1,=a2,=b2,=c2,=p8 => a,a1,a2,b2,c2,p8 ALL:= n; try (n, @a1)
                           a,a1,a2,b2,c2,p8 ALL:= 0
  : =a1, =b,=bx,=by,=p4 => a,a1, b,bx,by,p4 ALL:= n; try (n, @a1)
                           a,a1, b,bx,by,p4 ALL:= 0
  : =a1, =b,=bx,=cx,=p9 => a,a1, b,bx,cx,p9 ALL:= n; try (n, @a1)
                           a,a1, b,bx,cx,p9 ALL:= 0
  : =a1, =b,=bx,=b1,=p5 => a,a1, b,bx,b1,p5 ALL:= n; try (n, @a1)
                           a,a1, b,bx,b1,p5 ALL:= 0
  : =a1, =b,=bx, =c,=p1 => a,a1, b,bx, c,p1 ALL:= n; try (n, @a1)
                           a,a1, b,bx, c,p1 ALL:= 0
  : =a1, =b,=b1,=b2,=p5 => a,a1, b,b1,b2,p5 ALL:= n; try (n, @a1)
                           a,a1, b,b1,b2,p5 ALL:= 0
  : =a1, =b,=b1, =c,=p5 => a,a1, b,b1, c,p5 ALL:= n; try (n, @a1)
                           a,a1, b,b1, c,p5 ALL:= 0
  : =a1, =b,=b1,=c1,=p5 => a,a1, b,b1,c1,p5 ALL:= n; try (n, @a1)
                           a,a1, b,b1,c1,p5 ALL:= 0
  : =a1, =b, =c,=cx,=pC => a,a1, b, c,cx,pC ALL:= n; try (n, @a1)
                           a,a1, b, c,cx,pC ALL:= 0
  : =a1, =b, =c,=c1,=p7 => a,a1, b, c,c1,p7 ALL:= n; try (n, @a1)
                           a,a1, b, c,c1,p7 ALL:= 0
  : =a1, =b, =c, =d,=p3 => a,a1, b, c, d,p3 ALL:= n; try (n, @a1)
                           a,a1, b, c, d,p3 ALL:= 0
  : =a1,=b1,=b2,=b3,=p4 => a,a1,b1,b2,b3,p4 ALL:= n; try (n, @a1)
                           a,a1,b1,b2,b3,p4 ALL:= 0
  : =a1,=b1,=b2,=c2,=p9 => a,a1,b1,b2,c2,p9 ALL:= n; try (n, @a1)
                           a,a1,b1,b2,c2,p9 ALL:= 0
//: =a1,=b1,=b2,=c1,=p1 => a,a1,b1,b2,c1,p1 ALL:= n; try (n, @a1)
//                         a,a1,b1,b2,c1,p1 ALL:= 0
  : =a1,=b1,=c1, =c,=p7 => a,a1,b1,c1, c,p7 ALL:= n; try (n, @a1)
                           a,a1,b1,c1, c,p7 ALL:= 0
  : =a1,=b1,=c1,=c2,=pC => a,a1,b1,c1,c2,pC ALL:= n; try (n, @a1)
                           a,a1,b1,c1,c2,pC ALL:= 0
  : =a1,=b1,=c1,=d1,=p3 => a,a1,b1,c1,d1,p3 ALL:= n; try (n, @a1)
                           a,a1,b1,c1,d1,p3 ALL:= 0
  :  =b,=bx,=by,=bz,=p3 => a, b,bx,by,bz,p3 ALL:= n; try (n, @a1)
                           a, b,bx,by,bz,p3 ALL:= 0
  :  =b,=bx,=by,=cy,=pC => a, b,bx,by,cy,pC ALL:= n; try (n, @a1)
                           a, b,bx,by,cy,pC ALL:= 0
  :  =b,=bx,=by,=b1,=pB => a, b,bx,by,b1,pB ALL:= n; try (n, @a1)
                           a, b,bx,by,b1,pB ALL:= 0
  :  =b,=bx,=by, =c,=p6 => a, b,bx,by, c,p6 ALL:= n; try (n, @a1)
                           a, b,bx,by, c,p6 ALL:= 0
  :  =b,=bx,=by,=cx,=p1 => a, b,bx,by,cx,p1 ALL:= n; try (n, @a1)
                           a, b,bx,by,cx,p1 ALL:= 0
  :  =b,=bx,=cx,=cy,=p9 => a, b,bx,cx,cy,p9 ALL:= n; try (n, @a1)
                           a, b,bx,cx,cy,p9 ALL:= 0
  :  =b,=bx,=cx, =c,=p5 => a, b,bx,cx, c,p5 ALL:= n; try (n, @a1)
                           a, b,bx,cx, c,p5 ALL:= 0
  :  =b,=bx,=cx,=dx,=p4 => a, b,bx,cx,dx,p4 ALL:= n; try (n, @a1)
                           a, b,bx,cx,dx,p4 ALL:= 0
//:  =b,=bx,=cx,=b1,=p1 => a, b,bx,cx,b1,p1 ALL:= n; try (n, @a1)
//                         a, b,bx,cx,b1,p1 ALL:= 0
  :  =b,=bx,=b1,=b2,=pB => a, b,bx,b1,b2,pB ALL:= n; try (n, @a1)
                           a, b,bx,b1,b2,pB ALL:= 0
  :  =b,=bx,=b1, =c,=pA => a, b,bx,b1, c,pA ALL:= n; try (n, @a1)
                           a, b,bx,b1, c,pA ALL:= 0
//:  =b,=bx,=b1,=c1,=p1 => a, b,bx,b1,c1,p1 ALL:= n; try (n, @a1)
//                         a, b,bx,b1,c1,p1 ALL:= 0
  :  =b,=bx, =c, =d,=pB => a, b,bx, c, d,pB ALL:= n; try (n, @a1)
                           a, b,bx, c, d,pB ALL:= 0
//:  =b,=bx, =c,=c1,=p1 => a, b,bx, c,c1,p1 ALL:= n; try (n, @a1)
//                         a, b,bx, c,c1,p1 ALL:= 0
  :  =b,=b1,=b2,=a2,=p7 => a, b,b1,b2,a2,p7 ALL:= n; try (n, @a1)
                           a, b,b1,b2,a2,p7 ALL:= 0
  :  =b,=b1,=b2,=b3,=p3 => a, b,b1,b2,b3,p3 ALL:= n; try (n, @a1)
                           a, b,b1,b2,b3,p3 ALL:= 0
  :  =b,=b1,=b2, =c,=p6 => a, b,b1,b2, c,p6 ALL:= n; try (n, @a1)
                           a, b,b1,b2, c,p6 ALL:= 0
  :  =b,=b1,=b2,=c2,=pC => a, b,b1,b2,c2,pC ALL:= n; try (n, @a1)
                           a, b,b1,b2,c2,pC ALL:= 0
//:  =b,=b1,=b2,=c1,=p1 => a, b,b1,b2,c1,p1 ALL:= n; try (n, @a1)
//                         a, b,b1,b2,c1,p1 ALL:= 0
  :  =b,=b1, =c,=c1,=p5 => a, b,b1, c,c1,p5 ALL:= n; try (n, @a1)
                           a, b,b1, c,c1,p5 ALL:= 0
  :  =b,=b1, =c, =d,=pB => a, b,b1, c, d,pB ALL:= n; try (n, @a1)
                           a, b,b1, c, d,pB ALL:= 0
//:  =b,=b1, =c,=cx,=p1 => a, b,b1, c,cx,p1 ALL:= n; try (n, @a1)
//                         a, b,b1, c,cx,p1 ALL:= 0
  :  =b,=b1,=c1,=c2,=p9 => a, b,b1,c1,c2,p9 ALL:= n; try (n, @a1)
                           a, b,b1,c1,c2,p9 ALL:= 0
  :  =b,=b1,=c1,=d1,=p4 => a, b,b1,c1,d1,p4 ALL:= n; try (n, @a1)
                           a, b,b1,c1,d1,p4 ALL:= 0
  :  =b, =c,=cx,=cy,=p8 => a, b, c,cx,cy,p8 ALL:= n; try (n, @a1)
                           a, b, c,cx,cy,p8 ALL:= 0
  :  =b, =c,=cx,=dx,=p4 => a, b, c,cx,dx,p4 ALL:= n; try (n, @a1)
                           a, b, c,cx,dx,p4 ALL:= 0
  :  =b, =c,=cx,=c1,=p6 => a, b, c,cx,c1,p6 ALL:= n; try (n, @a1)
                           a, b, c,cx,c1,p6 ALL:= 0
  :  =b, =c,=cx, =d,=pB => a, b, c,cx, d,pB ALL:= n; try (n, @a1)
                           a, b, c,cx, d,pB ALL:= 0
  :  =b, =c,=c1,=c2,=p8 => a, b, c,c1,c2,p8 ALL:= n; try (n, @a1)
                           a, b, c,c1,c2,p8 ALL:= 0
  :  =b, =c,=c1, =d,=pB => a, b, c,c1, d,pB ALL:= n; try (n, @a1)
                           a, b, c,c1, d,pB ALL:= 0
  :  =b, =c,=c1,=d1,=p4 => a, b, c,c1,d1,p4 ALL:= n; try (n, @a1)
                           a, b, c,c1,d1,p4 ALL:= 0
  :  =b, =c, =d,=dx,=p3 => a, b, c, d,dx,p3 ALL:= n; try (n, @a1)
                           a, b, c, d,dx,p3 ALL:= 0
  :  =b, =c, =d,=d1,=p3 => a, b, c, d,d1,p3 ALL:= n; try (n, @a1)
                           a, b, c, d,d1,p3 ALL:= 0
  :  =b, =c, =d, =e,=p2 => a, b, c, d, e,p2 ALL:= n; try (n, @a1)
                           a, b, c, d, e,p2 ALL:= 0

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
    writef "Pento version 3 entered\n"

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

    try(0, board)

    writef("\nThe total number of solutions is %d\n", count)

    RETURN 0


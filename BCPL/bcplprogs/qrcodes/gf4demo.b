/*

This is to demonstrate arithmetic on polynomials whose coefficients
are in GF(2^4).

Implemented by Martin Richards (c) Feb 2016
*/

GET "libhdr"

MANIFEST {
  // Parameters for the GF(2^4) arithmetic.

  GFvalmax =15  // GF(2^4) elements are in the range 0 to 15
  GFlog2upb=15 
  GFexp2upb=30  // 30 = 15+15

  GFpoly = #b_1_0011
  GFtbit = #b_1_0000  // =GFvakmax+1
}

GLOBAL {
  gf_log2:ug
  gf_exp2

  G              // The generator polynomial
  M              // Original message 
  C              // Original codeword
  R              // Corrupted codeword


}

LET start() = VALOF
{ newline()
  gf_log2 := 0
  gf_exp2 := 0
  initlogs()

  rs_demo()

  IF gf_log2 DO freevec(gf_log2)
  IF gf_exp2 DO freevec(gf_exp2)

  RESULTIS 0
}

// #################### GF(2^k) Arithmetic ##########################

// Addition and subtraction are the same.
AND gf_add(x, y) = x XOR y

AND gf_sub(x, y) = x XOR y

// Multiplication in GF(2^8) can be done by regular multiplication
// producing a result typically too large to fit in 8 bits, followed
// by taking the remainder after division my 100011101. We will
// implement multiplication using table lookup of discrete logarithm
// and anti-logorithm tables. These are both quite small and can be
// precomputed. We can create these two tables using the function
// initlogs.
AND initlogs() BE
{ LET x = 1
  gf_log2 := getvec(GFlog2upb)
  gf_exp2 := getvec(GFexp2upb)
  UNLESS gf_log2 & gf_exp2 DO
  { writef("initlogs: More space needed*n")
    abort(999)
  }

  // Initialise gf_exp2 with powers of 2 in GF(2^4).
  // While doing so place the inverse entries in gf_log2. Using
  // a double sized exp2 table improves the efficiency of both
  // functions gf_mul and gf_div.

  gf_log2!0 := -1

  FOR i = 0 TO GFvalmax DO // All possible element values
  { gf_exp2!i := x
    gf_exp2!(i+GFvalmax) := x // Note 2^15=1 in GF(2^4)
    gf_log2!x := i
    x := x<<1
    UNLESS (x & GFtbit)=0 DO x := x XOR GFpoly
  }
  RETURN  // Comment out to output the tables

  FOR i = 0 TO GFlog2upb DO
  { IF i MOD 20 = 0 DO writef("*nlog2 %x1: ", i)
    writef(" %x1", gf_log2!i)
  }
  newline()

  FOR i = 0 TO GFexp2upb DO
  { IF i MOD 20 = 0 DO writef("*nexp2 %x1: ", i)
    writef(" %x1", gf_exp2!i)
  }
  newline()
}

AND gf_mul(x, y) = VALOF
{ // Perform GF multiplication using logarithms base 2.
  // Since log 0 is undefined x=0 and y=0 are special cases.
  IF x=0 | y=0 RESULTIS 0
  RESULTIS gf_exp2!(gf_log2!x + gf_log2!y)
}

AND gf_div(x, y) = VALOF
{ // Perform GF division using logarithms base 2.
  // Since log 0 is undefined x=0 and y=0 are special cases.
  IF y=0 DO
  { writef("gf_div: Division by zero*n")
    abort(999)
  }
  IF x=0 RESULTIS 0
  RESULTIS gf_exp2!(GFvalmax + gf_log2!x - gf_log2!y)
}

AND gf_pow(x,y) = gf_exp2!((gf_log2!x * y) MOD GFvalmax)

AND gf_inverse(x) = gf_exp2!(GFvalmax - gf_log2!x)

/*  ################# Polynomial Arithmetc ########################

We now implement some some functions that work on polynomials with GF
coefficients. We will use a vectors to represent a polynomials. Its
zeroth element will be the degree of the polynomial (n, say) and v!1
will be the coefficient x^n, v!2 will be the coefficient x^(n-1), and
so on. So v!(n+1) will be the coefficient of x^0, which is the
constant term.

*/

AND gf_poly_scale(p, x, q) BE
{ // Multiply, using gf_mul, every coefficient of polynomial p by
  // scalar x leaving the result in q, assumed to be large enough.
  LET deg = p!0  // The degree of polynomial p
  q!0 := deg      // The degree of the result
  FOR i = 1 TO deg+1 DO q!i := gf_mul(p!i, x)
} 

AND gf_poly_add(p, q, r) BE
{ // Add polynomials p and q leaving the result in r
  LET degp = p!0 // The number of coefficients is one larger
  LET degq = q!0 // than the degree of the polynomial.
  LET degr = degp
  IF degq>degr DO degr := degq
  // degr is the larger of the degrees of p and q.
  r!0 := degr    // The degree of the result
  FOR i = 1 TO degp+1    DO r!(i+degr-degp) := p!i
  FOR i = 1 TO degr-degp DO r!i := 0 // Pad higher coeffs with 0s
  FOR i = 1 TO degq+1 DO r!(i+degr-degq) := r!(i+degr-degq) XOR q!i
}

// GF addition and subtraction are the same.
AND gf_poly_sub(p, q, r) BE gf_poly_add(p, q, r)

AND gf_poly_mul(p, q, r) BE
{ // Multiply polynomials p and q leaving the result in r
  LET degp = p!0
  LET degq = q!0
  LET degr = degp+degq

  r!0 := degr    // Degree of the result
  FOR i = 1 TO degr+1 DO r!i := 0
  FOR j = 1 TO degq+1 DO
    FOR i = 1 TO degp+1 DO
      r!(i+j-1) := r!(i+j-1) XOR gf_mul(p!i, q!j)
}

AND gf_poly_mulbyxn(p, e, r) BE
{ // Multiply polynomials p by x^e leaving the result in r
  LET degp = p!0
  LET degr = degp + e
  r!0 := degr
  FOR i = 1 TO degp+1 DO r!i := p!i
  FOR i = degp+2 TO degr+1 DO r!i := 0
}

AND gf_poly_eval(p, x) = VALOF
{ // Evaluate polynomial p for a given x using Horner's method.
  // Eg use:  ax^3 + bx^2 + cx^1 + d  =  ((ax + b)x + c)x + d
  LET res = p!1
  FOR i = 2 TO p!0+1 DO
    res := gf_mul(res,x) XOR p!i // mul by x and add next coeff
  RESULTIS res
}

AND gf_generator_poly(e, g) BE
{ // Set in g the polynomial resulting from the expansion of
  // (x-2^0)(x-2^1)(x-2^2) ... (x-2^(e-1))
  // Note that the polynomial has leading term x^(n-1).
  LET t = VEC GFvalmax
  g!0, g!1 := 0, 1 // The polynomial: 1.
  FOR i = 0 TO e-1 DO
  { LET u, v, w = 1, 1, gf_pow(2,i) // (x + 2^i), i in 0 to e-1
    // @u points to polynomial:        (x - 2^i)
    // which in GF arithmetic is also: (x + 2^i)
    FOR i = 0 TO g!0+1 DO t!i := g!i // Copy g into t
    gf_poly_mul(t, @u, g) // Multiply t by (x-2^i) into g
  }
}

AND pr_poly(p) BE
{ FOR i = 1 TO p!0+1 DO writef(" %x1", p!i)
  newline()
}

AND pr_poly_dec(p) BE
{ FOR i = 1 TO p!0+1 DO writef(" %i3", p!i)
  newline()
}

AND gf_poly_divmod(p, q, r) BE
{ // This divides polynomial p by ploynomial q placing
  // the quotient and remainder in r, assumed to be large
  // enough.
  LET degp = p!0   // The degree of polynomial p.
  LET degq = q!0   // The degree of polynomial q.
  LET degr = degp

  LET t = VEC GFvalmax  // Vector to hold the next product of the generator

  UNLESS q!1 > 0 DO
  { writef("The divisor must have a non zero leading coefficient*n")
    abort(999)
    RETURN
  }

  // Copy polynomial p into r.
  r!0 := degr
  FOR i = 1 TO degr+1 DO r!i := p!i

  //writef("q:         "); pr_poly(q)
  //writef("r:         "); pr_poly(r)

  FOR i = 1 TO degp-degq+1 DO
  { LET dig = gf_div(r!i, q!1)
    IF dig DO
    { gf_poly_scale(q, dig, t)
      //writef("scaled  q: ")
      //FOR j = 2 TO i DO writef("   ")
      //pr_poly(t)
      r!i := dig
      FOR j = 2 TO t!0+1 DO r!(i+j-1) := r!(i+j-1) XOR t!j
    }
    //writef("new     r: "); pr_poly(r)
  }
}

AND gf_poly_div(p, q, r) BE
{ gf_poly_divmod(p, q, r)
  r!0 := p!0 - q!0  // Select just the quotient
}

AND gf_poly_mod(p, q, r) BE
{ LET degp = p!0
  LET degq = q!0
  LET degr = degq - 1
  gf_poly_divmod(p, q, r)
  r!0 := degr  // Over write the quotient with the remainder.
  FOR i = 1 TO degr+1 DO r!i := r!(i+degp-degr)
}

// ################# Reed-Solomon Functions #####################

AND rs_encode_msg(msg, e, r) BE
{ // This appends e Reed-Solomon check bytes on the end of the
  // message bytes placing the result in r which is assumed to be
  // large enough.
  LET degm = msg!0      // The degree of the message polynomial.
  LET degr = degm+e     // The degree of the RS codeword polynomial
  LET g = VEC GFvalmax  // Vector to hold the generator.
  LET t = VEC GFvalmax  // Vector to hold a temporary polinomial.

  // Place the message polynomial multiplied by x^n in r.
  r!0 := degr 
  FOR i = 1 TO degm+1       DO r!i := msg!i
  FOR i =  degm+2 TO degr+1 DO r!i := 0
  // Typically msg: 5 6 7
  // r:             5 6 7 0 0 0 0 0 0
  //writef("initial r: "); pr_poly(r)

  gf_generator_poly(e, g)
  // Typically e=6
  // g = (x+2^0)(x+2^1)(x+2^2)(x+2^3)(x+2^4)(x+2^5)
  //   = x^6 + Ax^5 + Fx^4 + 2x^3 + 4x^2 + 3x^1 + 1

  // g!1 is always one.
  writef("generator: "); pr_poly(g)

  // Calculate the remainder after dividing polinomials r by g.
  FOR i = 1 TO degm+1 DO
  { LET dig = r!i
    IF dig DO
    { gf_poly_scale(g, dig, t)
      //writef("scaled  g: ")
      //FOR j = 2 TO i DO writef("   ")
      //pr_poly(t)
      FOR j = 2 TO g!0+1 DO r!(i+j-1) := r!(i+j-1) XOR t!j
    }
    //writef("new     r: "); pr_poly(r)
  }
  // Copy msg (again) into the senior end of r replacing the
  // bytes that currently hold the quotient.
  FOR i = 1 TO degm+1 DO r!i := msg!i
}

AND rs_calc_syndromes(codeword, e, r) BE
{ // e = the number of error correction bytes
  // Typically if e=6 and codeword = 5 6 0 0 7 0 8 8 6
  // r is set as follows:
  // r!0 = 5
  // r!1 = 2  =codeword(2^0)
  // r!2 = 7  =codeword(2^1)
  // r!3 = A  =codeword(2^2)
  // r!4 = 1  =codeword(2^3)
  // r!5 = 2  =codeword(2^4)
  // r!6 = 0  =codeword(2^5)

  // 
  //writef("*nrs_cal_syndromes:*n")
  //writef("codeword:  "); pr_poly(codeword)
  r!0 := e-1  // The degree of the syndromes polynomial.
  FOR i = 0 TO e-1 DO
  { LET p2i = gf_pow(2,i)
    LET res = gf_poly_eval(codeword, p2i)
    //writef("%i2 2^i = %x1 => %x1 %i3*n", i, p2i, res, res)
    r!(i+1) := res // r!(i+1) = codeword(2^i)
  }
}

AND rs_demo() BE
{ LET msg = ?
  LET g = VEC 26 // For the codeword
  LET r = VEC 26 // For the codeword
  LET s = VEC 26 // For the syndromes
  LET c = VEC 26 // Corrupted codeword
  LET t = VEC 26 // Temp polynomial

  writef("*nrs_demo entered*n")

  newline()
  msg := TABLE 2, 5,6,7
  writef("Powers of 2: ") // 1 2 4 8 3 6 C B 5 A 7 E F D 9 1
  FOR i = 0 TO 15 DO writef(" %x1", gf_pow(2,i))
  newline()
  newline()

  rs_encode_msg(msg, 6, r)   // Compute the RS codeword leaving it in r
  writef("message:   "); pr_poly(msg)  // 5 6 7
  newline()
  //writef("generator: "); pr_poly(g)    // 1 A F 2 4 3 1
  writef("codeword:  "); pr_poly(r)    // 5 6 7 E 7 B 8 8 6
  FOR i = 0 TO r!0+1 DO c!i := r!i
  c!3 := 0
  c!4 := 0
  c!6 := 0
  writef("corrupted: "); pr_poly(c)    //  5 6 0 0 7 0 8 8 6
                                       //  1 2 3 4 5 6 7 8 9
  rs_calc_syndromes(c, 6, s) // Compute the syndromes of polynomial c
  writef("syndromes: "); pr_poly(s)

  writef("Note that*n")
  gf_poly_mod(c, (TABLE 1, 1, 1), t)  // (x+2^0)
  writef("Remainder after dividing c by (x+1) is %x1*n", t!1)
  writef("gf_poly_eval(c, 1) is                  %x1*n", gf_poly_eval(c, 1))
  gf_poly_mod(c, (TABLE 1, 1, 2), t)  // (x+2^0)
  writef("Remainder after dividing c by (x+2) is %x1*n", t!1)
  writef("gf_poly_eval(c, 2) is                  %x1*n", gf_poly_eval(c, 2))
  gf_poly_mod(c, (TABLE 1, 1, 4), t)  // (x+2^0)
  writef("Remainder after dividing c by (x+4) is %x1*n", t!1)
  writef("gf_poly_eval(c, 4) is                  %x1*n", gf_poly_eval(c, 4))
  gf_poly_mod(c, (TABLE 1, 1, 8), t)  // (x+2^0)
  writef("Remainder after dividing c by (x+8) is %x1*n", t!1)
  writef("gf_poly_eval(c, 8) is                  %x1*n", gf_poly_eval(c, 8))
  gf_poly_mod(c, (TABLE 1, 1, 3), t)  // (x+2^0)
  writef("Remainder after dividing c by (x+3) is %x1*n", t!1)
  writef("gf_poly_eval(c, 3) is                  %x1*n", gf_poly_eval(c, 3))
  gf_poly_mod(c, (TABLE 1, 1, 6), t)  // (x+2^0)
  writef("Remainder after dividing c by (x+6) is %x1*n", t!1)
  writef("gf_poly_eval(c, 6) is                  %x1*n", gf_poly_eval(c, 6))

newline()
  writef("position 3 corresponds to term x^6*n")
  writef("position 4 corresponds to term x^5*n")
  writef("position 6 corresponds to term x^3*n")

  newline()
  writef("2^(0**6) 2^(0**5) 2^(0**3) = %x1 %x1 %x1*n",
          gf_exp2! 0 , gf_exp2! 0, gf_exp2! 0)
  writef("2^(1**6) 2^(1**5) 2^(1**3) = %x1 %x1 %x1*n",
          gf_exp2! 6 , gf_exp2! 5, gf_exp2! 3)
  writef("2^(2**6) 2^(2**5) 2^(2**3) = %x1 %x1 %x1*n",
          gf_exp2!12 , gf_exp2!10, gf_exp2! 6)
  writef("2^(3**6) 2^(3**5) 2^(3**3) = %x1 %x1 %x1*n",
          gf_exp2! 3 , gf_exp2! 0, gf_exp2! 9)
  writef("2^(4**6) 2^(4**5) 2^(4**3) = %x1 %x1 %x1*n",
          gf_exp2! 9 , gf_exp2! 5, gf_exp2!12)
  writef("2^(5**6) 2^(5**5) 2^(5**3) = %x1 %x1 %x1*n",
          gf_exp2! 0 , gf_exp2!10, gf_exp2! 0)

  newline()

  writef("1**7 + 1**E + 1**B = %x1   -- 2 = S0*n",
          gf_mul(#x1,#x7) XOR gf_mul(#x1,#xE) XOR gf_mul(#x1,#xB))
  writef("C**7 + 6**E + 8**B = %x1   -- 7 = S1*n",
          gf_mul(#xC,#x7) XOR gf_mul(#x6,#xE) XOR gf_mul(#x8,#xB))
  writef("F**7 + 7**E + C**B = %x1   -- A = S2*n",
          gf_mul(#xF,#x7) XOR gf_mul(#x7,#xE) XOR gf_mul(#xC,#xB))
  writef("8**7 + 1**E + A**B = %x1   -- 1 = S3*n",
          gf_mul(#x8,#x7) XOR gf_mul(#x1,#xE) XOR gf_mul(#xA,#xB))
  writef("A**7 + 6**E + F**B = %x1   -- 2 = S4*n",
          gf_mul(#xA,#x7) XOR gf_mul(#x6,#xE) XOR gf_mul(#xF,#xB))
  writef("1**7 + 7**E + 1**B = %x1   -- 0 = S5*n",
          gf_mul(#x1,#x7) XOR gf_mul(#x7,#xE) XOR gf_mul(#x1,#xB))

  writef("*nie Y1=7  Y2=E  Y3=B  assuming errors at a=6 b=5 c=3*n")

  newline()

  // Locator polynomial
  // L(x) = (1 + X1x)(1+X2x)(1+X3x)
  // where X1=2^6, X2=2^5, X3=2^3
  // so
  // L(x) = (1 + 2^6x)(1_2^5x)(1+2^3x)
  //      = 1 + L1x^1 + L2x^2 + L3x^3
  // where L1= , L2= , L3=

  { LET t6 = TABLE 1, #xC, 1
    LET t5 = TABLE 1, #x6, 1
    LET t3 = TABLE 1, #x8, 1
    LET t65 = VEC 26
    LET L   = VEC 26
    gf_poly_mul(t6, t5, t65)
    gf_poly_mul(t65, t3, L)
    writef("Error locator polynomial*n")
    writef("L = "); pr_poly(L)
    writef("L(2^-6) = %x1*n", gf_poly_eval(L, gf_inverse(#xC)))
    writef("L(2^-5) = %x1*n", gf_poly_eval(L, gf_inverse(#x6)))
    writef("L(2^-3) = %x1*n", gf_poly_eval(L, gf_inverse(#x8)))

    writef("( S2 S1 S0 ) x ( L1 ) = ( S3 )*n")
    writef("( S3 S2 S1 )   ( L2 ) = ( S4 )*n")
    writef("( S4 S3 S2 )   ( L3 ) = ( S5 )*n")

    writef("( A 7 2 ) x ( 2 ) = ( %x1 -- 1 = S3 )*n",
           gf_mul(#xA,#x2) XOR gf_mul(#x7,#x7) XOR gf_mul(#x2,#x9))
    writef("( 1 A 7 )   ( 1 )   ( %x1 -- 2 = S4 )*n",
           gf_mul(#x1,#x2) XOR gf_mul(#xA,#x7) XOR gf_mul(#x7,#x9))
    writef("( 2 1 A )   ( 9 )   ( %x1 -- 0 = S5 )*n",
           gf_mul(#x2,#x2) XOR gf_mul(#x1,#x7) XOR gf_mul(#xA,#x9))
  }
abort(9123)
}


AND find_errata_locator(e_pos, r) BE
{ // This function is only used if we know the locations of
  // the error bytes.
  // Compute the erasues/error/errata locator polynomial from
  // given positions. e_pos is a list or error positions numbered
  // from the right hand end of the code word starting at zero.
  // The resulting polynomial is placed in r and is the product
  // of terms of the form (1 - x*2^i) for each i in e_pos.
  // Typical e_pos is TABLE 2, 3,5,6
  // and, r is set to the expanded polinomial
  //       (1-x2^3)(1-x2^5)(1-x2^6)

  r!0, r!1 := 0, 1 // The initial polynomial
  writef("find_errata_locator: e_pos: "); pr_poly_dec(e_pos)
//abort(2000)
  FOR i = 1 TO e_pos!0+1 DO
  { LET e = e_pos!i // e is an exponent of 2 corresponding to
                    // the location of an error in the corrupted
                    // codeword.
    LET u, v, w = 1, gf_pow(2, e), 1 // term = (1 - x*2^e)
    LET t = VEC GFvalmax
    FOR i = 0 TO r!0+1 DO t!i := r!i // Copy r into t
    writef("*nfind_errata_locator: e=%n*n", e)
    writef("Multiplying:  "); pr_poly(t)
    writef("by term:      "); pr_poly(@u)
    gf_poly_mul(t, @u, r) // r := r * term
    writef("Giving:       "); pr_poly(r)
  }
}

AND test_find_errata_locator() BE
{ LET poslist = TABLE 3, 1, 4, 5
  LET e_loc = VEC 20
  LET g = VEC 20
  LET r = VEC 20
  LET c = VEC 20
  LET msg = TABLE 3, 1, 2, 3, 4

  writef("*nTesting find_errata_locator*n")

  newline()
  gf_generator_poly(6, g)    // Compute the generator polinomial
  rs_encode_msg(msg, 6, r)   // Compute the RS codeword leaving it in r

  newline()
  writef("message:   "); pr_poly(msg)  // 12 34 56
  writef("generator: "); pr_poly(g)    // 01 3F 01 DA 20 E3 26
  writef("codeword:  "); pr_poly(r)    // 12 34 56 78 17 A7 56 4B 78 DD
  FOR i = 0 TO r!0+1 DO c!i := r!i
  c!3 := 0
  c!4 := 0
  c!6 := 0
  writef("corrupted: "); pr_poly(c)    // 12 34 00 00 17 00 56 4B 78 DD
  rs_calc_syndromes(c, 6, r) // Compute the syndromes of polynomial c
  writef("syndromes: "); pr_poly(r)

  newline()

  writef("%x1    %x1 %x1 %x1*n", gf_poly_eval(c, gf_exp2!(3)), // S3
                                 gf_poly_eval(c, gf_exp2!(2)), // S2
                                 gf_poly_eval(c, gf_exp2!(1)), // S1
                                 gf_poly_eval(c, gf_exp2!(0))) // S0

  writef("%x1    %x1 %x1 %x1*n", gf_poly_eval(c, gf_exp2!(4)), // S4
                                 gf_poly_eval(c, gf_exp2!(3)), // S3
                                 gf_poly_eval(c, gf_exp2!(2)), // S2
                                 gf_poly_eval(c, gf_exp2!(1))) // S1

  writef("%x1    %x1 %x1 %x1*n", gf_poly_eval(c, gf_exp2!(5)), // S5
                                 gf_poly_eval(c, gf_exp2!(4)), // S4
                                 gf_poly_eval(c, gf_exp2!(3)), // S3
                                 gf_poly_eval(c, gf_exp2!(2))) // S2
  newline()

/*
Lambda(x) = (1+x*2^e1)(1+x*2^e2)(1+x*2^e3)
          = 1 + L1*x + L2*x^2 + L3*x^3

where e1, e2 and e3 are the powers of 2 corresponding to the
error locations in the corrupted codeword.
*/

  { LET Lambda = VEC 40
    LET d1, a1, b1 = 1, gf_exp2!7, 1 // (1+x2^7)
    LET d2, a2, b2 = 1, gf_exp2!6, 1 // (1+x2^6)
    LET d3, a3, b3 = 1, gf_exp2!4, 1 // (1+x2^4)
    writef("t1= "); pr_poly(@d1)
    writef("t2= "); pr_poly(@d2)
    writef("t3= "); pr_poly(@d3)
    newline()
    gf_poly_mul(@d1, @d2, r)
    writef("t1**t2= "); pr_poly(r)
    gf_poly_mul(r, @d3, Lambda)
    writef("Lambda= "); pr_poly(Lambda)
    FOR x = 0 TO GFvalmax DO
    { LET a = gf_poly_eval(@d1, x)
      LET b = gf_poly_eval(@d2, x)
      LET c = gf_poly_eval(@d3, x)
      LET prod = gf_mul(gf_mul(a, b), c)
      UNLESS x<6 | x=#x1B | x=#x36 | x=#xD8 LOOP
      writef("t1(%x1)=%x1 t2(%x1)=%x1 t3(%x1)=%x1 prod=%x1 L(%x1)=%x1*n",
              x,a,        x,b,         x,c,     prod, x,gf_poly_eval(Lambda,x))
    }
    newline()
    { LET e = TABLE 9, #x00,#x00,#x56,#x78,#x00,#xA7,#x00,#x00,#x00,#x00
      FOR i = 0 TO 5 DO
      { LET x = gf_poly_eval(e, gf_exp2!i)
        writef("E(2^%n) = %x1*n", i, x)
      }
      newline()
    }
    writef("2^-7= %x1*n", gf_inverse(gf_exp2!7))
    writef("2^-6= %x1*n", gf_inverse(gf_exp2!6))
    writef("2^-4= %x1*n", gf_inverse(gf_exp2!4))

    writef("Lambda(2^-7)= %x1*n", gf_poly_eval(Lambda, gf_inverse(gf_exp2!7)))
    writef("Lambda(2^-6)= %x1*n", gf_poly_eval(Lambda, gf_inverse(gf_exp2!6)))
    writef("Lambda(2^-4)= %x1*n", gf_poly_eval(Lambda, gf_inverse(gf_exp2!4)))
    newline()
/*
    ( 6E ) = ( DE 81 89 ) x ( L1 )
    ( 82 )   ( 6E DE 81 )   ( L2 )
    ( 7A )   ( 82 6E DE )   ( L3 )

    giving L1=D0, L2=1B and L3=98.
*/
    writef("DE**D0+81**1B+89**98 = %x1    -- 6E*n",
           gf_mul(#xDE,#xD0) XOR gf_mul(#x81,#x1B) XOR gf_mul(#x89,#x98))
    writef("6E**D0+DE**1B+81**98 = %x1    -- 82*n",
           gf_mul(#x6E,#xD0) XOR gf_mul(#xDE,#x1B) XOR gf_mul(#x81,#x98))
    writef("82**D0+6E**1B+DE**98 = %x1    -- 7A*n",
           gf_mul(#x82,#xD0) XOR gf_mul(#x6E,#x1B) XOR gf_mul(#xDE,#x98))
  }

  newline()
  poslist := TABLE 2, 7,6,4
  find_errata_locator(poslist, e_loc)  
  writef("poslist= "); pr_poly_dec(poslist)
  writef("e_loc=   "); pr_poly(e_loc)
  newline()
//abort(5555)
}

AND rs_find_error_locator(synd, err_loc) BE
{ // This sets err_loc to the error locator polynomial
  // given the syndromes polynomial. It is only used
  // when we do not know the locations of any of the
  // error bytes, so the maximum number of error that
  // can be found is the (synd!0+1)/2. It uses the
  // Berlekamp-Massey algorithm.
  LET old_loc = VEC 50
  LET degs = synd!0
  LET k, l = 1, 0
  LET Lambda = VEC 50  // To hold the error locator polynomial
  LET newL   = VEC 50  // To hold the error locator polynomial
  LET C      = VEC 50  // To hold a correction polynomial
  LET P1     = VEC 50
  LET P2     = VEC 50

  Lambda!0, Lambda!1 := 0, 1  // Polynomial: 1
  C!0, C!1, C!2 := 1, 1, 0    // Polynomial: x+0

  UNTIL k > degs+1 DO // degs+1 = number of correction bytes
  { LET delta = 0 //synd!1 // codeword(2^0)
    LET degL = Lambda!0
    //newline()
    //writef("Lambda: "); pr_poly(Lambda)
    //writef("C:      "); pr_poly(C)
    //writef("synd:   "); pr_poly(synd)
    //writef("k=%n l=%n*n", k, l)

    FOR i = 0 TO l DO
    { LET Li = Lambda!(degL+1-i) // Coeff of x^i in current Lambda
      LET f = synd!(k-i)         // S(2^(k-i))
      LET Lif = gf_mul(Li, f)
      //writef("i=%n delta: %x1*n", i, delta)
      delta := delta XOR Lif
      //writef("i=%n Li=%x1 f=%x1 Lif=%x1 => delta=%x1*n",
      //        i,   Li,    f,    Lif,       delta)
    }
    //writef("delta: %x1*n", delta)

    IF delta DO
    { gf_poly_scale(C, delta, P1)
      //writef("Multiply C by delta=%x1 giving: ", delta); pr_poly(P1)
      gf_poly_add(P1, Lambda, newL)
      //writef("Add L giving newL              "); pr_poly(newL)
      IF 2*l < k DO
      { l := k-l
        gf_poly_scale(Lambda, gf_inverse(delta), C)
        //writef("Since 2**l < k set C = Lambda/delta: "); pr_poly(C)
      }
    }

    // Multiply C by x
    C!0 := C!0 + 1
    C!(C!0+1) := 0
    //writef("Multiply C by x giving: "); pr_poly(C)

    FOR i = 0 TO newL!0+1 DO Lambda!i := newL!i
    //writef("Set new version of Lambda:   "); pr_poly(Lambda)
    k := k+1
  }
  FOR i = 0 TO Lambda!0+1 DO err_loc!i := Lambda!i

  newline()
  writef("*nLambda:          "); pr_poly(Lambda)
  newline()
  FOR i = 15 TO 20 DO
  {// writef("Lambda(2^%i2): %x1*n", i, gf_poly_eval(Lambda, gf_exp2(i)))
  }
}

AND test_rs_find_error_locator() BE
{ LET g = VEC 20
  LET r = VEC 20
  LET c = VEC 20
  LET Lambda = VEC 20
  LET msg = ?

  writef("*nTesting rs_find_error_locator*n")


  //newline()

  // This test case uses GF(2^8)

  msg := TABLE 3, 1, 2, 3, 4
  gf_generator_poly(6, g)    // Compute the generator polinomial
  rs_encode_msg(msg, 6, r)   // Compute the RS codeword leaving it in r

  newline()
  writef("message:   "); pr_poly(msg)  // 12 34 56
  writef("generator: "); pr_poly(g)    // 01 3F 01 DA 20 E3 26
  writef("codeword:  "); pr_poly(r)    // 12 34 56 78 17 A7 56 4B 78 DD
  FOR i = 0 TO r!0+1 DO c!i := r!i
  c!3 := 0
  c!4 := 0
  c!6 := 0
  writef("corrupted: "); pr_poly(c)    // 12 34 00 00 17 00 56 4B 78 DD
  rs_calc_syndromes(c, 6, r) // Compute the syndromes of polynomial c

  // Follow the worked example in CKP Clarke's BBC paper.
  // To agree with the paper we must use GF(2^4)
  //r := TABLE 3, 15, 3, 4, 12
  writef("syndromes: "); pr_poly(r)

  rs_find_error_locator(r, Lambda)
  writef("Lambda:   "); pr_poly(Lambda)
}

AND rs_find_error_evaluator(synd, Lambda, Omega) BE
{ // Compute the error evaluator polynomial omega.
  // 
  // omega(x) = (synd(x) * Lambda(x)) MOD x^(e+1) // e=n-k
  LET degs = synd!0

  gf_poly_mul(synd, Lambda, Omega)
  writef("synd:            ");  pr_poly(synd)
  writef("lambda:          ");  pr_poly(Lambda)
  writef("synd ** Lambda:   "); pr_poly(Omega)
  FOR i = 0 TO degs DO Omega!(i+1) := Omega!(i+1+Omega!0-degs)
  Omega!0 := degs
  writef("Omega:           "); pr_poly(Omega)
}

AND rs_demo1() BE
{ LET msg  = ?
  LET n    = ?      // Code word length
  LET k    = ?      // Message length
  LET e    = ?      // The number of check bytes  

  LET S       = VEC 100 // For the syndromes polynomial
  AND R       = VEC 100 // For the codeword for msg
  AND C       = VEC 100 // For the corrupted codeword
  AND G       = VEC 100 // For the generator polynomial
  AND Lambda  = VEC 100 // For the erasures polynomial
  AND Ldash   = VEC 100 // For d/dx of Lambda
  AND Omega   = VEC 100 // For the evaluator polynomial
  AND err_pos = VEC 100 // For the error positions

  writef("*nTesting rs_find_error_evaluator*n*n")

  msg := TABLE 3, 1, 2, 3, 4

  k := 4   // Message bytes
  e := 6    // correction bytes
  n := k+e  // codeword bytes

  gf_generator_poly(6, G)  // Compute the generator polinomial
  rs_encode_msg(msg, 6, R) // Compute the RS codeword leaving it in R

  newline()
  writef("message:   "); pr_poly(msg) // 12 34 56
  writef("generator: "); pr_poly(G)   // 01 3F 01 DA 20 E3 26
  writef("codeword:  "); pr_poly(R)   // 12 34 56 78 17 A7 56 4B 78 DD
  FOR i = 0 TO R!0+1 DO C!i := R!i
  C!3 := 0
  C!4 := 0
  C!6 := 0
  writef("corrupted: "); pr_poly(C)   // 12 34 00 00 17 00 56 4B 78 DD
  rs_calc_syndromes(C, 6, S)          // syndromes of polynomial c

  writef("syndromes: "); pr_poly(S)

  rs_find_error_locator(S, Lambda)
  writef("Lambda:   "); pr_poly(Lambda)
  rs_find_error_evaluator(S, Lambda, Omega)
  writef("Omega:    "); pr_poly(Omega)
  newline()

writef("Checking Omega*n")
writef("O0 = S0: %x1=%x1*n", Omega!(Omega!0+1), S!(S!0+1))
writef("O1 = S1+S0L1: %x1=%x1*n", Omega!(Omega!0),
       gf_add(S!(S!0), gf_mul(S!(S!0+1),Lambda!(Lambda!0))))
abort(7777)
  writef("Lambda:     "); pr_poly(Lambda)


  { LET p, q = 1, 0
    writef("Lambda:     "); pr_poly(Lambda)

    FOR i = Lambda!0 TO 0 BY -1 DO
    { // Just copy the coefficients of the odd powers of lambda
//writef("p=%i2: ", p)
      IF (i & 1) = 1 DO
      { Ldash!0 := q
        q := q+1
        Ldash!q := Lambda!p
//writef("Ldash!%n = %x1", q+1, Lambda!p)
      }
//newline()
      p := p+1
    }
    writef("Ldash:      "); pr_poly(Ldash)
  }

  err_pos!0 := -1
  FOR i = 0 TO n-1 DO
  { LET a = gf_poly_eval(Lambda, gf_inverse(gf_exp2!i))
    writef("Lambda(2^%i2) = %x1*n", i, a)
    IF a=0 DO
    { LET p = gf_poly_eval(Omega,  gf_inverse(gf_exp2!(i)))
      LET q = gf_poly_eval(Ldash,  gf_inverse(gf_exp2!(2*i)))
      LET j = err_pos!0
      j := j+1
      err_pos!0 := j
      err_pos!(j+1) := i
//writef("setting err_pos  j=%n i=%n*n", j, i)
      writef("Correction: %x1 from %x1/%x1*n",
              gf_mul(gf_div(p, q),gf_exp2!i), p, q)
    }
  }
  //writef("err_pos:     "); pr_poly_dec(err_pos)
}

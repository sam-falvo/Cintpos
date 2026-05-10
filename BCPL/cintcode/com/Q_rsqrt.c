#include <stdio.h>

float Q_rsqrt(float number)
{ // Calculate 1/sqrt(number) approx about +/-1%
  long i;
  float x2, y;
  const float threehalfs = 1.5F;

  x2 = number * 0.5F;
  y  = number;
  i  = * ( long *) &y;
  i  = 0x5f3759df - (i>>1);
  y  = * ( float *) &i;
  y  = y * ( threehalfs - (x2*y*y));
  y  = y * ( threehalfs - (x2*y*y)); // can be removed
}

int main() {
  float a;
  a = Q_rsqrt(3.0);
  
  printf("1/sqrt(3.0) = %9.6f\n", a);
  return 0;
}

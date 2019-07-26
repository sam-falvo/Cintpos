#include <stdio.h>

int main() {
  printf("%20.6f\n", 123.456789);
  printf("%20.3e\n", 123.456789);
  printf("%20.6f\n", 123.456789e20);
  printf("%20.3e\n", 123.456789e20);
  printf("%20.6f\n", 123.456789e-20);
  printf("%20.3e\n", 123.456789e-20);
}

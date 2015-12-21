#include <stdio.h>

int main() {
  int x;
  double y;
  double z;
  char *s;
  int b;
  int c;

  x = 4;
  x = 5;
  printf("%d\n", x);

  y = 9.0;
  y = 10.0;
  printf("%.2f\n", y);

  z = y + 2.0;
  printf("%.2f\n", z);

  s = "test";
  s = "new";
  printf("%s\n", s);

  b = 1;
  printf("%d\n", b);

  c = 0;
  printf("%d\n", c);

  return 0;
}

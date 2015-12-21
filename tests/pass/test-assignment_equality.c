#include <stdio.h>

int main() {
  int x;
  int y;
  printf("%d\n", (x = 10) == (y = 10));
  printf("%d\n", y);
  x = (y = 9);
  printf("%d\n", x);
  printf("%d\n", y);
  return 0;
}

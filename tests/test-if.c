#include <stdio.h>

int main() {
  int x = 2;
  if (x == 2) {
      x = x * 5;
      printf("%s\n", "x is 2");
      printf("%d\n", x);
  }
  else {
      x = x / 10;
      printf("%s\n", "x is not 2");
      printf("%d\n", x);
  }
  return 0;
}

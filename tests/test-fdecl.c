#include <stdio.h>

void foo(int x, int y) {
    printf("%d\n", x+y);
}

void bar(char *s, int b) {
  printf("%s\n", s);
  printf("%d\n", b);
}

int main() {
    bar("hello", 1 == 1);
    foo(2,4);
    printf("%s\n", "main");
    return 0;
}

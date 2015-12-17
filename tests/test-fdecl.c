#include <stdio.h>

void foo(int x, int y) {
    printf("%s\n", "foo");
}

int main() {
    foo(2,4);
    printf("%s\n", "main");
    return 0;
}

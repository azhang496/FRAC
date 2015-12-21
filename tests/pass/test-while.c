#include <stdio.h>

int main() {
    int x = 1;
    while(x <= 2) {
        printf("%s\n", "x is less than or equal to 2");
        printf("%d\n", x);
        x = x + 1;
    }
    printf("%d\n", x);
    return 0;
}

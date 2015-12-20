#include "turtle.h"
#include <string.h>
#include <stdio.h>

void koch_flake(char var, int iter) {
    if (iter < 0) {
        if (var == 'F') {
            turtle_forward(1);
        }
    } else {
        if (var == 'F') {
            koch_flake('F', iter - 1);
            koch_flake('m', iter - 1);
            koch_flake('F', iter - 1);
            koch_flake('p', iter - 1);
            koch_flake('p', iter - 1);
            koch_flake('F', iter - 1);
            koch_flake('m', iter - 1);
            koch_flake('F', iter - 1);
        }
        if (var == 'p') {
            turtle_turn_left(60);
        }
        if (var == 'm') {
            turtle_turn_right(60);
        }
    }
}

void koch_flake_start(int iter) {
    koch_flake('F', iter);
    koch_flake('p', iter);
    koch_flake('p', iter);
    koch_flake('F', iter);
    koch_flake('p', iter);
    koch_flake('p', iter);
    koch_flake('F', iter);
}

int main() {
    turtle_init(2000,2000);
    koch_flake_start(5);
    turtle_save_bmp("koch_flake.bmp");
    turtle_cleanup();
    return 0;
}

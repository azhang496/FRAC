#include "turtle.h"
#include <string.h>
#include <stdio.h>

void koch(char var, int iter, int step) {
  if (iter < 0) {
    if (var == 'F') {
      turtle_forward(step);
    }
  } else {
    if(var == 'F') {
      koch('F', iter - 1, step);
      koch('m', iter - 1, step);
      koch('F', iter - 1, step);
      koch('p', iter - 1, step);
      koch('p', iter - 1, step);
      koch('F', iter - 1, step);
      koch('m', iter - 1, step);
      koch('F', iter - 1, step);
    }
    if (var == 'm') {
      turtle_turn_right(60);
    }
    if (var == 'p') {
      turtle_turn_left(60);
    }
  }
}

void koch_start(int iter, int step) {
  koch('F', iter, step);
  koch('p', iter, step);
  koch('p', iter, step);
  koch('F', iter, step);
  koch('p', iter, step);
  koch('p', iter, step);
  koch('F', iter, step);
}

int main() {
    int koch = 3;
    turtle_init(2000, 2000);
    koch_start(5, 1);
    turtle_save_bmp("koch.bmp");
    turtle_cleanup();
    printf("%d\n", koch);
    return 0;
}

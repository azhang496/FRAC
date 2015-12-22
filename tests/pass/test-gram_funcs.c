#include "turtle.h"
#include <string.h>
#include <stdio.h>

void koch(char var, int iter) {
  if (iter < 0) {
    if (var == 'F') {
      turtle_forward(1);
    }
  } else {
    if(var == 'F') {
      koch('F', iter - 1);
      koch('m', iter - 1);
      koch('F', iter - 1);
      koch('p', iter - 1);
      koch('p', iter - 1);
      koch('F', iter - 1);
      koch('m', iter - 1);
      koch('F', iter - 1);
    }
    if (var == 'm') {
      turtle_turn_right(60);
    }
    if (var == 'p') {
      turtle_turn_left(60);
    }
  }
}

void koch_start(int iter) {
  koch('F', iter);
  koch('p', iter);
  koch('p', iter);
  koch('F', iter);
  koch('p', iter);
  koch('p', iter);
  koch('F', iter);
}

int main() {
    int koch = 3;
    turtle_init(2000, 2000);
    koch_start(5);
    turtle_save_bmp("koch.bmp");
    turtle_cleanup();
    printf("%d\n", koch);
    return 0;
}

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
  turtle_init(2000, 2000);
  koch_start(5);
  turtle_save_bmp("koch.bmp");
  turtle_cleanup();
  char buf[1024];
  int i;
  for(i = 0; i <5; i++) {
    turtle_init(2000, 2000);
    koch_start(i+1);
    sprintf(buf, "koch%02d.bmp", i);
    turtle_save_bmp(buf);
    turtle_cleanup();
  }
  return 0;
}

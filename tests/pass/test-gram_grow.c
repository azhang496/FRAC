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
  turtle_init(2000, 2000);
  koch_start(5, 1);
  turtle_save_bmp("koch.bmp");
  turtle_cleanup();
  char buf[1024];
  int i;
  int arr[] = {50,20,8,3,1};
  for(i = 0; i <5; i++) {
    turtle_init(2000, 2000);
    koch_start(i+1, arr[i]);
    sprintf(buf, "koch%02d.bmp", i);
    turtle_save_bmp(buf);
    turtle_cleanup();
  }
  return 0;
}

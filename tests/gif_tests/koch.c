#include "turtle.h"
#include <string.h>
#include <stdio.h>

void my_gram(char var, int iter, int step) {
if (iter < 0) {
if (var == 'F') {
turtle_forward(step);
}
} else {
if(var == 'F') {
my_gram('F', iter - 1, step);
my_gram('m', iter - 1, step);
my_gram('F', iter - 1, step);
my_gram('p', iter - 1, step);
my_gram('p', iter - 1, step);
my_gram('F', iter - 1, step);
my_gram('m', iter - 1, step);
my_gram('F', iter - 1, step);
}
if (var == 'p') {
turtle_turn_left(60);
}
if (var == 'm') {
turtle_turn_right(60);
}
}
}
void my_gram_start(int iter, int step) {
my_gram('F', iter,step);
my_gram('p', iter,step);
my_gram('p', iter,step);
my_gram('F', iter,step);
my_gram('p', iter,step);
my_gram('p', iter,step);
my_gram('F', iter,step);
}
int main(){
char buf[1024];
int arr[] = { 50,20,8,3,1 };
for(int i = 0; i <5; i++) {
turtle_init(2000, 2000);
my_gram_start(i+1,arr[i]);
sprintf(buf, "my_gram%d.bmp", i);
turtle_save_bmp(buf);
turtle_cleanup();
}
return 0;
}

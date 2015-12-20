#include "turtle.h"
#include <stdio.h>
#include <string.h>

void my_gram(char var, int iter)
{
if (iter < 0) {
if (var == 'a') {
turtle_turn_left(1);
}
if (var == 'b') {
turtle_turn_left(2);
}
} else {
if(var == 'b') {
my_gram('b', iter - 1);
}
if(var == 'a') {
my_gram('a', iter - 1);
my_gram('b', iter - 1);
my_gram('b', iter - 1);
}
}
}
void my_gram_start(int iter)
{
my_gram('a', iter - 1);
my_gram('b', iter - 1);
my_gram('b', iter - 1);
my_gram('b', iter - 1);
my_gram('a', iter - 1);
my_gram('a', iter - 1);
my_gram('b', iter - 1);
my_gram('a', iter - 1);
my_gram('b', iter - 1);
my_gram('a', iter - 1);
}
int main(){
turtle_init(2000, 2000);
my_gram_start(2);
turtle_save_bmp("my_gram.bmp");
turtle_cleanup();
turtle_init(2000, 2000);
return 0;
}


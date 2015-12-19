#include "turtle.h"
#include <stdio.h>
#include <string.h>

void my_gram(char var, int iter)
{
    if (iter < 0) {
        if (var == 'a') {
            turtle_turn_left(1);
        }
    } else {
        if(var == 'a') {
            my_gram('b', iter - 1);
            my_gram('a', iter - 1);
            my_gram('a', iter - 1);
            my_gram('b', iter - 1);
            my_gram('a', iter - 1);
            my_gram('a', iter - 1);
        }
        if (var == 'b') {
            turtle_forward(1);
        }
    }
}
void my_gram_start(int iter)
{
my_gram('a', iter - 1);
my_gram('a', iter - 1);
my_gram('b', iter - 1);
my_gram('b', iter - 1);
my_gram('b', iter - 1);
}
void foo(){
}
int main(){
return 0;
}


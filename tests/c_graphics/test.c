#include "turtle.h"
#include <string.h>
#include <stdio.h>

void koch_flake(char var, int iter)
{
	if (iter == 0) {
		if (var == 'F')
			turtle_forward(1);
	}
	else {
		if (var == 'F') {
			koch_flake('F', iter - 1);
			turtle_turn_left(60);
			koch_flake('F', iter - 1);
			turtle_turn_right(60);
			turtle_turn_right(60);
			koch_flake('F', iter - 1);
			turtle_turn_left(60);
			koch_flake('F', iter - 1);
		}
	}
}	

void koch_flake_start(int iter)
{
	koch_flake('F', iter);
	turtle_turn_right(60);
	turtle_turn_right(60);
	koch_flake('F', iter);
	turtle_turn_right(60);
	turtle_turn_right(60);
	koch_flake('F', iter);
}

void dragon(char var, int iter)
{
	if (iter == 0) 
		;
	else {
		if (var == 'X') {
			dragon('X', iter - 1);
			turtle_turn_right(90);
			dragon('Y', iter - 1);
			turtle_forward(5);
			turtle_turn_right(90);
		}
		if (var == 'Y') {
			turtle_turn_left(90);
			turtle_forward(5);
			dragon('X', iter - 1);
			turtle_turn_left(90);
			dragon('Y', iter - 1);
		}
	}
}

void dragon_start(int iter)
{
	turtle_forward(5);
	dragon('X', iter);
}

int main()
{
	turtle_init(2000,2000);
	koch_flake_start(6);
	turtle_save_bmp("koch_flake.bmp");
	turtle_cleanup();
	turtle_init(2000, 2000);
	dragon_start(15);
	turtle_save_bmp("dragon.bmp");
	
	turtle_cleanup();

	return 0;
}

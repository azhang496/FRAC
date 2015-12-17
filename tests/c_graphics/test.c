#include "turtle.h"
#include <string.h>
#include <stdio.h>

void koch_flake(char var, int iter)
{
	if (iter < 0) {
		/* if a rule can evaluate to a non-terminal 
		AND a terminal, the terminal output ends up here
		after all iterations have completed */
		if (var == 'F') {
			turtle_forward(1);
		}
	} else {
		/* rule evaluation */
		if (var == 'F') {
			koch_flake('F', iter - 1);
			koch_flake('-', iter - 1);
			koch_flake('F', iter - 1);
			koch_flake('+', iter - 1);
			koch_flake('+', iter - 1);
			koch_flake('F', iter - 1);
			koch_flake('-', iter - 1);
			koch_flake('F', iter - 1);
		}
		/* these rules only evaluate to a terminal, so they 
		are executed here instead of waiting for iterations */
		if (var == '-') {
			turtle_turn_right(60);
		}
		if (var == '+') {
			turtle_turn_left(60);
		}
	}
}	
/* start state */
void koch_flake_start(int iter)
{
	/* init string */
	koch_flake('F', iter);
	koch_flake('+', iter);
	koch_flake('+', iter);
	koch_flake('F', iter);
	koch_flake('+', iter);
	koch_flake('+', iter);
	koch_flake('F', iter);
}

void dragon(char var, int iter)
{
	if (iter < 0) {
		;
	} else { 
		if (var == 'X') {
			dragon('X', iter - 1);
			dragon('+', iter - 1);
			dragon('Y', iter - 1);
			dragon('F', iter - 1);
		}
		if (var == 'Y') {
			dragon('F', iter - 1);
			dragon('X', iter - 1);
			dragon('-', iter - 1);
			dragon('Y', iter - 1);
		}
		if (var == 'F') {
			turtle_forward(5);
		}
		if (var == '+') {
			turtle_turn_left(90);
		}
		if (var == '-') {
			turtle_turn_right(90);
		}
	}
}

void dragon_start(int iter)
{
	dragon('F', iter);
	dragon('X', iter);
}

void sierp_tri(char var, int iter)
{
	if (iter < 0) {
		if (var == 'A') {
			turtle_forward(1);
		}
		if (var == 'B') {
			turtle_forward(1);
		}
	} else {
		if (var == 'A') {
			sierp_tri('+', iter - 1);
			sierp_tri('B', iter - 1);
			sierp_tri('-', iter - 1);
			sierp_tri('A', iter - 1);
			sierp_tri('-', iter - 1);
			sierp_tri('B', iter - 1);
			sierp_tri('+', iter - 1);
		}
		if (var == 'B') {
			sierp_tri('-', iter - 1);
			sierp_tri('A', iter - 1);
			sierp_tri('+', iter - 1);
			sierp_tri('B', iter - 1);
			sierp_tri('+', iter - 1);
			sierp_tri('A', iter - 1);
			sierp_tri('-', iter - 1);
		}
		if (var == '+') {
			turtle_turn_left(60);
		}
		if (var == '-') {
			turtle_turn_right(60);
		}
	}								
}

void sierp_tri_start(int iter)
{
	sierp_tri('A', iter);
}

int main()
{
	turtle_init(2000,2000);
	koch_flake_start(5);
	turtle_save_bmp("koch_flake.bmp");
	turtle_cleanup();

	turtle_init(2000, 2000);
	dragon_start(15);
	turtle_save_bmp("dragon.bmp");	
	turtle_cleanup();

	turtle_init(2000, 2000);
	sierp_tri_start(8);
	turtle_save_bmp("sierp_tri.bmp");
	turtle_cleanup();
	return 0;
}

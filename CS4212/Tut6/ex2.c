/*
	Benjamin Tan Wei Hao
	U077129N
	Tutorial 6 Exercise 2
*/

#include <stdlib.h>
#include <stdio.h>

struct drawable {
	void (* draw) (struct drawable * self);
} ; 

void drawable_draw(struct drawable * self) {
	printf("Generic drawable object\n");
}

void init_drawable(struct drawable * self) {
	self->draw = &drawable_draw;
}

struct drawable * make_drawable() {
	struct drawable * retVal = malloc(sizeof(struct drawable));
	init_drawable(retVal);
	return retVal;
}

struct square {
	int x;
	int y;
	int side;
	void (* draw) (struct square * self);
	void (* super_draw) (struct square * self);
};

void square_draw(struct square * self) {
	printf("Square with corner at (%d,%d) and side %d\n", 
		   self->x, self->y, self->side);
	self->super_draw = self->draw;
}

void init_square(struct square * self) {
	init_drawable(self);
	self->draw = &square_draw;
}

struct square * make_square(int x, int y, int side) {
	struct square * retVal = malloc(sizeof(struct square));
	init_square(retVal);
	retVal->x = x;
	retVal->y = y;
	retVal->side = side;
	return retVal;
}

struct circle {
	int x;
	int y;
	int radius;
	void (* draw) (struct circle * self);
	void (* super_draw) (struct circle * self);	
};

void circle_draw(struct circle * self) {
	printf("Circle with center at (%d,%d) and radius %d\n",
			self->x, self->y, self->radius);
	self->super_draw = self->draw; // <----
}

void init_circle(struct circle * self) {
	init_drawable(self);
	self->draw = &circle_draw;
}

struct circle * make_circle(int x, int y, int radius) {
	struct circle * retVal = malloc(sizeof(struct circle));
	init_circle(retVal);		// <--- 
	retVal->x = x;
	retVal->y = y;
	retVal->radius = radius;
	return retVal;
}

int main() {

	struct drawable * d = make_drawable();
	d->draw(d);

	struct square * s = make_square(10,10,10);
	s->draw(s);

	struct circle * c = make_circle(10,10,10);
	c->draw(c);

}
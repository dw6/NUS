/*
	CS2104 Tutorial 4, Exercise 2 
	
	int **(*(*p)())[10]

	p is a pointer to a function that returns a pointer to an array (of size 10) of 
	pointers to pointers to integers.
	
	int **(*f())[]
	
	f is a function that returns a pointer to an array of pointers to pointers to integers

*/

int **(*f())[] {
	static int** arr[10];
	return &arr;
}

int ****g() {
	static int*** arr[10];
	return arr;
}

int main() {
	
	return 0;
}
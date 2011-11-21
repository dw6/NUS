#include <stdio.h>
#include <stdlib.h>

int Ackermann( int m, int n ) {
	if ( m == 0 ) return n+1;
	else if ( m > 0 && n == 0 ) return Ackermann ( m-1, 1 );
	else return Ackermann ( m-1, Ackermann(m ,n -1) );
}

typedef int (*k_int)(int) ;

int Ackermann_CPS( int m, int n ) {
	int Ackermann_aux( int m, int n, k_int k) {

		int k0( int ret0) {
			return k( ret0 );
		}

		int k1 (int ret1) {
			int k2 (int ret2) {
				return k(ret2);	
			}
			return Ackermann_aux( m-1, ret1 , k2 );
		}

		if ( m == 0 ) return k( n+ 1);
		else if ( m > 0 && n == 0 ) return Ackermann_aux ( m-1, 1, k0 ); // Two different 
		else return Ackermann_aux( m, n-1 , k1 );						 // continuations are used.
	}

	int identity( int x ) { return x; }
	return Ackermann_aux( m, n, identity);
}

int main() {
	printf("Result: %i\n", Ackermann(3,5));	
	printf("Result: %i\n", Ackermann(3,5));	
}
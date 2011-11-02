 void main() {
 	int x, y ;
    x = 1 ;
    if (x < 0) {
    	int x; x = 2 ; y = x ;
        while ( y > 0 ) {
			int z ;
			z = x*x ;
			y=y/2 ;
			if ( z >= y ) z = z - 1;
			else z = z + 1;
		}
		y = y>0 ? x+1 : x-1 ; 
	} 
	x=1; 
}
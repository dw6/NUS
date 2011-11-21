#include <stdio.h>

int factorial(int n) {
  int fact_aux(int n, int aux) {
    if ( n == 0 ) return aux ;
    else return fact_aux(n-1,n*aux) ;
  }
  return fact_aux(n,1) ;
}

int factorial1(int n) {
  int fact_aux(int aux) {
    if ( n == 0 ) return aux ;
    else return fact_aux((n--)*aux) ;
  }
  return fact_aux(1) ;
}

int factorial_tlo(int n) {
  int fact_aux(int n, int aux) {
  begin:
    if ( n == 0 ) return aux ;
    else {
      // We reuse the current arguments, and simply
      // jump to the beginning
      aux = n * aux ;
      n = n - 1 ;
      goto begin ;
    }
  }
  return fact_aux(n,1) ;
}

typedef int (*k_int)(int) ;

int factorial_cps(int n) {
  int fact_aux(int n, k_int k) {
    int k1(int r) {
      return k(n*r) ;
    }
    if ( n == 0 ) return k(1) ;
    else return fact_aux(n-1,k1) ;
  }
  int identity(int x) { return x ; }
  return fact_aux(n,identity) ;
}

int fib ( int n ) {
  int fib_aux (int n, k_int k) {
    int k1 ( int ret ) {
      int k2 ( int ret2 ) {
	return k(ret+ret2) ;
      }
      return fib_aux(n-2,k2) ;
    }
    if ( n <= 1 ) return k(1) ;
    else return fib_aux(n-1,k1) ;
  }
  int identity(int x) { return x ; }
  return fib_aux(n,identity) ;
}

char * hanoi(int n) {
  char * hanoi_aux(int n, int a, int b, int c) {
    if ( n > 0 ) {
      char * line = "%sMove a disk from pole %d to pole %d.\n%s" ;
      char * s1 = hanoi_aux(n-1,a,c,b) ;
      char * s2 = hanoi_aux(n-1,c,b,a) ;
      char * result = (char*)malloc(strlen(s1)+strlen(s2)+strlen(line)+1) ;
      sprintf(result,line,s1,a,b,s2) ;
      return result ;
    }
    return "" ;
  }
  return hanoi_aux(n,1,2,3) ;
}

typedef char * (*k_string)(char *) ;

char * hanoi_cps(int n) {
  char * hanoi_aux(int n, int a, int b, int c, k_string k) {
    char * k1 (char * s1) {
      char * k2 (char * s2) {
	char * line = "%sMove a disk from pole %d to pole %d.\n%s" ;
	char * result = (char*)malloc(strlen(s1)+strlen(s2)+strlen(line)+1) ;
	sprintf(result,line,s1,a,b,s2) ;
        return k(result) ;
      }
      return hanoi_aux(n-1,c,b,a,k2) ;
    }
    if ( n > 0 ) {
      return hanoi_aux(n-1,a,c,b,k1) ;
    } else {
      return k("") ;
    }
  }
  char * unit(char * x) { return x ; }
  return hanoi_aux(n,1,2,3,unit) ;
}

int main() {
  printf ( "Tail Recursive factorial: factorial(10) = %d\n", factorial(10) ) ;
  printf ( "Tail Recursive factorial: factorial1(10) = %d\n", factorial1(10) ) ;
  printf ( "CPS factorial: factorial_cps(10) = %d\n", factorial_cps(10) ) ;
  printf ( "Tail Call Optimized factorial: factorial_tlo(10) = %d\n", factorial_tlo(10) ) ;
  printf ( "CPS fib(5)= %d\n", fib(5)) ;
  printf ( "Towers of hanoi for %d disks:\n%s", 4, hanoi(4)) ;
  printf ( "Towers of hanoi for %d disks:\n%s", 4, hanoi_cps(4)) ;
}

// #include <stdio.h>

// void hanoi(char ** p, int n, int a, int b, int c) {
//    if ( n == 0 ) return ;
//    hanoi(p,n-1,a,c,b) ;
//    **p = '0'+(char)a ;
//    (*p) ++ ;
//    **p = ' ' ;
//    (*p) ++ ;
//    **p = 't' ;
//    (*p) ++ ;
//    **p = 'o' ;
//    (*p) ++ ;
//    **p = ' ' ;
//    (*p) ++ ;
//    **p = '0'+(char)b ;
//    (*p) ++ ;
//    **p = '\n' ;
//    (*p) ++ ;
//    hanoi(p,n-1,c,b,a) ;
// }
// int main() {
// char a[1000] ; 
// char *p = a ; 
// hanoi(&p,4,1,2,3) ; 
// *p = '\0' ; 
// printf(a) ; 
// } 

#include <stdio.h>

void hanoi(char ** p, int n, int a, int b, int c) {
   if ( n == 0 ) return ;
   hanoi(p,n-1,a,c,b) ;
   hanoi(p,n-1,c,b,a) ;
}

int main() {
char a[1000] ; 
char *p = a ; 
hanoi(&p,4,1,2,3) ; 
*p = '\0' ; 
printf(a) ; 

} 
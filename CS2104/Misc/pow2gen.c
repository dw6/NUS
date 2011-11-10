// Here's an implementation of the powers of 2 iterator in C. 
//
// Each iterator has 3 parts:
//
// * a structure with at least 2 elements: the 'next' pointer to function,      
//   that will point to the function that will compute the next element, 
//   and the 'continue_from' variable, that records some sort of state of the computation.
// 
// * a function to compute the next element, which takes the structure described above, 
//   and computes the next element. It also advances the 'continue_from'.
//
// * a constructor for the iterator; this one simply allocates memory and sets the fields 
//   of the structure accordingly.

// The code contains two iterators: the 'map', and the 'pow2', which computes the powers of two, and needs to rely on 'map'.

#include <stdio.h>
#include <stdlib.h>

// generic structure, the base of all generators
struct gen { 
  int (*next)() ;
  int continue_from ;
} ;

typedef int (*fptr)() ; 

// map
// structure for map
struct mapgen { 
  int (*next)() ;
  int continue_from ;
  fptr f ;
  struct gen * g ;
} ;

int mapf(struct mapgen *p) { // next function for map
  return p->f(p->g->next(p->g)) ;
}

struct gen * map(fptr f, struct gen * g) { // constructor for map iterator
  struct mapgen * p = (struct mapgen *)malloc(sizeof(struct mapgen));
  p->next = mapf;
  p->continue_from = 0;
  p->f = f;
  p->g = g;
  return (struct gen *)p ;
}
  

// powers of 2
struct pow2s {
  int (*next)() ;
  int continue_from ;
  struct gen * g ;
};

// anonymous lambda is translated into this
int times2(int x) { 
  return 2*x ;
}

// forward declaration of constructor
struct gen * pow2() ; 

// next function for iterator
int pow2next(struct pow2s * g){ 
  switch(g->continue_from) {
  case 0:
    g->continue_from = 1;
    return 1;
  case 1:
    g->g = map(times2,pow2()) ;
    g->continue_from = 2;
  case 2:
    return g->g->next(g->g) ;
  }
}    

// constructor for pow2
struct gen * pow2() { 
  struct pow2s * p = (struct pow2s *)malloc(sizeof(struct pow2s));
  p->next = pow2next;
  p->continue_from = 0;
  return (struct gen *)p;
}

// in main, create an iterator and print some of its elements.
int main() {
  int i ;
  struct gen * p = pow2() ;
  for(i=0;i<10;i++)
    printf("%d ",p->next(p)) ;
  printf("\n");
}

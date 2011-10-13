#include <stdio.h>
#include <stdlib.h>

// Equivalent C Code
// int pascal(int n, int k) {
// 	int x, y;
// 	if (n == 0 or n == k) return 1 ;
// 	x = pascal(n-1,k);
// 	y = pascal(n-1,k-1);
// 	return x + y;
// }


int eax, ebx, ecx, edx, edi, esi, esp, ebp, result;
unsigned char M[10000] ;


// Arguments are stored in ecx and edx
void exec() {
	esp = 10000 ;

	// Caller Prologue
	esp -= 4 ; *(int*)&M[esp] = eax ;			// push eax 
	esp -= 4 ; *(int*)&M[esp] = ecx ;			// push ecx 
	esp -= 4 ; *(int*)&M[esp] = edx ;			// push edx 
	esp -= 4 ; *(int*)&M[esp] = 5 ;				// push K 
	esp -= 4 ; *(int*)&M[esp] = 10 ;			// push N 
	eax = (int) && return_2 ;					// push the return address ...
	esp -= 4 ; *(int*)&M[esp] = eax ; 		    // here!
	goto pascal ;								// jump to our function


// Callee Prologue
pascal:
	esp -= 4 ; *(int*)&M[esp] = ebp ; 			// push (old) ebp
	ebp = esp ;									// new value of ebp
	esp -= 4 ; *(int*)&M[esp] = ebx ;			// push ebx
	esp -= 4 ; *(int*)&M[esp] = edi ;			// push edi
	esp -= 4 ; *(int*)&M[esp] = esi ;			// push esi
	esp -= 8 ; 		   					    	// space for locsvar x & y

 	ebx = *(int*)&M[ebp+8] ;
 	edi = *(int*)&M[ebp+12];

 	if (ebx == 0)   goto return_1;				// base case: return 1
 	if (ebx == edi) goto return_1;
 	goto skip ;									// jump to main body of function

skip:
	esp -= 4 ; *(int*)&M[esp] = eax ;			// push eax 
	esp -= 4 ; *(int*)&M[esp] = ecx ;			// push ecx 
	esp -= 4 ; *(int*)&M[esp] = edx ;			// push edx 

	ebx = *(int*)&M[ebp+8] ;					// store n into register
	ebx -= 1 ;									// store n-1 into register
	edi = *(int*)&M[ebp+12] ;					// store k into register

	esp -= 4 ; *(int*)&M[esp] = edi ;			// push k 
	esp -= 4 ; *(int*)&M[esp] = ebx ;			// push n-1
	eax = (int) && return_address1 ;			// push the return address ...
	esp -= 4 ; *(int*)&M[esp] = eax ; 			// here!
	goto pascal ;

return_address1:
	esp += 8 ;									// clear all arguments
	edx = *(int*)&M[esp] ; esp += 4 ;			// restore edx	
	ecx = *(int*)&M[esp] ; esp += 4 ;			// restore ecx	
	*(int*)&M[ebp-16] = eax ;					// x = return value
	eax = *(int*)&M[esp] ; esp += 4 ;			// restore eax
	esp -= 4 ; *(int*)&M[esp] = eax ;			// push eax 
	esp -= 4 ; *(int*)&M[esp] = ecx ;			// push ecx 
	esp -= 4 ; *(int*)&M[esp] = edx ;			// push edx 
	ebx = *(int*)&M[ebp+8]; 		        	// store n into register  
	ebx -= 1 ;									// store n-1	
	edi = *(int*)&M[ebp+12]; 		        	// store k into register  
	edi -= 1 ;									// store k-1
	esp -= 4 ; *(int*)&M[esp] = edi ;			// push k 
	esp -= 4 ; *(int*)&M[esp] = ebx ;			// push n-1					
	eax = (int) && return_address2 ;			// assign return address
	esp -= 4 ; *(int*)&M[esp] = eax ; 			// push in the return address
	goto pascal ;

return_address2:
	esp += 8 ;									// clear arguments
	edx = *(int*)&M[esp] ; esp += 4 ;			// restore edx	
	ecx = *(int*)&M[esp] ; esp += 4 ;			// restore ecx	
	*(int*)&M[ebp-20] = eax ;					// y = return value
	eax = *(int*)&M[esp] ; esp += 4 ;			// restore eax
	eax = *(int*)&M[ebp-16]; 		        	// load x 
	eax += *(int*)&M[ebp-20]; 		        	// add y; return value set now 

exit_pascal:							    	// callee epilogue
	esp += 8 ;						        	// clear local variables 
	esi = *(int*)&M[esp] ; esp += 4 ;			// restore 	
	edi = *(int*)&M[esp] ; esp += 4 ;			// callee	
	ebx = *(int*)&M[esp] ; esp += 4 ;			// registers	
	ebp = *(int*)&M[esp] ; esp += 4 ;
	esp += 4 ; 
	goto * *(void**)&M[esp-4];   				// return
	
return_1:
	eax = 1 ;
	goto exit_pascal;							// jump to the epilogue 

// Caller Epilogue (Cleanup code for caller)
return_2: 
	esp += 8 ; 									// clear arguments
	edx = *(int*)&M[esp] ; esp += 4 ; 			// pop edx 
	ecx = *(int*)&M[esp] ; esp += 4 ; 			// pop ecx
	result = eax;								// cheater!
	eax = *(int*)&M[esp] ; esp += 4 ; 			// pop eax 

}


int main() {
	exec() ;
	printf("result : %d\n", result);
}
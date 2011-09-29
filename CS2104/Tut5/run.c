#include <stdio.h>
int eax,ebx,ecx,edx,esi,edi,ebp,esp;
unsigned char M[10000];
void exec(void) {
    esp -= 4 ; *(int*)&M[esp] = 10 ; // push 10

    ecx = *(int*)&M[0] ; // save value of 0
    esp -= 4 ; *(int*)&M[esp] = 40 ; // push 40
    ecx = *(int*)&M[4] ; // save value of 1
    esp -= 4 ; *(int*)&M[esp] = 30 ; // push 30
    ecx = *(int*)&M[8] ; // save value of 2
    esp -= 4 ; *(int*)&M[esp] = 20 ; // push 20
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[400] = ecx ; // pop m
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[404] = ecx ; // pop i
Lwhile0:
    ecx = *(int*)&M[404] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push i
    esp -= 4 ; *(int*)&M[esp] = 3 ; // push 3
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    if ( eax < ecx ) goto Lwhilebody0;
    goto Lendwhile0;
Lwhilebody0:
    // start of if-then statement
    ecx = *(int*)&M[400] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push m
    ecx = *(int*)&M[404] ; // save value of i
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    if ( eax < ecx ) goto Lthen1; // if condition
    goto Lendif1;
Lthen1:
    ecx = *(int*)&M[404] ; // save value of i
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[400] = ecx ; // pop m
Lendif1:
    ecx = *(int*)&M[404] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push i
    esp -= 4 ; *(int*)&M[esp] = 1 ; // push 1
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[404] = ecx ; // pop i
    goto Lwhile0;
Lendwhile0:
{}}

int main() {
    esp = 10000 ;
    exec();
    printf("i=%d\n",*(int*)&M[404]);
    printf("m=%d\n",*(int*)&M[400]);
    printf("a=%d\n",*(int*)&M[0]);
    return 0;
}

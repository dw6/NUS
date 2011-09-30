#include <stdio.h>
int eax,ebx,ecx,edx,esi,edi,ebp,esp;
unsigned char M[10000];
void exec(void) {
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[0] = ecx ; // pop s
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[4] = ecx ; // pop i
Lwhile0:
    ecx = *(int*)&M[4] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push i
    esp -= 4 ; *(int*)&M[esp] = 10 ; // push 10
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    if ( eax < ecx ) goto Lwhilebody0;
    goto Lendwhile0;
Lwhilebody0:
    ecx = *(int*)&M[0] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push s
    ecx = *(int*)&M[4] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push i
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[0] = ecx ; // pop s
    ecx = *(int*)&M[4] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push i
    esp -= 4 ; *(int*)&M[esp] = 1 ; // push 1
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[4] = ecx ; // pop i
    goto Lwhile0;
Lendwhile0:
{}}

int main() {
    esp = 10000 ;
    exec();
    printf("i=%d\n",*(int*)&M[4]);
    printf("s=%d\n",*(int*)&M[0]);
    return 0;
}
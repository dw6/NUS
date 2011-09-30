#include <stdio.h>
int eax,ebx,ecx,edx,esi,edi,ebp,esp;
unsigned char M[10000];
void exec(void) {
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[0] = ecx ; // pop s
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[4] = ecx ; // pop w
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[8] = ecx ; // pop t
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[12] = ecx ; // pop i
Lwhile0:
    ecx = *(int*)&M[12] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push i
    esp -= 4 ; *(int*)&M[esp] = 10 ; // push 10
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    if ( eax < ecx ) goto Lwhilebody0;
    goto Lendwhile0;
Lwhilebody0:
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[16] = ecx ; // pop j
Lwhile1:
    ecx = *(int*)&M[16] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push j
    esp -= 4 ; *(int*)&M[esp] = 10 ; // push 10
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    if ( eax < ecx ) goto Lwhilebody1;
    goto Lendwhile1;
Lwhilebody1:
    esp -= 4 ; *(int*)&M[esp] = 0 ; // push 0
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[20] = ecx ; // pop k
Lwhile2:
    ecx = *(int*)&M[20] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push k
    esp -= 4 ; *(int*)&M[esp] = 10 ; // push 10
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    if ( eax < ecx ) goto Lwhilebody2;
    goto Lendwhile2;
Lwhilebody2:
    ecx = *(int*)&M[8] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push t
    ecx = *(int*)&M[20] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push k
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[8] = ecx ; // pop t
    ecx = *(int*)&M[20] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push k
    esp -= 4 ; *(int*)&M[esp] = 1 ; // push 1
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[20] = ecx ; // pop k
    goto Lwhile2;
Lendwhile2:
    ecx = *(int*)&M[4] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push w
    ecx = *(int*)&M[16] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push j
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[4] = ecx ; // pop w
    ecx = *(int*)&M[16] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push j
    esp -= 4 ; *(int*)&M[esp] = 1 ; // push 1
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[16] = ecx ; // pop j
    goto Lwhile1;
Lendwhile1:
    ecx = *(int*)&M[0] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push s
    ecx = *(int*)&M[12] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push i
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[0] = ecx ; // pop s
    ecx = *(int*)&M[12] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push i
    esp -= 4 ; *(int*)&M[esp] = 1 ; // push 1
    ecx = *(int*)&M[esp] ; esp += 4 ;
    eax = *(int*)&M[esp] ; esp += 4 ;
    eax += ecx ;
    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of +
    ecx = *(int*)&M[esp] ; esp += 4 ;
    *(int*)&M[12] = ecx ; // pop i
    goto Lwhile0;
Lendwhile0:
{}}

int main() {
    esp = 10000 ;
    exec();
    printf("k=%d\n",*(int*)&M[20]);
    printf("j=%d\n",*(int*)&M[16]);
    printf("i=%d\n",*(int*)&M[12]);
    printf("t=%d\n",*(int*)&M[8]);
    printf("w=%d\n",*(int*)&M[4]);
    printf("s=%d\n",*(int*)&M[0]);
    return 0;
}
class Exception1 extends RuntimeException {
    public Exception1(String s) {
super(s) ; }
}
class Exception2 extends RuntimeException {
    public Exception2(String s) {
super(s) ; }
}
class Exception3 extends RuntimeException {
    public Exception3(String s) {
super(s) ; }
}
public class ExceptionExample {
    public static void main ( String [] args ) {
try {first(Integer.parseInt(args[0]),Integer.parseInt(args[1])) ;
     System.out.println("This might not get printed") ;
} catch (Exception1 e) {
     System.out.println(e) ;
} catch (Exception2 e) {
        System.out.println(e) ;
}
}
public static void first (int a, int b) {
}
System.out.println("In first: "+
     "this might not always get printed") ;
try {if ( a == 1 )
       throw new Exception1("Exception 1 from first") ;
     second(b) ;
} catch (Exception3 e) {
     System.out.println(e) ;
} finally {
}    System.out.println("In first");
}
public static void second (int b) {
   if ( b == 1 )
}
+x+","+y+") and radius "+factor*radius) ;
    throw new Exception2("Exception 2 from second") ;
if ( b == 2 )
    throw new Exception3("Exception 3 from second") ;
System.out.println("In second") ;
}
}


#include <stdio.h>
#include <setjmp.h>

struct Exception {
    enum{
        NOEXCEPTION,E1,E2,E3
    }type;
    char* msg;
} ex;

jmp_buf stack[100];
int sp = -1;

jmp_buf* push(){
    return &stack[++sp];
}

jmp_buf* pop(){
    return &stack[sp--];
}

void second(int b){
    if(b==1){
        ex.type=E2;
        ex.msg = "Exception 2 from second";
        longjmp(*pop(),1);
    }
    if(b==2){
        ex.type=E3;
        ex.msg ="Exception 3 from second";
        longjmp(*pop(),1);
    }
    printf("In second");
}

void first(int a, int b){
    if(!setjmp(*push())){
        if(a==1){
            ex.type=E1;
            ex.msg = "Exception 1 from first";
            longjmp(*pop(),1);
        }
        second(b);
        pop();
    } else{
        switch(ex.type){
            case E3:
                printf(ex.msg);
                ex.type=NOEXCEPTION;
                goto finally;
            default:
            finally:
                printf("In first");
            if(ex.type != NOEXCEPTION)
                longjmp(*pop(),1);
        }
    }
    printf("In first: this might not always get printed");
}

int main(int argc,char** args){
    if(!setjmp(*push())){
        first(0,2);
        printf("This might not get printed");
        pop();
    } else{
        switch(ex.type){
            case E1:
            case E2:
                printf(ex.msg);
                ex.type=NOEXCEPTION;
                goto finally;
            default:
            finally:
                if(ex.type!=NOEXCEPTION){
                    longjmp(*pop(),1);
                }
        }
    }
}
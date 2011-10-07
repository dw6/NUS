// Benjamin Tan Wei Hao
// U077129N
// Problem Set 5, Exercsise 3


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define EPS	0.000000000000001

double g_x(double x) {
	return x*x-1.0;
}
double solve(double (*fp)(double), double x1, double x2) 
{
	if (fabs(x1-x2) < EPS) 
	{
		return (x1+x2)/2 ;
	} 
	else if (fp(x1) * fp((x1+x2)/2) <= 0) 
	{
        return solve(fp,x1,(x1+x2)/2) ;	
	} 
	else if (fp(x2) * fp((x1+x2)/2) <= 0) 
	{
		return solve(fp,(x1+x2)/2,x2) ;
	}
}

int main() {
	printf("%f\n",solve(&sin, 1.0, 4.0));
	printf("%f\n",solve(&cos, 1.0, 4.0));
	printf("%f\n",solve(&tan, 1.0, 4.0));
	printf("%f\n",solve(&g_x, 1.0, 4.0));
	return 0;
}
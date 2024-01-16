#include <stdio.h>

int fib(int n);
int main() {
int n = 0;
printf("Fibonacci of: ");
scanf("%d", (&n));
printf("Result: %d\n", fib(n));
printf("Press any key to quit.");
getchar();
return 0;
};

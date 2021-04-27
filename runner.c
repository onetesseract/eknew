#include <stdio.h>

double neonmain();

void printd(double d) {
    printf("%f\n", d);
}
void printi(int i) {
    printf("%i\n", i);
}
void printip(int* n) {
    printf("%i\n", *n);
}
int main() {
    neonmain();
}
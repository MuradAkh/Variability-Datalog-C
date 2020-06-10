#include<stddef.h>
#include<stdio.h>



int main(){
    #ifdef GOODBYE
    
    int *x;
    x = malloc(27);
    #endif
    
    #ifdef GOODBYE
    #ifdef HELLO
    int* y;
    y = x;

    #endif
    #endif

    return 0;
}

// https://github.com/markus-kusano/pdgpdgp/blob/master/ContextInsenAA/test/malloc01/main.c
#include <stdlib.h>
#include <stdio.h>



int main(int argc, char *argv[]) {
  char *x = (char *) malloc(sizeof(char));
  //*x = 'a';
  char *y;
  char a = hello(0);
  *y = a;
  y[1] = (char *) x;
  char *z = y[1];
  int f = 1;
  int g = (int) f;

  int i;

  while (1 > 0)
  {
    g = f;
    f = g;
  }
  
  

  //printf("x: %c, y: %c, z: %c\n", *x, *y, *z);

  return 0;
}
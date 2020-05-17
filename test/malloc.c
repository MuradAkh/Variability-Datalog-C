// https://github.com/markus-kusano/pdgpdgp/blob/master/ContextInsenAA/test/malloc01/main.c
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  char *x;
  x = (char*) malloc(sizeof(char));
  //*x = 'a';
  char *y;
  y = x;
  char *z = y;
  int f = 1;
  int g = f;

  //printf("x: %c, y: %c, z: %c\n", *x, *y, *z);

  return 0;
}
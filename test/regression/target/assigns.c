// https://github.com/markus-kusano/pdgpdgp/blob/master/ContextInsenAA/test/malloc01/main.c
#include <stdlib.h>
#include <stdio.h>



int main(int argc, char *argv[]) {
  //*x = 'a';
  char *y;
  int x;
  char a = *y;
  *y = a;
  y[1] = (char *) x;
  char *z = y[1];
  int f = 1;
  int g = (int) f;
  


  return 0;
}
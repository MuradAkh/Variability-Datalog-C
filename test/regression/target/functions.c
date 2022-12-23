#include <stdlib.h>

void *ap();
void *azp;

void ***xp(){

    char *x = malloc(sizeof(char));
      #if !defined LINUX 
    return NULL;
    #endif
}

int main(int argc, char *argv[]) {
  char *x = malloc(sizeof(char));
  char *a = (char*) malloc(sizeof(char));
  char *b = (char*) malloc(32);
  char *c;
  int i = xp();
  c = (char*) malloc(32);
  return 0;
}
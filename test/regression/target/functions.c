#include <stdlib.h>

int xp(){
    char *x = malloc(sizeof(char));
    return 1;
}

int main(int argc, char *argv[]) {
  char *x = malloc(sizeof(char));
  char *a = (char*) malloc(sizeof(char));
  char *b = (char*) malloc(32);
  char *c;
  xp();
  c = (char*) malloc(32);
  return 0;
}
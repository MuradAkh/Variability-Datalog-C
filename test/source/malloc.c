// https://github.com/markus-kusano/pdgpdgp/blob/master/ContextInsenAA/test/malloc01/main.c



int main(int argc, char *argv[]) {
  int a, b;
  #ifdef go
goto lab;
  #endif

  b = 1;
lab:
  a = 2;



  return 0;
}
void foo() {
  float f = 4.5;
  bool b = false;

  for (f = 1; b != true; f++)
    b = false;

  for (f = 1; b != true; f++) {
    int x = 1;
    b = false;
    i++;
  }
}
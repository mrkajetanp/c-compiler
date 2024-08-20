int main(void) {
  int a = 0;
  int b = 0;

  while (a < 5)
    a = a + 1;

  do {
    a = a - 1;
    if (a == 1)
      break;
  } while (a > 0);

  for (a = 0 ; a < 5; a = a + 1) {
    b = b + 1;
    continue;
  }

  for (int c = 0 ; c < 5; c = c + 1)
    b = b + 1;

  return b;
}

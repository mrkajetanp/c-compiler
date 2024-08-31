int sum(int first, int second);

int big_sum(int a, int b, int c, int d, int e, int f, int g, int h, int i) {
  return a + b + c + d + e + f + g + h + i;
}

int sum(int a, int b) {
  return a + b;
}

int test(void) {
  int some = 8;
  return big_sum(0, 1, 2, 3, 4, 5, 6, 7, some);
}

int main(void) {
  int one = 5;
  int two = 6;
  int result = sum(one, two);
  return result;
}

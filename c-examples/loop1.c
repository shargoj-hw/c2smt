int main() {
  float x = 1.0;
  while (x != 55) {
    int y = x*x;
    x += 1.2 * y;
  }
  return x;
}

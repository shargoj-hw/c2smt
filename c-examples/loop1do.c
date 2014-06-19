int main() {
  float x = 1.0f;
  do {
    int y = x*x;
    x += 1.2 * y;
  } while (x != 55);
  return x;
}

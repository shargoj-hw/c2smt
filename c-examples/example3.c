float example3(float x) {
  float y;
  if (x > 0) {
    y = x + x;
    y = y * x;
  }
  return y;
}

int main() {
  float c = 2;
  float d = example3(c);
  return 0;
}

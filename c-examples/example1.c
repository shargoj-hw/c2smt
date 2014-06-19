float example1(float x) {
  float y;
  if (x > 0)
    y = x + x;
  else
    y = x;
  return y;
}

int main() {
  float c = 2;
  float d = example1(c);
  return 0;
}

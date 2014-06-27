float example2(float x) {
  float y;
  if (x > 0) {
    y = x + x;
    y = y * x;
  }
  else
    y = x;
  return y;
}

int main() {
  float c = 2;
  float d = example2(c);
  return 0;
}

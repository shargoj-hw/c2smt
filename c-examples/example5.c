float example5(float x) {
  float y;
  if (x > 0)
    y = x + x;
  else
    y = x;
  return y;
}

int main() {
  float c = 3000.5;
  float d;
  d = example5(c);
  return 0;
}

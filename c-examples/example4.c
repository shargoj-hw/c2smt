float example4(float x) {
  float y, z;
  if (x > 0) {
    y = x + x;
    y = y * x;
  }
  z = y + x;
  z = z + x; 
  return z;
}

int main() {
  float c = 2;
  float d = example4(c);
  return 0;
}

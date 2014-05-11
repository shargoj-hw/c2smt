module CExamples where

import Utils

  -- Loopy programs:
loop1 = parseFromString $ "\
\ int main() { \
\   float x = 1.0f;\
\   while (x != 55) {\
\     int y = x*x;\
\     x += 1.2 * y;\
\   }\
\   return x;\
\ }"

loop2 = parseFromString $ "\
\ int main() {\
\   float x = 1.0f;\
\   while (true)\
\     x += 1.2 * x;\
\   return x;\
\ }"

multiDecl = parseFromString $ "\
\ int main () {\
\   float x;\
\   x = 3 * 9;\
\   float y;\
\   y += x + 3;\
\   return 0;\
\ }"

complicatedDecl = parseFromString $ "\
\ int main () {\
\   float a, b=2.0, *c, *d = 4;\
\   static float n = 3, m;\
\   return 0;\
\ }";

ifreturn1 = parseFromString $ "\
\ int main () {\
\   float a;\
\   if (true) {} \
\   a = 3;\
\   return a;\
\}"

ifreturn2 = parseFromString $ "\
\ int main () {\
\   float a;\
\   if (true) {return 0;} \
\   a = 3;\
\   return a;\
\}"

ifreturn3 = parseFromString $ "\
\ int main () {\
\   float a;\
\   if (true) {a += 2;} \
\   a = 3;\
\   return a;\
\}"

ifreturn4 = parseFromString $ "\
\ int main () {\
\   float a;\
\   if (true) {a += 2;} \
\   else return 4;\
\   a = 3;\
\   return a;\
\}"

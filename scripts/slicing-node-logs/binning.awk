{ p = log($0 / sz)/log(10); if (p < 0) p = 0; x = sz * 10 ^ int(p); print int($0/x)*x; }

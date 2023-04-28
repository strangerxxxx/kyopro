#include <bits/stdc++.h>
using namespace std;
long long pow_ll(long long x, long long y)
{
  long long res = 1;
  while (y)
  {
    if (y & 1)
    {
      res *= x;
    }
    x *= x;
    y >>= 1;
  }
  return res;
}
long long mod_pow(long long x, long long y, long long mod = 998244353)
{
  long long res = 1;
  while (y)
  {
    if (y & 1)
    {
      res *= x;
      res %= mod;
    }
    x *= x;
    x %= mod;
    y >>= 1;
  }
  return res;
}
long long mod_inv(long long x, long long mod = 998244353)
{
  return mod_pow(x, mod - 2, mod);
}
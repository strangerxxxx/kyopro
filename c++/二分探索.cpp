#ifdef STRANGERXXX
#define _GLIBCXX_DEBUG
#endif
#include <bits/stdc++.h>
using namespace std;
typedef long long ll;
bool f(ll mid)
{
  return true;
}
int main()
{
  ll ok = 0;
  ll ng = 1E18;
  ll mid;
  while (abs(ok - ng) > 1)
  {
    mid = (ok + ng) / 2;
    if (f(mid))
    {
      ok = mid;
    }
    else
    {
      ng = mid;
    }
  }
  print(ok);
}

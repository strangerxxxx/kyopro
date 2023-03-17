#include <bits/stdc++.h>
using namespace std;
typedef long long ll;
int main()
{
  ll ok = 0;
  ll ng = 1E18;
  ll mid;
  auto check = [&](ll mid) -> bool
  {
    return true;
  };
  while (abs(ok - ng) > 1)
  {
    mid = (ok + ng) / 2;
    if (check(mid))
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
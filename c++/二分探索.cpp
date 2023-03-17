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
int main_double()
{
  double ok = 0;
  double ng = 1E18;
  double mid;
  const double eps = 1E-12;
  auto check = [&](double mid) -> bool
  {
    return true;
  };
  while (abs(ok - ng) > eps)
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
  cout << fixed << setprecision(15) << ok << "\n";
}
// https://atcoder.jp/contests/abc242/tasks/abc242_g
#include <bits/stdc++.h>
using namespace std;
typedef long long ll;
typedef vector<ll> VI;
typedef vector<VI> VVI;
typedef vector<VVI> VVVI;
#define SORT(V) sort((V).begin(), (V).end())
#define RSORT(V) sort((V).rbegin(), (V).rend())
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
#define REPA(i, I) for (const auto &i : I)
int main()
{
  ll n, q;
  cin >> n;
  VI a(n);
  REP(i, n)
  {
    cin >> a[i];
  }
  cin >> q;
  VI ans(q);
  ll range = n / sqrt(q) + 1;
  // ll range = sqrt(3) * n / sqrt(q << 1) + 1;
  ll qn = n / range + 1;
  VVVI queries(qn, VVI());
  ll l, r;
  REP(i, q)
  {
    cin >> l >> r;
    l--;
    r--;
    queries[l / range].push_back(VI({r, l, i}));
  }
  ll index, left = 0, right = 0, nowans = 0;
  VI color(n + 1, 0);
  auto add = [&](ll i) -> void
  {
    if (color[a[i]] % 2)
    {
      nowans++;
    }
    color[a[i]]++;
  };
  auto rem = [&](ll i) -> void
  {
    color[a[i]]--;
    if (color[a[i]] % 2)
    {
      nowans--;
    }
  };
  REP(i, qn)
  {
    if (i & 1)
    {
      RSORT(queries[i]);
    }
    else
    {
      SORT(queries[i]);
    }
    REPA(j, queries[i])
    {
      r = j[0];
      l = j[1];
      index = j[2];
      while (right <= r)
      {
        add(right);
        right++;
      }
      while (left > l)
      {
        left--;
        add(left);
      }
      while (right - 1 > r)
      {
        right--;
        rem(right);
      }
      while (left < l)
      {
        rem(left);
        left++;
      }
      ans[index] = nowans;
    }
  }
  REP(i, q)
  {
    cout << ans[i] << endl;
  }
}

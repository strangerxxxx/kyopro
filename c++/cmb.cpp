#include <bits/stdc++.h>
using namespace std;
typedef long long ll;
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
#define REP3(i, m, n) for (ll i = (m); i < (ll)(n); i++)
ll pow_mod(ll x, ll n, ll mod = 998244353)
{
  ll res = 1;
  while (n)
  {
    if (n & 1)
    {
      res *= x;
      res %= mod;
    }
    x *= x;
    x %= mod;
    n >>= 1;
  }
  return res;
}
ll cmb(ll n, ll r, ll mod = 998244353)
{
  if (n < 0 || n < r)
  {
    return 0;
  }
  if (r > n - r)
  {
    return cmb(n, n - r, mod);
  }
  ll c = 1, d = 1;
  REP(i, r)
  {
    c *= n - i;
    d *= r - i;
    c %= mod;
    d %= mod;
  }
  return c * pow_mod(d, mod - 2, mod) % mod;
}
ll perm(ll n, ll r = -1, ll mod = 998244353)
{
  if (r == -1)
  {
    r = n;
  }
  if (n < 0 || n < r)
  {
    return 0;
  }
  ll res = 1;
  REP(_, r)
  {
    res *= n--;
    res %= mod;
  }
  return res;
}
ll hom(ll n, ll r, ll mod = 998244353)
{
  return cmb(n + r - 1, r, mod);
}
struct Combination
{
  ll n_max, mod;
  vector<ll> modinv, fac, facinv;
  Combination(ll maxn, ll mod_ = 998244353)
  {
    n_max = maxn;
    mod = mod_;
    modinv = make_modinv_list(n_max);
    tie(fac, facinv) = make_factorial_list(n_max);
  }
  vector<ll> make_modinv_list(ll n)
  {
    vector<ll> res(n + 1);
    res[0] = 0;
    res[1] = 1;
    REP3(i, 2, n + 1)
    {
      res[i] = mod - res[mod % i] * (mod / i) % mod;
    }
    return res;
  }
  pair<vector<ll>, vector<ll>> make_factorial_list(ll n)
  {
    pair<vector<ll>, vector<ll>> res;
    res.first = vector<ll>(n + 1, 1);
    res.second = vector<ll>(n + 1, 1);
    REP3(i, 1, n + 1)
    {
      res.first[i] = res.first[i - 1] * i % mod;
      res.second[i] = res.second[i - 1] * modinv[i] % mod;
    }
    return res;
  }
  ll calc(ll n, ll r)
  {
    if (n < 0 || n < r)
    {
      return 0;
    }
    assert(r <= n_max);
    return fac[n] * facinv[r] % mod * facinv[n - r] % mod;
  }
};
int main()
{
  // ll n;
  // cin >> n;
  // ll m;
  // cin >> m;
  Combination cmb(100LL);
  cout << cmb.calc(100, 30) << endl;
}

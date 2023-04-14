#include <bits/stdc++.h>
using namespace std;
typedef long long ll;
typedef vector<ll> VI;
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
#define REPA(i, I) for (const auto &i : I)
random_device seed_gen;
mt19937_64 mt(seed_gen());
const ll md = (1LL << 61) - 1;
const ll rn = mt() % (md - 4) + 2;
const ll MASK30 = (1UL << 30) - 1;
const ll MASK31 = (1UL << 31) - 1;
ll CalcMod(ll x)
{
  ll xu = x >> 61;
  ll xd = x & md;
  ll res = xu + xd;
  if (res >= md)
    res -= md;
  return res;
}
ll Mul(ll a, ll b)
{
  ll au = a >> 31;
  ll ad = a & MASK31;
  ll bu = b >> 31;
  ll bd = b & MASK31;
  ll mid = ad * bu + au * bd;
  ll midu = mid >> 30;
  ll midd = mid & MASK30;
  return CalcMod(au * bu * 2 + midu + (midd << 31) + ad * bd);
}
ll modpow(ll a, ll n, ll mod)
{
  ll res = 1;
  while (n)
  {
    if (n & 1)
      res = Mul(res, a);
    a = Mul(a, a);
    n >>= 1;
  }
  return res;
}
VI hash_list(string s, ll l)
{
  ll n = s.size();
  VI res(n - l + 1);
  ll x = 0;
  REP(i, l)
  {
    x = Mul(x, rn);
    x += s[i];
  }
  x %= md;
  res[0] = x;
  ll denom = modpow(rn, l - 1, md);
  REP(i, n - l)
  {
    x -= Mul(s[i], denom);
    x = Mul(x, rn);
    x += s[i + l];
    x %= md;
    res[i + 1] = x;
  }
  return res;
}
ll get_hash(string s)
{
  ll res = 0;
  REP(i, s.size())
  {
    res = Mul(res, rn);
    res += s[i];
    res %= md;
  }
  return res;
}
#define REP3(i, m, n) for (ll i = (m); i < (ll)(n); i++)
void PRINTVI(VI V)
{
  if (V.size())
  {
    cout << V[0];
    REP3(i, 1, V.size())
    {
      cout << " " << V[i];
    }
  }
  cout << endl;
}
int main()
{
  string s, t;
  cin >> s >> t;
  if (s.size() < t.size())
  {
    return 0;
  }
  ll x = get_hash(t);
  VI y = hash_list(s, t.size());
  REP(i, y.size())
  {
    if (y[i] == x)
    {
      cout << i << endl;
    }
  }
}

#pragma region header
#ifdef STRANGERXXX
#define _GLIBCXX_DEBUG
#endif
/*Pythonで提出してない？*/
// #pragma GCC target("avx2")
// #pragma GCC optimize("O3")
// #pragma GCC optimize("unroll-loops")
// #pragma GCC target("sse,sse2,sse3,ssse3,sse4,popcnt,abm,mmx,avx,tune=native")
#include <bits/stdc++.h>
using namespace std;
#include <atcoder/all>
using namespace atcoder;
using mint = modint998244353;
// using mint = modint1000000007;
// #include <boost/multiprecision/cpp_int.hpp>
// using namespace boost::multiprecision;
typedef long long ll;
const ll MOD = 998244353;
typedef unsigned int uint;
typedef unsigned long long ull;
clock_t STARTTIME = clock();
#define TIME() static_cast<double>(clock() - STARTTIME) / CLOCKS_PER_SEC * 1.0
const int INF32 = INT_MAX;
const int IINF32 = INT_MIN;
const uint UINF32 = UINT_MAX;
const long long INF64 = LLONG_MAX;
const long long IINF64 = LLONG_MIN;
const unsigned long long UINF64 = ULLONG_MAX;
const double EPS = 1e-10;
const double PI = 3.141592653589793238;
const int dx[4] = {1, 0, -1, 0};
const int dy[4] = {0, 1, 0, -1};
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
#define REP3(i, m, n) for (ll i = (m); i < (ll)(n); i++)
#define REP4(i, m, n, d) for (ll i = (m); i < (ll)(n); i = i + (ll)(d))
#define REPR(i, n) for (ll i = (ll)(n)-1; i >= 0; i--)
#define REP3R(i, m, n) for (ll i = (ll)(n)-1; i >= (ll)(m); i--)
#define REP4R(i, m, n, d) for (ll i = (m); i >= (ll)(n); i = i + (ll)(d))
#define REPA(i, I) for (const auto &i : I)
#define REPIJ(i, j, n)             \
  for (ll i = 0; i < (ll)(n); i++) \
    for (ll j = i + 1; j < (ll)(n); j++)
#define REPIN(a)     \
  for (auto &&i : a) \
  {                  \
    cin >> i;        \
  }
#define LEN(x) x.size()
typedef pair<ll, ll> PII;
typedef vector<ll> VI;
typedef vector<VI> VVI;
typedef vector<VVI> VVVI;
typedef vector<string> VS;
typedef vector<VS> VVS;
typedef vector<PII> VP;
typedef set<ll> SI;
typedef multiset<ll> MSI;
typedef map<ll, ll> MI;
typedef unordered_set<ll> USI;
typedef unordered_map<ll, ll> UMI;
typedef priority_queue<ll> PQ;
typedef priority_queue<ll, VI, greater<ll>> RPQ;
typedef deque<ll> DQ;
random_device seed_gen;
mt19937 mt(seed_gen());
mt19937_64 mt64(seed_gen());
#define RANDINT() mt64()
#define RAND() (double)mt64() / UINF64
#define pb push_back
#define eb emplace_back
#define mp make_pair
#define endl '\n'
#define SUM(V) accumulate((V).begin(), (V).end(), 0LL)
#define ALL(a) (a).begin(), (a).end()
#define SORT(V) sort((V).begin(), (V).end())
#define RSORT(V) sort((V).rbegin(), (V).rend())
#define REVERSE(V) reverse((V).begin(), (V).end())
#define mod(a, b) ((ll)a % (ll)b + (ll)b) % (ll)b
#define ctoll(c) (ll) c - 48
#define MAXVI(V) *max_element((V).begin(), (V).end())
#define MINVI(V) *min_element((V).begin(), (V).end())
#define UB(V, x) upper_bound((V).begin(), (V).end(), x)
#define LB(V, x) lower_bound((V).begin(), (V).end(), x)
#define bisect_left(V, x) lower_bound((V).begin(), (V).end(), x) - (V).begin()
#define bisect_right(V, x) upper_bound((V).begin(), (V).end(), x + 1) - (V).begin()
#define BS(V, x) binary_search((V).begin(), (V).end(), x)
#define MEMSET(v, h) memset((v), h, sizeof(v))
#define MEMCPY(v, h) memcpy((v), h, sizeof(v))
#define pcnt __builtin_popcount
template <class T>
using V = vector<T>;
template <class T>
bool chmax(T &a, const T &b)
{
  if (a < b)
  {
    a = b;
    return 1;
  }
  return 0;
}
template <class T>
bool chmin(T &a, const T &b)
{
  if (b < a)
  {
    a = b;
    return 1;
  }
  return 0;
}
string join(const vector<string> &v, const char *delim = 0)
{
  string s;
  if (!v.empty())
  {
    s += v[0];
    for (decltype(v.size()) i = 1, c = v.size(); i < c; ++i)
    {
      if (delim)
        s += delim;
      s += v[i];
    }
  }
  return s;
}
void print() { cout << "\n"; }
template <class T, class... A>
void print(const T &first, const A &...rest)
{
  if (sizeof...(rest))
  {
    cout << first << " ";
    print(rest...);
    return;
  }
  cout << first << "\n";
}
template <class... A>
void print(const A &...rest) { print(rest...); }
#define PRINTD(d) cout << fixed << setprecision(15) << d << "\n"
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
  cout << "\n";
}
void PRINTVVI(VVI V)
{
  REPA(v, V)
  {
    PRINTVI(v);
  }
}
void PRINTVS(VS V)
{
  print(join(V, " "));
}
void PRINTVVS(VVS V)
{
  REPA(v, V)
  {
    PRINTVS(v);
  }
}
void PRINTPII(PII P)
{
  cout << P.first << " " << P.second << "\n";
}
class range
{
private:
  struct I
  {
    int x;
    int operator*() { return x; }
    bool operator!=(I &lhs) { return x < lhs.x; }
    void operator++() { ++x; }
  };
  I i, n;

public:
  range(int n) : i({0}), n({n}) {}
  range(int i, int n) : i({i}), n({n}) {}
  I &begin() { return i; }
  I &end() { return n; }
};
ll pow_ll(ll x, ll y)
{
  ll res = 1;
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
struct custom_hash
{
  static uint64_t splitmix64(uint64_t x)
  {
    // http://xorshift.di.unimi.it/splitmix64.c
    x += 0x9e3779b97f4a7c15;
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
    x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
    return x ^ (x >> 31);
  }

  size_t operator()(uint64_t x) const
  {
    static const uint64_t FIXED_RANDOM = chrono::steady_clock::now().time_since_epoch().count();
    return splitmix64(x + FIXED_RANDOM);
  }
};
ll divceil(ll a, ll b)
{
  return (a + (b - 1)) / b;
}
PII divmod(ll a, ll b)
{
  ll m = mod(a, b);
  return make_pair((a - m) / b, m);
}
std::uint32_t XORShiftRandom()
{
  static uint32_t y = 2463534241;
  y = y ^ (y << 13);
  y = y ^ (y >> 17);
  return y = y ^ (y << 5);
}
#pragma endregion
int main()
{
  cin.tie(nullptr);
  ios_base::sync_with_stdio(false);
  ll n;
  cin >> n;
  ll m;
  cin >> m;
  VI a(n);
  REP(i, n) { cin >> a[i]; }
  ll ans = 0;
  if (true)
  {
    print("Yes");
  }
  else
  {
    print("No");
  }
}

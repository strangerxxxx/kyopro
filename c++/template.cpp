#ifdef STRANGERXXX
#define _GLIBCXX_DEBUG
#endif
/*Pythonで提出してない？*/
#include <bits/stdc++.h>
using namespace std;
#include <atcoder/all>
using namespace atcoder;
using mint = modint998244353;
// using mint = modint1000000007;
typedef long long ll;
const ll MOD = 998244353;
typedef unsigned int uint;
typedef unsigned long long ull;
clock_t STARTTIME = clock();
#define TIME() static_cast<double>(clock() - STARTTIME) / CLOCKS_PER_SEC * 1.0
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
mt19937_64 mt(seed_gen());
#define RAND() (double)mt() / UINF64
#define RANDINT(l, r) mt() / (UINF64 / ((ll)(r) + 1 - (ll)(l))) + (ll)(l)
#define pb push_back
#define mp make_pair
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
const int INF32 = INT_MAX;
const int IINF32 = INT_MIN;
const uint UINF32 = UINT_MAX;
const ll INF64 = LLONG_MAX;
const ll IINF64 = LLONG_MIN;
const ull UINF64 = ULLONG_MAX;
const double EPS = 1e-10;
const double PI = 3.141592653589793238;
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
void print() { cout << endl; }
template <class T, class... A>
void print(const T &first, const A &...rest)
{
  if (sizeof...(rest))
  {
    cout << first << " ";
    print(rest...);
    return;
  }
  cout << first << endl;
}
template <class... A>
void print(const A &...rest) { print(rest...); }
#define PRINTD(d) printf("%.16lf\n", (double)d)
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
  cout << P.first << " " << P.second << endl;
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
ll divceil(ll a, ll b)
{
  return (a + (b - 1)) / b;
}
PII divmod(ll a, ll b)
{
  ll m = mod(a, b);
  return make_pair((a - m) / b, m);
}
int main()
{
  ll n;
  cin >> n;
  unordered_map<ll, ll> c;
  ll m = 0;
  PII ans;
  ll a;
  REP(i, n)
  {
    cin >> a;
    if (c.count(a))
    {
      c[a] += 1;
    }
    else
    {
      c[a] = 1;
    }
    if (c[a] > m)
    {
      m = c[a];
      ans.first = a;
      ans.second = c[a];
    }
  }
  print(ans);
  if (true)
  {
    print("Yes");
  }
  else
  {
    print("No");
  }
}

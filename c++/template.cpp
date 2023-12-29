#pragma region header
#ifdef STRANGERXXX
#define _GLIBCXX_DEBUG
#endif
/*Pythonで提出してない？*/
// #pragma GCC target("avx2")
#pragma GCC optimize("O3")
#pragma GCC optimize("unroll-loops")
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
// const double PI = numbers::pi;
const int dx[4] = {1, 0, -1, 0};
const int dy[4] = {0, 1, 0, -1};
#define REP(i, end) for (ll i : views::iota(0, end))
#define REP3(i, begin, end) for (ll i : views::iota(begin, end))
#define REP4(i, m, n, d) for (ll i = (m); i < (ll)(n); i = i + (ll)(d))
#define REPR(i, end) for (ll i : views::iota(0, end) | views::reverse)
#define REP3R(i, begin, end) \
    for (ll i : views::iota(begin, end) | views::reverse)
#define REP4R(i, m, n, d) for (ll i = (m); i > (ll)(n); i = i + (ll)(d))
#define REPA(i, I) for (const auto &i : I)
#define REPAR(i, I) for (const auto &i : I | views::reverse)
#define REPB(i, I) for (auto &&i : I)
#define REPIJ(i, j, n)               \
    for (ll i = 0; i < (ll)(n); i++) \
        for (ll j = i + 1; j < (ll)(n); j++)
#define REPIN(a)         \
    for (auto &&i : a) { \
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
// #define RANDINT() mt64()
// #define RAND() (double)mt64() / UINF64
#define pb push_back
#define eb emplace_back
#define mp make_pair
#define mt make_tuple
#define endl '\n'
#define ALL(V) (V).begin(), (V).end()
#define SUM(V) reduce((V).begin(), (V).end(), 0LL)
#define SORT(V) ranges::sort(V)
#define RSORT(V) ranges::sort(V, ranges::greater());
#define REVERSE(V) ranges::reverse(V)
#define MINMAX(a, b) ranges::minmax(a, b)
#define mod(a, b) ((ll)a % (ll)b + (ll)b) % (ll)b
#define ctoll(c) (ll) c - 48
#define MAXVI(V) *ranges::max_element(V)
#define MINVI(V) *ranges::min_element(V)
#define UB(V, x) ranges::upper_bound(V, x)
#define LB(V, x) ranges::lower_bound(V, x)
#define bisect_left(V, x) ranges::lower_bound(V, x) - (V).begin()
#define bisect_right(V, x) ranges::upper_bound(V, x) - (V).begin()
#define BS(V, x) ranges::binary_search(V, x)
#define MEMSET(v, h) memset((v), h, sizeof(v))
#define MEMCPY(v, h) memcpy((v), h, sizeof(v))
template <class T>
using V = vector<T>;
template <class T>
bool chmax(T &a, const T &b) {
    if (a < b) {
        a = b;
        return 1;
    }
    return 0;
}
template <class T>
bool chmin(T &a, const T &b) {
    if (b < a) {
        a = b;
        return 1;
    }
    return 0;
}
string join(const vector<string> &v, const char *delim = 0) {
    string s;
    if (!v.empty()) {
        s += v[0];
        for (decltype(v.size()) i = 1, c = v.size(); i < c; ++i) {
            if (delim) s += delim;
            s += v[i];
        }
    }
    return s;
}
void print() { cout << endl; }
template <class T, class... A>
void print(const T &first, const A &...rest) {
    if (sizeof...(rest)) {
        cout << first << " ";
        print(rest...);
        return;
    }
    cout << first << endl;
}
template <class... A>
void print(const A &...rest) {
    print(rest...);
}
template <typename T>
void print(const vector<T> &V) {
    for (int i = 0; i < (int)V.size(); i++) {
        cout << (i == 0 ? "" : " ") << V[i];
    }
    cout << endl;
}
template <typename T>
void print(const vector<vector<T>> &V) {
    for (int i = 0; i < (int)V.size(); i++) {
        print(V[i]);
    }
}
template <typename T1, typename T2>
void print(const pair<T1, T2> &P) {
    cout << P.first << " " << P.second << endl;
}
template <typename T>
void print(const set<T> &st) {
    auto itr = st.begin();
    for (int i = 0; i < (int)st.size(); i++) {
        cout << (itr == st.begin() ? "" : " ") << *itr;
        itr++;
    }
    cout << endl;
}
template <typename T>
void print(const unordered_set<T> &st) {
    auto itr = st.begin();
    for (int i = 0; i < (int)st.size(); i++) {
        cout << (itr == st.begin() ? "" : " ") << *itr;
        itr++;
    }
    cout << endl;
}
template <typename T>
void print(const multiset<T> &st) {
    auto itr = st.begin();
    for (int i = 0; i < (int)st.size(); i++) {
        cout << (itr == st.begin() ? "" : " ") << *itr;
        itr++;
    }
    cout << endl;
}
template <typename T1, typename T2>
void print(const map<T1, T2> &mp) {
    cout << '{';
    for (auto &[key, val] : mp) {
        cout << key << ": " << val << ", ";
    }
    cout << '}' << endl;
}
template <typename T1, typename T2>
void print(const unordered_map<T1, T2> &mp) {
    cout << '{';
    for (auto &[key, val] : mp) {
        cout << key << ": " << val << ", ";
    }
    cout << '}' << endl;
}
template <class T>
vector<pair<ll, T>> enumerate(const vector<T> &a, ll start = 0, ll step = 1) {
    vector<pair<ll, T>> res;
    ll idx = start;
    for (const auto &i : a) {
        res.push_back(make_pair(idx, i));
        idx += step;
    }
    return res;
}
struct custom_hash {
    static uint64_t splitmix64(uint64_t x) {
        // http://xorshift.di.unimi.it/splitmix64.c
        x += 0x9e3779b97f4a7c15;
        x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
        x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
        return x ^ (x >> 31);
    }

    size_t operator()(uint64_t x) const {
        static const uint64_t FIXED_RANDOM =
            chrono::steady_clock::now().time_since_epoch().count();
        return splitmix64(x + FIXED_RANDOM);
    }
};
ll divceil(ll a, ll b) { return (a + (b - 1)) / b; }
PII divmod(ll a, ll b) {
    ll m = mod(a, b);
    return make_pair((a - m) / b, m);
}
uint32_t XORShiftRandom() {
    static uint32_t y = 2463534241;
    y = y ^ (y << 13);
    y = y ^ (y >> 17);
    return y = y ^ (y << 5);
}
#define RANDINT() XORShiftRandom()
#define RAND() (double)XORShiftRandom() / UINF32
#pragma endregion
int main() {
    cin.tie(nullptr);
    ios_base::sync_with_stdio(false);
    cout << fixed << setprecision(15);
    ll n;
    cin >> n;
    ll m;
    cin >> m;
    VI a(n);
    REP(i, n) { cin >> a[i]; }
    ll ans = 0;
    print(true ? "Yes" : "No");
}

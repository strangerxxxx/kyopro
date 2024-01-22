#pragma region header
/*Pythonで提出してない？*/
#ifdef STRANGERXXX
#define _GLIBCXX_DEBUG
#endif
#ifdef ATCODER
#pragma GCC target("arch=skylake-avx512")
#endif
#pragma GCC optimize("O3")
#pragma GCC optimize("unroll-loops")
// #pragma GCC target("sse,sse2,sse3,ssse3,sse4,popcnt,abm,mmx,avx,tune=native")
#include <bits/stdc++.h>
using namespace std;
#if __has_include(<atcoder/all>)
#include <atcoder/all>
using namespace atcoder;
using mint = modint998244353;
// using mint = modint1000000007;
ostream &operator<<(ostream &os, const mint &i) {
    os << i.val();
    return os;
}
#endif
#if __has_include(<boost/multiprecision/cpp_int.hpp>)
#include <boost/multiprecision/cpp_int.hpp>
using namespace boost::multiprecision;
#endif
typedef long long ll;
constexpr ll MOD = 998244353;
typedef unsigned int uint;
typedef unsigned long long ull;
const clock_t STARTTIME = clock();
#define TIME() static_cast<double>(clock() - STARTTIME) / CLOCKS_PER_SEC * 1.0
constexpr int INF32 = INT_MAX;
constexpr int IINF32 = INT_MIN;
constexpr uint UINF32 = UINT_MAX;
constexpr long long INF64 = LLONG_MAX;
constexpr long long IINF64 = LLONG_MIN;
constexpr unsigned long long UINF64 = ULLONG_MAX;
constexpr double EPS = 1e-10;
constexpr int dx[4] = {1, 0, -1, 0};
constexpr int dy[4] = {0, 1, 0, -1};
#if __cplusplus >= 201707L
#define REP(i, end) for (int i : views::iota(0, (end)))
#define REP_(end) for ([[maybe_unused]] int _ : views::iota(0, (end)))
#define REP3(i, begin, end) for (auto i : views::iota((begin), (end)))
#define REPR(i, end) for (int i : views::iota(0, (end)) | views::reverse)
#define REP3R(i, begin, end) \
    for (auto i : views::iota((begin), (end)) | views::reverse)
#define REPAR(i, I) for (const auto &i : I | views::reverse)
#else
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
#define REP_(n) for (ll _ = 0; _ < (ll)(n); _++)
#define REP3(i, m, n) for (ll i = (m); i < (ll)(n); i++)
#endif
#define REP4(i, begin, end, step) \
    for (ll i = (begin); i < (ll)(end); i = i + (ll)(step))
#define REP4R(i, begin, end, step) \
    for (ll i = (begin); i >= (ll)(end); i = i + (ll)(step))
#define REPA(i, I) for (const auto &i : I)
#define REPB(i, I) for (auto &&i : I)
#define REPIJ(i, j, n)               \
    for (ll i = 0; i < (ll)(n); i++) \
        for (ll j = i + 1; j < (ll)(n); j++)
#define REPIN(a)         \
    for (auto &&i : a) { \
        cin >> i;        \
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
typedef pair<ll, ll> PII;
typedef vector<ll> VI;
typedef vector<VI> VVI;
typedef vector<VVI> VVVI;
typedef vector<string> VS;
typedef vector<VS> VVS;
typedef vector<PII> VP;
typedef set<ll> SI;
typedef multiset<ll> MSI;
typedef unordered_set<ll, custom_hash> USI;
typedef map<ll, ll> MI;
typedef unordered_map<ll, ll, custom_hash> UMI;
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
#define SUM(V) reduce((V).begin(), (V).end())
#if __cplusplus >= 201707L
#define LEN(x) ssize(x)
#define SORT(V) ranges::sort(V)
#define RSORT(V) ranges::sort(V, ranges::greater())
#define REVERSE(V) ranges::reverse(V)
#define MINMAX(a, b) ranges::minmax(a, b)
#define MAXVI(V) *ranges::max_element(V)
#define MINVI(V) *ranges::min_element(V)
#define UB(V, x) ranges::upper_bound(V, x)
#define LB(V, x) ranges::lower_bound(V, x)
#define bisect_left(V, x) ranges::lower_bound(V, x) - (V).begin()
#define bisect_right(V, x) ranges::upper_bound(V, x) - (V).begin()
#define BS(V, x) ranges::binary_search(V, x)
#define IN(V, x) (V).contains(x)
constexpr double PI = numbers::pi;
#else
#define LEN(x) (int)(x).size()
#define SORT(V) sort((V).begin(), (V).end())
#define RSORT(V) sort((V).rbegin(), (V).rend())
#define IN(V, x) (bool)(V).count(x)
constexpr double PI = 3.141592653589793238462643383;
#endif
#define mod(a, b) ((a) % (b) + (b)) % (b)
#define MEMSET(v, h) memset((v), h, sizeof(v))
#define MEMCPY(v, h) memcpy((v), h, sizeof(v))
template <class T>
using V = vector<T>;
template <class T>
using VV = vector<vector<T>>;
template <class T>
bool chmax(T &a, const T &b) {
    if (a < b) {
        a = b;
        return true;
    }
    return false;
}
template <class T>
bool chmin(T &a, const T &b) {
    if (b < a) {
        a = b;
        return true;
    }
    return false;
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
template <class Key, class Compare, class Allocator>
ostream &operator<<(ostream &os, const set<Key, Compare, Allocator> &st) {
    auto itr = st.begin();
    for (int i = 0; i < (int)st.size(); i++) {
        cout << (itr == st.begin() ? "" : " ") << *itr;
        itr++;
    }
    return os;
}
template <class Key, class Hash, class Pred, class Allocator>
ostream &operator<<(ostream &os, unordered_set<Key, Hash, Pred, Allocator> st) {
    auto itr = st.begin();
    for (int i = 0; i < (int)st.size(); i++) {
        cout << (itr == st.begin() ? "" : " ") << *itr;
        itr++;
    }
    return os;
}
template <class Key, class Compare, class Allocator>
ostream &operator<<(ostream &os, multiset<Key, Compare, Allocator> st) {
    auto itr = st.begin();
    for (int i = 0; i < (int)st.size(); i++) {
        cout << (itr == st.begin() ? "" : " ") << *itr;
        itr++;
    }
    return os;
}
template <class Key, class T, class Compare, class Allocator>
ostream &operator<<(ostream &os, const map<Key, T, Compare, Allocator> &mp) {
    cout << '{';
    for (auto &[key, val] : mp) {
        cout << key << ": " << val << ", ";
    }
    cout << '}';
    return os;
}
template <class Key, class T, class Hash, class Pred, class Allocator>
ostream &operator<<(ostream &os,
                    const unordered_map<Key, T, Hash, Pred, Allocator> &mp) {
    cout << '{';
    for (auto &[key, val] : mp) {
        cout << key << ": " << val << ", ";
    }
    cout << '}';
    return os;
}
template <class T, class Container, class Compare>
ostream &operator<<(ostream &os, priority_queue<T, Container, Compare> pq) {
    while (!pq.empty()) {
        os << pq.top() << " ";
        pq.pop();
    }
    return os;
}
template <class T1, class T2>
ostream &operator<<(ostream &os, const pair<T1, T2> &p) {
    cout << p.first << " " << p.second;
    return os;
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
    for (const auto &i : V) {
        print(i);
    }
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

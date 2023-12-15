#include <bits/stdc++.h>
using namespace std;
#include <atcoder/all>
using namespace atcoder;
using mint = modint998244353;
typedef long long ll;
const ll MOD = 998244353;
const long long INF64 = LLONG_MAX;
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
typedef vector<ll> VI;
// https://onlinejudge.u-aizu.ac.jp/problems/DSL_2_A
using S = ll;
// 区間取得のときの関数
S op(S f, S g) { return min(f, g); }
// opの単位元
S e() { return INF64; }
int main() {
    cin.tie(nullptr);
    ios_base::sync_with_stdio(false);
    ll n;
    cin >> n;
    ll q;
    cin >> q;
    vector<S> a(n, (1LL << 31) - 1);
    atcoder::segtree<S, op, e> seg(a);
    REP(_, q) {
        ll com, x, y;
        cin >> com >> x >> y;
        if (com == 0) {
            seg.set(x, y);
        } else {
            cout << seg.prod(x, y + 1) << endl;
        }
    }
}
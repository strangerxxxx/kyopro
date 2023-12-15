#include <bits/stdc++.h>
using namespace std;
#include <atcoder/all>
using namespace atcoder;
using mint = modint998244353;
typedef long long ll;
const ll MOD = 998244353;
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
// https://onlinejudge.u-aizu.ac.jp/problems/DSL_2_B
using S = ll;
// 区間取得のときの関数
S op(S f, S g) { return f + g; }
// opの単位元
S e() { return 0; }
int main() {
    cin.tie(nullptr);
    ios_base::sync_with_stdio(false);
    ll n;
    cin >> n;
    ll q;
    cin >> q;
    vector<S> a(n, 0);
    atcoder::segtree<S, op, e> seg(a);
    REP(_, q) {
        ll com, x, y;
        cin >> com >> x >> y;
        if (com == 0) {
            seg.set(x - 1, seg.get(x - 1) + y);
        } else {
            cout << seg.prod(x - 1, y) << endl;
        }
    }
}
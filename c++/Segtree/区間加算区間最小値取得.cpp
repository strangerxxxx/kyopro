#include <bits/stdc++.h>
using namespace std;
#include <atcoder/all>
using namespace atcoder;
using mint = modint998244353;
typedef long long ll;
const ll MOD = 998244353;
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
// https://onlinejudge.u-aizu.ac.jp/courses/library/3/DSL/2/DSL_2_H
using S = ll;
using T = ll;
// 区間取得のときの関数
S op(S a, S b) { return min(a, b); }
// opの単位元
S e() { return INF64; }
// ノードxに対し操作fを作用させる関数
S mapping(T f, S x) { return f + x; }
// lazyに対する操作の関数(gにfを作用させる)
T composition(T f, T g) { return f + g; }
// mappingの単位元
T id() { return 0; }
int main() {
    cin.tie(nullptr);
    ios_base::sync_with_stdio(false);
    ll n;
    cin >> n;
    ll q;
    cin >> q;
    vector<S> a(n, 0);
    atcoder::lazy_segtree<S, op, e, T, mapping, composition, id> seg(a);
    REP(_, q) {
        ll com;
        cin >> com;
        if (com == 0) {
            ll s, t, x;
            cin >> s >> t >> x;
            seg.apply(s, t + 1, x);
        } else {
            ll s, t;
            cin >> s >> t;
            cout << seg.prod(s, t + 1) << endl;
        }
    }
}
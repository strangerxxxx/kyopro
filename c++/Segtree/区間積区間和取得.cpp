#include <bits/stdc++.h>
using namespace std;
#include <atcoder/all>
using namespace atcoder;
using mint = modint998244353;
typedef long long ll;
const ll MOD = 998244353;
#define REP(i, n) for (ll i = 0; i < (ll)(n); i++)
// https://atcoder.jp/contests/abc332/tasks/abc332_f
struct S {
    mint value;
    ll size;
};
struct F {
    mint l, r;
};
// 区間取得のときの関数
S op(S a, S l) { return {a.value + l.value, a.size + l.size}; }
// opの単位元
S e() { return {0, 1}; }
// ノードxに対し操作fを作用させる関数
S mapping(F f, S x) { return {x.value * f.l + x.size * f.r, x.size}; }
// lazyに対する操作の関数(gにfを作用させる)
F composition(F f, F g) { return {f.l * g.l, f.l * g.r + f.r}; }
// mappingの単位元
F id() { return {1, 0}; }
int main() {
    cin.tie(nullptr);
    ios_base::sync_with_stdio(false);
    ll n;
    cin >> n;
    ll m;
    cin >> m;
    vector<S> a(n);
    REP(i, n) {
        ll j;
        cin >> j;
        a[i] = {j, 1};
    }

    lazy_segtree<S, op, e, F, mapping, composition, id> seg(a);
    REP(i, m) {
        ll l, r, x;
        cin >> l >> r >> x;
        ll range = r - (--l);
        mint inv_range = inv_mod(range, MOD);
        seg.apply(l, r, {(range - 1) * inv_range, x * inv_range});
    }
    VI ans(n);
    REP(i, n) { ans[i] = seg.prod(i, i + 1).value.val(); }
    REP(i, n - 1) { cout << ans[i] << ' '; }
    cout << ans[n - 1] << endl;
}
// https://atcoder.jp/contests/practice2/tasks/practice2_k
struct S {
    mint value;
    ll size;
};
struct F {
    mint b, c;
};
// 区間取得のときの関数
S op(S a, S b) { return {a.value + b.value, a.size + b.size}; }
// opの単位元
S e() { return {0, 1}; }
// ノードxに対し操作fを作用させる関数
S mapping(F f, S x) { return {x.value * f.b + x.size * f.c, x.size}; }
// lazyに対する操作の関数(gにfを作用させる)
F composition(F f, F g) { return {f.b * g.b, f.b * g.c + f.c}; }
// mappingの単位元
F id() { return {1, 0}; }
int main() {
    cin.tie(nullptr);
    ios_base::sync_with_stdio(false);
    ll n;
    cin >> n;
    ll q;
    cin >> q;
    vector<S> a(n);
    REP(i, n) {
        ll j;
        cin >> j;
        a[i] = {j, 1};
    }
    lazy_segtree<S, op, e, F, mapping, composition, id> seg(a);
    REP(_, q) {
        int query;
        cin >> query;
        if (query == 0) {
            ll l, r, b, c;
            cin >> l >> r >> b >> c;
            seg.apply(l, r, {b, c});
        } else {
            ll l, r;
            cin >> l >> r;
            print(seg.prod(l, r).value.val());
        }
    }
}

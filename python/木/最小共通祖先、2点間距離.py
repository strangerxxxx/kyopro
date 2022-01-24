class LowestCommonAncestor_doubling():
    # 最小共通祖先ダブリング版(重みなし)
    def __init__(self, n, childs, root=0) -> None:
        # childs[v]: 頂点vの子頂点 (親頂点は含んでもよい)
        self.n = n
        parent = [None] * self.n
        parent[root] = -1
        self.depth = [None] * self.n
        self.depth[root] = 0
        q = [root]
        while q:
            v = q.pop()
            for c in childs[v]:
                if parent[c] is None:
                    parent[c] = v
                    self.depth[c] = self.depth[v] + 1
                    q.append(c)

        self.LV = (self.n - 1).bit_length()
        self.kparent = self._construct(parent)

    def _construct(self, parent):
        kparent = [parent]
        S = parent
        for _k in range(self.LV):
            T = [0] * self.n
            for i in range(self.n):
                if S[i] is None:
                    continue
                T[i] = S[S[i]]
            kparent.append(T)
            S = T
        return kparent

    def query(self, u, v):
        dd = self.depth[v] - self.depth[u]
        if dd < 0:
            u, v = v, u
            dd = -dd

        # assert depth[u] <= depth[v]
        for k in range(self.LV + 1):
            if dd & 1:
                v = self.kparent[k][v]
            dd >>= 1

        # assert depth[u] == depth[v]
        if u == v:
            return u

        for k in range(self.LV - 1, -1, -1):
            pu = self.kparent[k][u]
            pv = self.kparent[k][v]
            if pu != pv:
                u = pu
                v = pv

        # assert self.kparent[0][u] == self.kparent[0][v]
        return self.kparent[0][u]

    def dist(self, u, v):
        # u, vの距離
        p = self.query(u, v)
        return self.depth[u] + self.depth[v] - 2 * self.depth[p]


class LowestCommonAncestor_euler():
    def __init__(self, n, childs, parent=0) -> None:
        # childs[v]: 頂点vの子頂点 (親頂点は含まない)
        self.n = n
        self.route = []
        self.first = [None] * self.n
        self.last = [None] * self.n
        self.order = [None] * self.n
        self.depth = [None] * self.n

        q = [(parent, 0)]
        order = 0
        while q:
            v, d = q.pop()
            if v >= 0:
                self.first[v] = len(self.route)
                self.last[v] = len(self.route)
                self.depth[v] = d
                self.order[v] = order
                order += 1
                self.route.append(v)
                for w in childs[v]:
                    if self.depth[w] is None:
                        q.append((~v, d))
                        q.append((w, d + 1))
            else:
                self.last[~v] = len(self.route)
                self.route.append(~v)
        # 存在しない範囲は深さが他よりも大きくなるようにする
        self.INF = (self.n, None)

        # LCAを計算するクエリの前計算
        M = 2 * self.n
        self.M0 = 2 ** (M - 1).bit_length()
        self.data = [self.INF] * (2 * self.M0)
        for i, v in enumerate(self.route):
            self.data[self.M0 - 1 + i] = (self.depth[v], i)
        for i in range(self.M0 - 2, -1, -1):
            self.data[i] = min(self.data[2 * i + 1], self.data[2 * i + 2])

    def _query(self, a, b):
        # LCAの計算 (generatorで最小値を求める)
        yield self.INF
        a += self.M0
        b += self.M0
        while a < b:
            if b & 1:
                b -= 1
                yield self.data[b-1]
            if a & 1:
                yield self.data[a-1]
                a += 1
            a >>= 1
            b >>= 1

    def query(self, u, v):
        # LCAの計算 (外から呼び出す関数)
        fu = self.first[u]
        fv = self.first[v]
        if fu > fv:
            fu, fv = fv, fu
        return self.route[min(self._query(fu, fv+1))[1]]

    def dist(self, u, v):
        # u, vの距離
        p = self.query(u, v)
        return self.depth[u] + self.depth[v] - 2 * self.depth[p]

    def treesize(self, *a):
        # *aからなる木の大きさ
        s = 0
        b = sorted(a, key=lambda x: self.order[x])
        for i in range(len(b)):
            s += self.dist(b[i], b[i - 1])
        return s // 2


class LowestCommonAncestor_doubling_weight():
    def __init__(self, n, childs, root=0) -> None:
        # childs[v]: 頂点vの子頂点とその重み (親頂点は含んでもよい)
        self.n = n
        parent = [None] * self.n
        parent[root] = -1
        self.depth = [None] * self.n
        self.depth[root] = 0
        self.distance = [None] * self.n
        self.distance[root] = 0
        q = [root]
        while q:
            v = q.pop()
            for c, d in childs[v]:
                if parent[c] is None:
                    parent[c] = v
                    self.depth[c] = self.depth[v] + 1
                    self.distance[c] = self.distance[v] + d
                    q.append(c)

        self.LV = (self.n - 1).bit_length()
        self.kparent = self._construct(parent)

    def _construct(self, parent):
        kparent = [parent]
        S = parent
        for _k in range(self.LV):
            T = [0] * self.n
            for i in range(self.n):
                if S[i] is None:
                    continue
                T[i] = S[S[i]]
            kparent.append(T)
            S = T
        return kparent

    def query(self, u, v):
        dd = self.depth[v] - self.depth[u]
        if dd < 0:
            u, v = v, u
            dd = -dd

        # assert depth[u] <= depth[v]
        for k in range(self.LV + 1):
            if dd & 1:
                v = self.kparent[k][v]
            dd >>= 1

        # assert depth[u] == depth[v]
        if u == v:
            return u

        for k in range(self.LV - 1, -1, -1):
            pu = self.kparent[k][u]
            pv = self.kparent[k][v]
            if pu != pv:
                u = pu
                v = pv

        # assert self.kparent[0][u] == self.kparent[0][v]
        return self.kparent[0][u]

    def dist(self, u, v):
        # u, vの距離
        p = self.query(u, v)
        return self.distance[u] + self.distance[v] - 2 * self.distance[p]


class LowestCommonAncestor_euler_weight():
    def __init__(self, n, childs, parent=0) -> None:
        # childs[v]: 頂点vの子頂点とその重み (親頂点は含んでもよい)
        self.n = n
        self.route = []
        self.first = [None] * self.n
        self.last = [None] * self.n
        self.order = [None] * self.n
        self.depth = [None] * self.n

        q = [(parent, 0)]
        order = 0
        while q:
            v, d = q.pop()
            if v >= 0:
                self.first[v] = len(self.route)
                self.last[v] = len(self.route)
                self.depth[v] = d
                self.order[v] = order
                order += 1
                self.route.append(v)
                for w, dist in childs[v]:
                    if self.depth[w] is None:
                        q.append((~v, d))
                        q.append((w, d + dist))
            else:
                self.last[~v] = len(self.route)
                self.route.append(~v)
        # 存在しない範囲は深さが他よりも大きくなるようにする
        self.INF = (self.n, None)

        # LCAを計算するクエリの前計算
        M = 2 * self.n
        self.M0 = 2 ** (M - 1).bit_length()
        self.data = [self.INF] * (2 * self.M0)
        for i, v in enumerate(self.route):
            self.data[self.M0 - 1 + i] = (self.depth[v], i)
        for i in range(self.M0 - 2, -1, -1):
            self.data[i] = min(self.data[2 * i + 1], self.data[2 * i + 2])

    def _query(self, a, b):
        # LCAの計算 (generatorで最小値を求める)
        yield self.INF
        a += self.M0
        b += self.M0
        while a < b:
            if b & 1:
                b -= 1
                yield self.data[b-1]
            if a & 1:
                yield self.data[a-1]
                a += 1
            a >>= 1
            b >>= 1

    def query(self, u, v):
        # LCAの計算 (外から呼び出す関数)
        fu = self.first[u]
        fv = self.first[v]
        if fu > fv:
            fu, fv = fv, fu
        return self.route[min(self._query(fu, fv+1))[1]]

    def dist(self, u, v):
        # u, vの距離
        p = self.query(u, v)
        return self.depth[u] + self.depth[v] - 2 * self.depth[p]

    def treesize(self, *a):
        # *aからなる木の大きさ
        s = 0
        b = sorted(a, key=lambda x: self.order[x])
        for i in range(len(b)):
            s += self.dist(b[i], b[i - 1])
        return s // 2


G = [[1, 2], [], [3], []]
l = LowestCommonAncestor_doubling(4, G)
m = LowestCommonAncestor_euler(4, G)
print(l.query(1, 2), m.query(1, 2))
print(l.query(0, 2), m.query(0, 2))
print(l.query(1, 3), m.query(1, 3))
print(l.query(2, 3), m.query(2, 3))

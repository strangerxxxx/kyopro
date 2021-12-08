class UnionFind:
    def __init__(self, n: int):
        self.n = n
        self.parent = [-1] * n
        self.groups = n

    def find(self, x: int) -> int:
        if self.parent[x] < 0:
            return x
        else:
            self.parent[x] = self.find(self.parent[x])
            return self.parent[x]

    def union(self, x: int, y: int) -> None:
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return
        if self.parent[x] > self.parent[y]:
            x, y = y, x
        self.parent[x] += self.parent[y]
        self.parent[y] = x
        self.groups -= 1

    def union_right(self, x: int, y: int) -> None:
        if y > x:
            x, y = y, x
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return
        self.parent[x] += self.parent[y]
        self.parent[y] = x
        self.groups -= 1

    def union_left(self, x: int, y: int) -> None:
        if x > y:
            x, y = y, x
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return
        self.parent[x] += self.parent[y]
        self.parent[y] = x
        self.groups -= 1

    def size(self, x: int) -> int:
        return -self.parent[self.find(x)]

    def same(self, x: int, y: int) -> bool:
        return self.find(x) == self.find(y)

    def members(self, x: int):
        root = self.find(x)
        return [i for i in range(self.n) if self.find(i) == root]

    def roots(self):
        return [i for i, x in enumerate(self.parent) if x < 0]

    def group_count(self) -> int:
        # return len(self.roots())
        return self.groups

    def sizes(self):
        return {i: -x for i, x in enumerate(self.parent) if x < 0}

    def all_group_members(self):
        d = {}
        for i in range(self.n):
            p = self.find(i)
            d[p] = d.get(p, []) + [i]
        return d

    def __str__(self):
        return '\n'.join('{}: {}'.format(k, v) for k, v in self.all_group_members().items())


class PartiallyPersistentUnionFind:
    # 部分永続UnionFind
    def __init__(self, n):
        self.INF = float("inf")
        self.n = n
        self.parent = list(range(self.n))
        self.sz = [1] * self.n
        self.pdepth = [1] * self.n
        self.S = [[(0, 1)] for _ in range(self.n)]
        self.T = [self.INF] * self.n

    def find(self, x, t):
        while self.T[x] <= t:
            x = self.parent[x]
        return x

    def union(self, x, y, t):
        px = self.find(x, t)
        py = self.find(y, t)
        if px == py:
            return 0
        if self.pdepth[py] < self.pdepth[px]:
            self.parent[py] = px
            self.T[py] = t
            self.sz[px] += self.sz[py]
            self.S[px].append((t, self.sz[px]))
        else:
            self.parent[px] = py
            self.T[px] = t
            self.sz[py] += self.sz[px]
            self.S[py].append((t, self.sz[py]))
            self.pdepth[py] = max(self.pdepth[py], self.pdepth[px] + 1)
        return 1

    def size(self, x, t):
        from bisect import bisect_right
        y = self.find(x, t)
        idx = bisect_right(self.S[y], (t, self.INF)) - 1
        return self.S[y][idx]

    def same(self, x, y, t):
        return self.find(x, t) == self.find(y, t)


class WeightedUnionFind:
    # ポテンシャル付きUnionFind
    def __init__(self, n):
        self.n = n
        self.parent = list(range(n + 1))
        self.rank = [0] * (n + 1)
        self.weight = [0] * (n + 1)

    def find(self, x):
        if self.parent[x] == x:
            return x
        else:
            y = self.find(self.parent[x])
            self.weight[x] += self.weight[self.parent[x]]
            self.parent[x] = y
            return y

    def union(self, x, y, w):
        # weight(y)=weight(x)+wとなるようにxとyをマージする
        px = self.find(x)
        py = self.find(y)
        if px == py:
            return
        if self.rank[px] > self.rank[py]:
            px, py, w = py, px, -w
        self.parent[px] = py
        self.weight[px] = w - self.weight[x] + self.weight[y]
        if self.rank[px] == self.rank[py]:
            self.rank[py] += 1

    def same(self, x, y):
        return self.find(x) == self.find(y)

    def diff(self, x, y):
        # xからyへのコスト
        if self.same(x, y):
            return self.weight[x] - self.weight[y]
        else:
            return None

    def all_group_members(self):
        d = {}
        w = {}
        for i in range(self.n):
            p = self.find(i)
            d[p] = d.get(p, []) + [i]
            w[p] = w.get(p, []) + [self.weight[i]]
        return d, w

    def __str__(self):
        d, w = self.all_group_members()
        return '\n'.join('{}: {} : {}'.format(k, v, x) for (k, v), (_, x) in zip(d.items(), w.items()))

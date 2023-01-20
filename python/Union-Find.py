class UnionFind:
    def __init__(self, n: int) -> None:
        self.n = n
        self.parent = [-1] * n
        self.groups = n

    def find(self, x: int) -> int:
        if self.parent[x] < 0:
            return x
        p = x
        while self.parent[p] >= 0:
            p = self.parent[p]
        while self.parent[x] >= 0:
            self.parent[x], x = p, self.parent[x]
        return p

    def union(self, x: int, y: int) -> bool:
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return False
        if self.parent[x] > self.parent[y]:
            x, y = y, x
        self.parent[x] += self.parent[y]
        self.parent[y] = x
        self.groups -= 1
        return True

    def union_right(self, x: int, y: int) -> bool:
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return False
        self.parent[x] += self.parent[y]
        self.parent[y] = x
        self.groups -= 1
        return True

    def union_left(self, x: int, y: int) -> bool:
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return False
        self.parent[x] += self.parent[y]
        self.parent[y] = x
        self.groups -= 1
        return True

    def size(self, x: int) -> int:
        return -self.parent[self.find(x)]

    def same(self, x: int, y: int) -> bool:
        return self.find(x) == self.find(y)

    def members(self, x: int) -> list:
        root = self.find(x)
        return [i for i in range(self.n) if self.find(i) == root]

    def roots(self) -> list:
        return [i for i, x in enumerate(self.parent) if x < 0]

    def group_count(self) -> int:
        return self.groups

    def sizes(self) -> dict:
        return {i: -x for i, x in enumerate(self.parent) if x < 0}

    def add_member(self):
        self.n += 1
        self.groups += 1
        self.parent.append(-1)
        return self.n - 1

    def all_group_members(self) -> dict:
        from collections import defaultdict

        d = defaultdict(list)
        for i in range(self.n):
            d[self.find(i)].append(i)
        return d

    def __str__(self) -> str:
        return "\n".join(
            "{}: {}".format(k, v) for k, v in self.all_group_members().items()
        )

    __repr__ = __str__


class UnionFind_Hashable:
    def __init__(self, a=[]) -> None:
        self.a = list(set(a))
        self.d = {x: i for i, x in enumerate(self.a)}
        self.n = len(self.d)
        self.parent = [-1] * len(self.d)
        self.groups = len(self.d)

    def get_index(self, x):
        if x in self.d:
            return self.d[x]
        self.a.append(x)
        self.d[x] = self.n
        self.n += 1
        self.parent.append(-1)
        self.groups += 1
        return self.n - 1

    def find(self, x) -> int:
        y = self.get_index(x)
        if self.parent[y] < 0:
            return y
        p = y
        while self.parent[p] >= 0:
            p = self.parent[p]
        while self.parent[y] >= 0:
            self.parent[y], y = p, self.parent[y]
        return p

    def union(self, x, y) -> bool:
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return False
        if self.parent[x] > self.parent[y]:
            x, y = y, x
        self.parent[x] += self.parent[y]
        self.parent[y] = x
        self.groups -= 1
        return True

    def size(self, x) -> int:
        return -self.parent[self.find(x)]

    def same(self, x, y) -> bool:
        return self.find(x) == self.find(y)

    def members(self, x) -> list:
        root = self.find(x)
        return [i for i in self.a if self.find(i) == root]

    def roots(self) -> list:
        return [self.a[i] for i, x in enumerate(self.parent) if x < 0]

    def group_count(self) -> int:
        return self.groups

    def sizes(self) -> dict:
        return {self.a[i]: -x for i, x in enumerate(self.parent) if x < 0}

    def all_group_members(self) -> dict:
        from collections import defaultdict

        d = defaultdict(list)
        for i in self.a:
            d[self.a[self.find(i)]].append(i)
        return d

    def __str__(self) -> str:
        return "\n".join(
            "{}: {}".format(k, v) for k, v in self.all_group_members().items()
        )


class PartiallyPersistentUnionFind:
    # 部分永続UnionFind
    def __init__(self, n) -> None:
        self.INF = float("inf")
        self.n = n
        self.parent = list(range(self.n))
        self.sz = [1] * self.n
        self.pdepth = [1] * self.n
        self.S = [[(0, 1)] for _ in range(self.n)]
        self.T = [self.INF] * self.n

    def find(self, x, t) -> int:
        while self.T[x] <= t:
            x = self.parent[x]
        return x

    def union(self, x, y, t) -> bool:
        px = self.find(x, t)
        py = self.find(y, t)
        if px == py:
            return False
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
        return True

    def size(self, x, t) -> int:
        from bisect import bisect_right

        y = self.find(x, t)
        idx = bisect_right(self.S[y], (t, self.INF)) - 1
        return self.S[y][idx]

    def same(self, x, y, t) -> bool:
        return self.find(x, t) == self.find(y, t)


class WeightedUnionFind:
    # ポテンシャル付きUnionFind
    def __init__(self, n) -> None:
        self.n = n
        self.parent = list(range(n + 1))
        self.rank = [0] * (n + 1)
        self.weight = [0] * (n + 1)

    def find(self, x) -> int:
        if self.parent[x] == x:
            return x
        else:
            q = []
            while self.parent[x] != x:
                q.append(x)
                x = self.parent[x]
            for i in reversed(q):
                self.weight[i] += self.weight[self.parent[i]]
                self.parent[i] = x
            return x

    def union(self, x, y, w) -> bool:
        # weight(y)=weight(x)+wとなるようにxとyをマージする
        px = self.find(x)
        py = self.find(y)
        if px == py:
            return False
        if self.rank[px] > self.rank[py]:
            x, y, px, py, w = y, x, py, px, -w
        self.parent[px] = py
        self.weight[px] = w - self.weight[x] + self.weight[y]
        if self.rank[px] == self.rank[py]:
            self.rank[py] += 1
        return True

    def same(self, x, y) -> bool:
        return self.find(x) == self.find(y)

    def diff(self, x, y) -> int:
        # xからyへのコスト
        if self.same(x, y):
            return self.weight[x] - self.weight[y]
        else:
            return None

    def all_group_members(self) -> tuple:
        d = {}
        w = {}
        for i in range(self.n):
            p = self.find(i)
            if p in d:
                d[p].append(i)
                w[p].append(self.weight[i])
            else:
                d[p] = [i]
                w[p] = [self.weight[i]]
        return d, w

    def __str__(self) -> str:
        d, w = self.all_group_members()
        return "\n".join(
            "{}: {} : {}".format(k, v, x)
            for (k, v), (_, x) in zip(d.items(), w.items())
        )

    __repr__ = __str__

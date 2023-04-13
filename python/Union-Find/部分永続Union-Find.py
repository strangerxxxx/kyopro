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

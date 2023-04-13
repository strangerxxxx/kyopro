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

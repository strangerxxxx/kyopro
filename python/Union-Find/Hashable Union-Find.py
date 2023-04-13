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

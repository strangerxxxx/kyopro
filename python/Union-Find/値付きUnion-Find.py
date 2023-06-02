class UnionFind_value:
    def __init__(self, n: int, value=None) -> None:
        self.n = n
        self.parent = [-1] * n
        self.groups = n
        self.values = value[:] if value else [0] * n

    def merge(self, parent: int, child: int) -> None:
        self.values[parent] += self.values[child]

    def get_value(self, x: int):
        return self.values[x]

    def set_value(self, x: int, v: int):
        self.values[self.find(x)] = v

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
        self.merge(x, y)
        self.groups -= 1
        return True

    def union_left(self, parent: int, child: int) -> bool:
        parent = self.find(parent)
        child = self.find(child)
        if parent == child:
            return False
        self.parent[parent] += self.parent[child]
        self.parent[child] = parent
        self.merge(parent, child)
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

class UnionFind_Rollbackable:
    # unionしたときだけ履歴を保持する
    def __init__(self, n: int) -> None:
        self.n = n
        self.parent = [-1] * n
        self.groups = n
        self.history = []

    def find(self, x: int) -> int:
        if self.parent[x] < 0:
            return x
        p = x
        while self.parent[p] >= 0:
            p = self.parent[p]
        return p

    def union(self, x: int, y: int) -> bool:
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return False
        if self.parent[x] > self.parent[y]:
            x, y = y, x
        self.history.append((x, self.parent[x]))
        self.history.append((y, self.parent[y]))
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

    def add_member(self) -> int:
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

    def undo(self) -> None:
        for _ in range(2):
            i, j = self.history.pop()
            self.parent[i] = j
        self.groups += 1

    def max_rollback_count(self) -> int:
        return len(self.history) >> 1

    def snapshot(self) -> None:
        self.history.clear()

    def rollback(self, state=-1) -> None:
        # assert -1 <= state <= self.max_rollback_count()
        if state == -1:
            state = self.max_rollback_count()
        for _ in range(state):
            self.undo()


class UnionFind_Rollbackable2:
    # unionしなくても履歴を保持する
    def __init__(self, n: int) -> None:
        self.n = n
        self.parent = [-1] * n
        self.groups = n
        self.history = []

    def find(self, x: int) -> int:
        if self.parent[x] < 0:
            return x
        p = x
        while self.parent[p] >= 0:
            p = self.parent[p]
        return p

    def union(self, x: int, y: int) -> bool:
        x = self.find(x)
        y = self.find(y)
        if x == y:
            self.history.append((-1, -1))
            self.history.append((-1, -1))
            return False
        if self.parent[x] > self.parent[y]:
            x, y = y, x
        self.history.append((x, self.parent[x]))
        self.history.append((y, self.parent[y]))
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

    def add_member(self) -> int:
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

    def undo(self) -> bool:
        if self.history[-1][0] >= 0:
            for _ in range(2):
                i, j = self.history.pop()
                self.parent[i] = j
            self.groups += 1
            return True
        else:
            self.history.pop()
            self.history.pop()
            return False

    def max_rollback_count(self) -> int:
        return len(self.history) >> 1

    def snapshot(self) -> None:
        self.history.clear()

    def rollback(self, state=-1) -> None:
        # assert -1 <= state <= self.max_rollback_count()
        if state == -1:
            state = self.max_rollback_count()
        for _ in range(state):
            self.undo()

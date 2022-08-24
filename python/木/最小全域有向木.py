def directed_MST(n, m, edges, root=0):
    froms = [0] * n
    from_cost = [0] * n
    from_heap = [SkewHeap() for i in range(n)]
    UF = UnionFind(n)
    par = [-1] * m
    stem = [-1] * n
    used = [0] * n
    used[root] = 2
    inds = []
    for i, (u, v, c) in enumerate(edges):
        from_heap[v].heappush(c * m + i)
    res = 0
    for v in range(n):
        if used[v] != 0:
            continue
        proc = []
        chi = []
        cycle = 0
        while used[v] != 2:
            used[v] = 1
            proc.append(v)
            if from_heap[v].root is None:
                return -1, [-1] * n
            from_cost[v], ind = divmod(from_heap[v].heappop(), m)
            froms[v] = UF.find(edges[ind][0])
            if stem[v] == -1:
                stem[v] = ind
            if froms[v] == v:
                continue
            res += from_cost[v]
            inds.append(ind)
            while cycle:
                par[chi.pop()] = ind
                cycle -= 1
            chi.append(ind)
            if used[froms[v]] == 1:
                p = v
                while True:
                    if not from_heap[p].root is None:
                        from_heap[p].heapadd(-from_cost[p] * m)
                    if p != v:
                        UF.union(v, p)
                        from_heap[v].root = from_heap[v].heapmeld(
                            from_heap[v].root, from_heap[p].root)
                    p = UF.find(froms[p])
                    new_v = UF.find(v)
                    from_heap[new_v] = from_heap[v]
                    v = new_v
                    cycle += 1
                    if p == v:
                        break
            else:
                v = froms[v]
        for v in proc:
            used[v] = 2
    visited = [0] * m
    tree = [-1] * n
    for i in inds[::-1]:
        if visited[i]:
            continue
        u, v, c = edges[i]
        tree[v] = u
        x = stem[v]
        while x != i:
            visited[x] = 1
            x = par[x]
    return res, tree


class UnionFind:
    def __init__(self, n: int) -> None:
        self.n = n
        self.parent = [-1] * n

    def find(self, x: int) -> int:
        if self.parent[x] < 0:
            return x
        else:
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
        return True

    def union_right(self, x: int, y: int) -> bool:
        if y > x:
            x, y = y, x
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return False
        self.parent[x] += self.parent[y]
        self.parent[y] = x
        return True

    def union_left(self, x: int, y: int) -> bool:
        if x > y:
            x, y = y, x
        x = self.find(x)
        y = self.find(y)
        if x == y:
            return False
        self.parent[x] += self.parent[y]
        self.parent[y] = x
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

    def sizes(self) -> dict:
        return {i: -x for i, x in enumerate(self.parent) if x < 0}

    def all_group_members(self) -> dict:
        from collections import defaultdict
        d = defaultdict(list)
        for i in range(self.n):
            p = self.find(i)
            d[p].append(i)
        return d

    def __str__(self) -> str:
        return '\n'.join('{}: {}'.format(k, v)
                         for k, v in self.all_group_members().items())


class SHNode:
    def __init__(self, val):
        self.left = None
        self.right = None
        self.val = val
        self.add = 0

    def lazy(self):
        if self.left is not None:
            self.left.add += self.add
        if self.right is not None:
            self.right.add += self.add
        self.val += self.add
        self.add = 0


class SkewHeap:
    def __init__(self):
        self.root = None

    def heapmeld(self, h1, h2):
        if h1 is None:
            return h2
        if h2 is None:
            return h1
        if h1.val + h1.add > h2.val + h2.add:
            h1, h2 = h2, h1
        h1.lazy()
        h1.right = self.heapmeld(h2, h1.right)
        h1.left, h1.right = h1.right, h1.left
        return h1

    def heappop(self):
        res = self.root
        res.lazy()
        self.root = self.heapmeld(res.left, res.right)
        return res.val

    def heappush(self, x):
        nh = SHNode(x)
        self.root = self.heapmeld(self.root, nh)

    def heaptop(self):
        if self.root is None:
            return None
        return self.root.val

    def heapadd(self, val):
        self.root.add += val

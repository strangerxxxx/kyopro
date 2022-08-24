def prim():

    def _prim(n, edges, start=0):
        import heapq
        visited = [False] * n
        q = []
        heapq.heappush(q, (0, start, None))
        size = 0
        tree = [[] for _ in range(n)]
        while q:
            cost, p, pos = heapq.heappop(q)
            if visited[p]:
                continue
            visited[p] = True
            if not pos is None:
                tree[pos].append(p)
                tree[p].append(pos)
            size += cost
            for nextp, d in edges[p]:
                if not visited[nextp]:
                    heapq.heappush(q, (d, nextp, p))
        return size, tree
    n = int(input())
    edges = [[] for _ in range(n)]
    for i in range(n):
        l = list(map(int, input().split()))
        for j, d in enumerate(l):
            if d >= 0:
                edges[i].append((j, d))
    # n, m, r = map(int, input().split())
    # edges = [[] for _ in range(n)]
    # for _ in range(m):
    #     s, t, w = map(int, input().split())
    #     s -= 1
    #     t -= 1
    #     edges[s].append((t, w))
    #     edges[t].append((s, w))
    print(_prim(n, edges)[0])


def kruskal():
    class UnionFind():
        def __init__(self, n: int):
            self.n = n
            self.parents = [-1] * n

        def find(self, x: int) -> int:
            if self.parents[x] < 0:
                return x
            else:
                self.parents[x] = self.find(self.parents[x])
                return self.parents[x]

        def union(self, x: int, y: int) -> None:
            x = self.find(x)
            y = self.find(y)
            if x == y:
                return
            if self.parents[x] > self.parents[y]:
                x, y = y, x
            self.parents[x] += self.parents[y]
            self.parents[y] = x

        def same(self, x: int, y: int) -> bool:
            return self.find(x) == self.find(y)

    def _kruskal(n, edges, isSorted=False):
        uf = UnionFind(n)
        size = 0
        tree = [[] for _ in range(n)]
        if isSorted:
            r = edges
        else:
            r = sorted(edges)
        for edge in r:
            w, s, t = edge
            if not uf.same(s, t):
                size += w
                uf.union(s, t)
                tree[s].append(t)
                tree[t].append(s)
        return size, tree
    n, m = map(int, input().split())
    edges = []
    for _ in range(m):
        s, t, w = map(int, input().split())
        s -= 1
        t -= 1
        edges.append((w, s, t))
    # n = int(input())
    # edges = []
    # d = [list(map(int, input().split())) for _ in range(n)]
    # for j in range(1, n):
    #     for i in range(j):
    #         if d[i][j] >= 0:
    #             edges.append((d[i][j], i, j))
    print(_kruskal(n, edges)[0])

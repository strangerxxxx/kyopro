from typing import NamedTuple, Optional, List, cast, Tuple


class MFGraph:
    class Edge(NamedTuple):
        src: int
        dst: int
        cap: int
        flow: int

    class _Edge:
        def __init__(self, dst: int, cap: int) -> None:
            self.dst = dst
            self.cap = cap
            self.rev: Optional[MFGraph._Edge] = None

    def __init__(self, n: int) -> None:
        self._n = n
        self._g: List[List[MFGraph._Edge]] = [[] for _ in range(n)]
        self._edges: List[MFGraph._Edge] = []

    def add_edge(self, src: int, dst: int, cap: int = 1) -> int:
        # assert 0 <= src < self._n
        # assert 0 <= dst < self._n
        # assert 0 <= cap
        m = len(self._edges)
        e = MFGraph._Edge(dst, cap)
        re = MFGraph._Edge(src, 0)
        e.rev = re
        re.rev = e
        self._g[src].append(e)
        self._g[dst].append(re)
        self._edges.append(e)
        return m

    def get_edge(self, i: int) -> Edge:
        # i番目に張られた辺を返す O(1)
        # assert 0 <= i < len(self._edges)
        e = self._edges[i]
        re = cast(MFGraph._Edge, e.rev)
        return MFGraph.Edge(re.dst, e.dst, e.cap + re.cap, re.cap)

    def edges(self) -> List[Edge]:
        # 張られた辺をすべて返す O(m)
        return [self.get_edge(i) for i in range(len(self._edges))]

    def change_edge(self, i: int, new_cap: int, new_flow: int) -> None:
        # i番目に張られた辺をnew_cap、new_flowに変更する O(1)
        # assert 0 <= i < len(self._edges)
        # assert 0 <= new_flow <= new_cap
        e = self._edges[i]
        e.cap = new_cap - new_flow
        # assert e.rev is not None
        e.rev.cap = new_flow

    def flow(self, s: int, t: int, flow_limit: Optional[int] = None) -> int:
        # s->tへflow_limitの範囲で流せた量を返す O(n^2 m)
        # assert 0 <= s < self._n
        # assert 0 <= t < self._n
        # assert s != t
        if flow_limit is None:
            flow_limit = cast(int, sum(e.cap for e in self._g[s]))

        current_edge = [0] * self._n
        level = [0] * self._n

        def fill(arr: List[int], value: int) -> None:
            for i in range(len(arr)):
                arr[i] = value

        def bfs() -> bool:
            fill(level, self._n)
            queue = []
            q_front = 0
            queue.append(s)
            level[s] = 0
            while q_front < len(queue):
                v = queue[q_front]
                q_front += 1
                next_level = level[v] + 1
                for e in self._g[v]:
                    if e.cap == 0 or level[e.dst] <= next_level:
                        continue
                    level[e.dst] = next_level
                    if e.dst == t:
                        return True
                    queue.append(e.dst)
            return False

        def dfs(lim: int) -> int:
            stack = []
            edge_stack: List[MFGraph._Edge] = []
            stack.append(t)
            while stack:
                v = stack[-1]
                if v == s:
                    flow = min(lim, min(e.cap for e in edge_stack))
                    for e in edge_stack:
                        e.cap -= flow
                        # assert e.rev is not None
                        e.rev.cap += flow
                    return flow
                next_level = level[v] - 1
                while current_edge[v] < len(self._g[v]):
                    e = self._g[v][current_edge[v]]
                    re = cast(MFGraph._Edge, e.rev)
                    if level[e.dst] != next_level or re.cap == 0:
                        current_edge[v] += 1
                        continue
                    stack.append(e.dst)
                    edge_stack.append(re)
                    break
                else:
                    stack.pop()
                    if edge_stack:
                        edge_stack.pop()
                    level[v] = self._n
            return 0

        flow = 0
        while flow < flow_limit:
            if not bfs():
                break
            fill(current_edge, 0)
            while flow < flow_limit:
                f = dfs(flow_limit - flow)
                flow += f
                if f == 0:
                    break
        return flow

    def min_cut(self, s: int) -> List[bool]:
        # sから到達可能かどうかのリストを返す O(n+m)
        visited = [False] * self._n
        stack = [s]
        visited[s] = True
        while stack:
            v = stack.pop()
            for e in self._g[v]:
                if e.cap > 0 and not visited[e.dst]:
                    visited[e.dst] = True
                    stack.append(e.dst)
        return visited


class BipartiteMatching:
    """
    軽量化Dinic法
    ref : https://snuke.hatenablog.com/entry/2019/05/07/013609
    """

    def __init__(self, n: int, m: int) -> None:
        self._n = n
        self._m = m
        self._to: list[list[int]] = [[] for _ in range(n)]

    def add_edge(self, a: int, b: int) -> None:
        self._to[a].append(b)

    def solve(self) -> list[tuple[int, int]]:
        n, m, to = self._n, self._m, self._to
        pre = [-1] * n
        root = [-1] * n
        p = [-1] * n
        q = [-1] * m
        upd = True
        while upd:
            upd = False
            s = [i for i, x in enumerate(p) if x == -1]
            for i in s:
                root[i] = i
            s_front = 0
            while s_front < len(s):
                v = s[s_front]
                s_front += 1
                if p[root[v]] != -1:
                    continue
                for u in to[v]:
                    if q[u] == -1:
                        while u != -1:
                            q[u] = v
                            p[v], u = u, p[v]
                            v = pre[v]
                        upd = True
                        break
                    u = q[u]
                    if pre[u] != -1:
                        continue
                    pre[u] = v
                    root[u] = root[v]
                    s.append(u)
            if upd:
                pre = [-1] * n
                root = [-1] * n
        return [(v, p[v]) for v in range(n) if p[v] != -1]


class BipartiteGraph:
    def __init__(self, n: int) -> None:
        self.n = n
        self.edges: list[set] = [set() for _ in range(n)]

    def add_edge(self, u, v) -> None:
        self.edges[u].add(v)
        self.edges[v].add(u)

    def construct(self):
        """
        2部グラフを構築する
        return False : 2部グラフではない
        """
        res = [-1] * self.n
        for start in range(self.n):
            if res[start] != -1:
                continue
            res[start] = 0
            q = [start]
            while q:
                i = q.pop()
                for j in self.edges[i]:
                    if res[j] == -1:
                        res[j] = res[i] ^ 1
                        q.append(j)
                    elif res[i] == res[j]:
                        return False
        return res


class GeneralMatching:
    def __init__(self, n):
        self.n = n
        self.graph = [[] for _ in range(n + 1)]
        self.edges = []
        self.cnt = n + 1
        self.mate = [0] * (n + 1)
        self.label = [-1] * (n + 1)
        self.first = [0] * (n + 1)

    def add_edge(self, u, v):  # 0-indexed
        self.graph[u + 1].append((v + 1, self.cnt))
        self.graph[v + 1].append((u + 1, self.cnt))
        self.edges.append((u + 1, v + 1))
        self.cnt += 1

    def eval_first(self, x):
        if self.label[self.first[x]] < 0:
            return self.first[x]
        self.first[x] = self.eval_first(self.first[x])
        return self.first[x]

    def rematch(self, u, v):
        t = self.mate[u]
        self.mate[u] = v
        if self.mate[t] != u:
            return
        if self.label[u] <= self.n:
            self.mate[t] = self.label[u]
            self.rematch(self.label[u], t)
        else:
            x, y = self.edges[self.label[u] - self.n - 1]
            self.rematch(x, y)
            self.rematch(y, x)

    def assign(self, x, y, num):
        r = self.eval_first(x)
        s = self.eval_first(y)
        join = 0
        if r == s:
            return
        self.label[r] = -num
        self.label[s] = -num
        while True:
            if s != 0:
                r, s = s, r
            r = self.eval_first(self.label[self.mate[r]])
            if self.label[r] == -num:
                join = r
                break
            self.label[r] = -num
        v = self.first[x]
        while v != join:
            self.queue.append(v)
            self.label[v] = num
            self.first[v] = join
            v = self.first[self.label[self.mate[v]]]
        v = self.first[y]
        while v != join:
            self.queue.append(v)
            self.label[v] = num
            self.first[v] = join
            v = self.first[self.label[self.mate[v]]]
        return

    def check(self, v):
        self.first[v] = 0
        self.label[v] = 0
        self.queue.append(v)
        while self.queue:
            x = self.queue.popleft()
            for y, lb in self.graph[x]:
                if self.mate[y] == 0 and y != v:
                    self.mate[y] = x
                    self.rematch(x, y)
                    return True
                elif self.label[y] >= 0:
                    self.assign(x, y, lb)
                elif self.label[self.mate[y]] < 0:
                    self.label[self.mate[y]] = x
                    self.first[self.mate[y]] = y
                    self.queue.append(self.mate[y])
        return False

    def solve(self):
        from collections import deque

        for i in range(1, self.n + 1):
            self.queue = deque()
            if self.mate[i] != 0:
                continue
            if self.check(i):
                self.label = [-1] * (self.n + 1)
        res = []
        for i in range(1, self.n + 1):
            if i < self.mate[i]:
                res.append((i - 1, self.mate[i] - 1))
        return res


def scipy_maximunflow():
    from scipy.sparse import csr_matrix
    from scipy.sparse.csgraph import maximum_flow, maximum_bipartite_matching
    import numpy as np

    # l, r, m = map(int, input().split())
    # edges = np.array([input().split() for _ in range(m)], dtype=np.int64).T
    # graph = coo_matrix((np.ones(m, dtype=np.int64), edges),
    #                    shape=(l, r)).tocsr()
    graph = csr_matrix(
        [
            [0, 16, 13, 0, 0, 0],
            [0, 10, 0, 12, 0, 0],
            [0, 4, 0, 0, 14, 0],
            [0, 0, 9, 0, 0, 20],
            [0, 0, 0, 7, 0, 4],
            [0, 0, 0, 0, 0, 0],
        ]
    )
    print(graph)
    print(maximum_flow(graph, 0, 5).flow_value)
    print(maximum_flow(graph, 0, 5).residual)
    # print(maximum_bipartite_matching(
    #     graph, perm_type='column'))  # scipy1.4.1はバグあり
    # ans = [(i, x)
    #        for i, x in enumerate(maximum_bipartite_matching(graph, perm_type='column')) if x >= 0]


# scipy_maximunflow()

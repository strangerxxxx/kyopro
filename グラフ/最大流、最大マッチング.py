from typing import NamedTuple, Optional, List, cast


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
        assert 0 <= src < self._n
        assert 0 <= dst < self._n
        assert 0 <= cap
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
        assert 0 <= i < len(self._edges)
        e = self._edges[i]
        re = cast(MFGraph._Edge, e.rev)
        return MFGraph.Edge(re.dst, e.dst, e.cap + re.cap, re.cap)

    def edges(self) -> List[Edge]:
        # 張られた辺をすべて返す O(m)
        return [self.get_edge(i) for i in range(len(self._edges))]

    def change_edge(self, i: int, new_cap: int, new_flow: int) -> None:
        # i番目に張られた辺をnew_cap、new_flowに変更する O(1)
        assert 0 <= i < len(self._edges)
        assert 0 <= new_flow <= new_cap
        e = self._edges[i]
        e.cap = new_cap - new_flow
        assert e.rev is not None
        e.rev.cap = new_flow

    def flow(self, s: int, t: int, flow_limit: Optional[int] = None) -> int:
        # s->tへflow_limitの範囲で流せた量を返す O(n^2 m)
        assert 0 <= s < self._n
        assert 0 <= t < self._n
        assert s != t
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
                        assert e.rev is not None
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


class BipartiteMatching(MFGraph):
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

    def __init__(self, a: int, b: int) -> None:
        self.a = a
        self.b = b
        self.start = a + b + 1
        self.goal = self.start + 1
        super().__init__(self.goal + 1)
        for i in range(self.a):
            super().add_edge(self.start, i)
        for i in range(self.a, self.a + self.b):
            super().add_edge(i, self.goal)

    def add_edge(self, src: int, dst: int, cap: int = 1) -> int:
        assert 0 <= src < self.a
        assert 0 <= dst < self.b
        return super().add_edge(src, self.a + dst, cap)

    def edges(self) -> List[Edge]:
        return [x._replace(dst=x.dst - self.a) for x in super().edges() if x.src !=
                self.start and x.dst != self.goal]

    def flow(self,  flow_limit: Optional[int] = None) -> int:
        return super().flow(self.start, self.goal, flow_limit=flow_limit)


def scipy_maximunflow():
    from scipy.sparse import csr_matrix
    from scipy.sparse.csgraph import maximum_flow, maximum_bipartite_matching
    import numpy as np
    # l, r, m = map(int, input().split())
    # edges = np.array([input().split() for _ in range(m)], dtype=np.int64).T
    # graph = coo_matrix((np.ones(m, dtype=np.int64), edges),
    #                    shape=(l, r)).tocsr()
    graph = csr_matrix([[0, 16, 13,  0,  0,  0],
                        [0, 10,  0, 12,  0,  0],
                        [0,  4,  0,  0, 14,  0],
                        [0,  0,  9,  0,  0, 20],
                        [0,  0,  0,  7,  0,  4],
                        [0,  0,  0,  0,  0,  0]])
    print(graph)
    print(maximum_flow(graph, 0, 5).flow_value)
    print(maximum_flow(graph, 0, 5).residual)
    print(maximum_bipartite_matching(graph, perm_type='column'))
# scipy_maximunflow()

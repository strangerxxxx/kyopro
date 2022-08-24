from typing import NamedTuple, Optional, List, Tuple, cast


class MCFGraph:
    # https://atcoder.github.io/ac-library/production/document_ja/mincostflow.html
    class Edge(NamedTuple):
        src: int
        dst: int
        cap: int
        flow: int
        cost: int

    class _Edge:
        def __init__(self, dst: int, cap: int, cost: int) -> None:
            self.dst = dst
            self.cap = cap
            self.cost = cost
            self.rev: Optional[MCFGraph._Edge] = None

    def __init__(self, n: int) -> None:
        self._n = n
        self._g: List[List[MCFGraph._Edge]] = [[] for _ in range(n)]
        self._edges: List[MCFGraph._Edge] = []

    def add_edge(self, src: int, dst: int, cap: int, cost: int) -> int:
        # srcからdstへ最大容量cap, コストcostの辺を追加する
        # assert 0 <= src < self._n
        # assert 0 <= dst < self._n
        # assert 0 <= cap
        m = len(self._edges)
        e = MCFGraph._Edge(dst, cap, cost)
        re = MCFGraph._Edge(src, 0, -cost)
        e.rev = re
        re.rev = e
        self._g[src].append(e)
        self._g[dst].append(re)
        self._edges.append(e)
        return m

    def get_edge(self, i: int) -> Edge:
        # assert 0 <= i < len(self._edges)
        e = self._edges[i]
        re = cast(MCFGraph._Edge, e.rev)
        return MCFGraph.Edge(
            re.dst,
            e.dst,
            e.cap + re.cap,
            re.cap,
            e.cost
        )

    def edges(self) -> List[Edge]:
        return [self.get_edge(i) for i in range(len(self._edges))]

    def flow(self, s: int, t: int,
             flow_limit: Optional[int] = None) -> Tuple[int, int]:
        # sからtへ流量flow_limitまで流せるだけ流し、その流量とコストを返す
        return self.slope(s, t, flow_limit)[-1]

    def min_cost_slope(self, s: int, t: int,
                       flow_limit: Optional[int] = None) -> List[Tuple[int, int]]:
        # 返り値に流量とコストの関係の折れ線が入る
        # assert 0 <= s < self._n
        # assert 0 <= t < self._n
        # assert s != t
        if flow_limit is None:
            flow_limit = cast(int, sum(e.cap for e in self._g[s]))

        dual = [0] * self._n
        prev: List[Optional[Tuple[int, MCFGraph._Edge]]] = [None] * self._n

        def refine_dual() -> bool:
            from heapq import heappush, heappop
            pq = [(0, s)]
            visited = [False] * self._n
            dist: List[Optional[int]] = [None] * self._n
            dist[s] = 0
            while pq:
                dist_v, v = heappop(pq)
                if visited[v]:
                    continue
                visited[v] = True
                if v == t:
                    break
                dual_v = dual[v]
                for e in self._g[v]:
                    w = e.dst
                    if visited[w] or e.cap == 0:
                        continue
                    reduced_cost = e.cost - dual[w] + dual_v
                    new_dist = dist_v + reduced_cost
                    dist_w = dist[w]
                    if dist_w is None or new_dist < dist_w:
                        dist[w] = new_dist
                        prev[w] = v, e
                        heappush(pq, (new_dist, w))
            else:
                return False
            dist_t = dist[t]
            for v in range(self._n):
                if visited[v]:
                    dual[v] -= cast(int, dist_t) - cast(int, dist[v])
            return True

        flow = 0
        cost = 0
        prev_cost_per_flow: Optional[int] = None
        result = [(flow, cost)]
        while flow < flow_limit:
            if not refine_dual():
                break
            f = flow_limit - flow
            v = t
            while prev[v] is not None:
                u, e = cast(Tuple[int, MCFGraph._Edge], prev[v])
                f = min(f, e.cap)
                v = u
            v = t
            while prev[v] is not None:
                u, e = cast(Tuple[int, MCFGraph._Edge], prev[v])
                e.cap -= f
                # assert e.rev is not None
                e.rev.cap += f
                v = u
            c = -dual[s]
            flow += f
            cost += f * c
            if c == prev_cost_per_flow:
                result.pop()
            result.append((flow, cost))
            prev_cost_per_flow = c
        return result


class BipartiteMCFMatching(MCFGraph):
    class Edge(NamedTuple):
        src: int
        dst: int
        cap: int
        flow: int
        cost: int

    class _Edge:
        def __init__(self, dst: int, cap: int) -> None:
            self.dst = dst
            self.cap = cap
            self.rev: Optional[MCFGraph._Edge] = None

    def __init__(self, a: int, b: int) -> None:
        self.a = a
        self.b = b
        self.start = a + b + 1
        self.goal = self.start + 1
        super().__init__(self.goal + 1)
        for i in range(self.a):
            super().add_edge(self.start, i, 1, 0)
        for i in range(a, a + b):
            super().add_edge(i, self.goal, 1, 0)

    def add_edge(self, src: int, dst: int, cap: int = 1, cost: int = 0) -> int:
        # assert 0 <= src < self.a
        # assert 0 <= dst < self.b
        return super().add_edge(src, self.a + dst, cap, cost)

    def edges(self) -> List[Edge]:
        return [x._replace(dst=x.dst - self.a) for x in super().edges() if x.src !=
                self.start and x.dst != self.goal]

    def flow(self,  flow_limit: Optional[int] = None) -> int:
        return super().flow(self.start, self.goal, flow_limit=flow_limit)


# class MCFGraph_max(MCFGraph):
#     # 最大版、作成中
#     def __init__(self, n: int, MAX: int = 10 ** 10) -> None:
#         super().__init__(n)
#         self.MAX = MAX

#     def add_edge(self, src: int, dst: int, cap: int, cost: int) -> int:
#         return super().add_edge(src, dst, cap, self.MAX - cost)

#     def get_edge(self, i: int):
#         assert 0 <= i < len(self._edges)
#         e = self._edges[i]
#         re = cast(MCFGraph._Edge, e.rev)
#         return MCFGraph.Edge(
#             re.dst,
#             e.dst,
#             e.cap + re.cap,
#             re.cap,
#             self.MAX - e.cost
#         )

#     def slope(self, s: int, t: int, flow_limit: Optional[int]) -> List[Tuple[int, int]]:
#         # return super().slope(s, t, flow_limit=flow_limit)
#         return [(flow, self.MAX * flow - cost) for flow, cost in super().slope(s, t, flow_limit=flow_limit)]


class MinCostFlow:
    INF = 10 ** 18

    def __init__(self, N):
        self.N = N
        self.G = [[] for i in range(N)]

    def add_edge(self, fr, to, cap, cost):
        forward = [to, cap, cost, None]
        backward = forward[3] = [fr, 0, -cost, forward]
        self.G[fr].append(forward)
        self.G[to].append(backward)

    def flow(self, s, t, f=1 << 64):
        from heapq import heappush, heappop
        N = self.N
        G = self.G
        INF = MinCostFlow.INF

        res = 0
        H = [0] * N
        prv_v = [0] * N
        prv_e = [None] * N

        d0 = [INF] * N
        dist = [INF] * N

        while f:
            dist[:] = d0
            dist[s] = 0
            que = [(0, s)]

            while que:
                c, v = heappop(que)
                if dist[v] < c:
                    continue
                r0 = dist[v] + H[v]
                for e in G[v]:
                    w, cap, cost, _ = e
                    if cap > 0 and r0 + cost - H[w] < dist[w]:
                        dist[w] = r = r0 + cost - H[w]
                        prv_v[w] = v
                        prv_e[w] = e
                        heappush(que, (r, w))
            if dist[t] == INF:
                return None

            for i in range(N):
                H[i] += dist[i]

            d = f
            v = t
            while v != s:
                d = min(d, prv_e[v][1])
                v = prv_v[v]
            f -= d
            res += d * H[t]
            v = t
            while v != s:
                e = prv_e[v]
                e[1] -= d
                e[3][1] += d
                v = prv_v[v]
        return res

class TwoSAT:
    def __init__(self, n):
        self.n = n
        self.scc = StronglyConnectedComponents(2 * n)
        self.ans = [False] * self.n

    def add_clause(self, i, f, j, g):
        self.scc.add_edge(2 * i + (not f), 2 * j + g)
        self.scc.add_edge(2 * j + (not g), 2 * i + f)

    def satisfiable(self):
        self.scc.build()
        for i in range(self.n):
            if self.scc.labels[2 * i] == self.scc.labels[2 * i + 1]:
                return False
            self.ans[i] = self.scc.labels[2 * i] < self.scc.labels[2 * i + 1]
        return True

    def answer(self):
        return self.ans


class StronglyConnectedComponents:
    def __init__(self, n):
        self.n = n
        self.graph = [[] for _ in range(n)]
        self.ord = [-1] * n
        self.low = [-1] * n
        self.labels = [-1] * n
        self.lb_cnt = 0

    def add_edge(self, v, nxt_v):
        self.graph[v].append(nxt_v)

    def build(self):
        k = 0
        idxs = [0] * self.n
        for v in range(self.n):
            if self.ord[v] == -1:
                k = self.dfs(v, k, idxs)
        self.labels = [self.lb_cnt - lb - 1 for lb in self.labels]

    def dfs(self, root, k, idxs):
        dfs_stack = [root]
        scc_stack = []
        while dfs_stack:
            v = dfs_stack[-1]
            if v < 0:
                v = ~v
                prv_v = dfs_stack[-2]
                self.low[prv_v] = min(self.low[prv_v], self.low[v])
                dfs_stack.pop()
                continue
            idx = idxs[v]
            if self.ord[v] == -1:
                self.ord[v] = self.low[v] = k
                k += 1
                scc_stack.append(v)
            if idx < len(self.graph[v]):
                nxt_v = self.graph[v][idx]
                idxs[v] += 1
                if self.ord[nxt_v] == -1:
                    dfs_stack.append(~nxt_v)
                    dfs_stack.append(nxt_v)
                elif self.labels[nxt_v] == -1:
                    self.low[v] = min(self.low[v], self.ord[nxt_v])
            else:
                if self.ord[v] == self.low[v]:
                    while True:
                        prv_v = scc_stack.pop()
                        self.labels[prv_v] = self.lb_cnt
                        if prv_v == v:
                            break
                    self.lb_cnt += 1
                dfs_stack.pop()
        return k

    def construct_dag(self):
        self.dag = [[] for _ in range(self.lb_cnt)]
        self.groups = [[] for _ in range(self.lb_cnt)]
        for v, lb in enumerate(self.labels):
            for nxt_v in self.graph[v]:
                nxt_lb = self.labels[nxt_v]
                if lb == nxt_lb:
                    continue
                self.dag[lb].append(nxt_lb)
            self.groups[lb].append(v)
        return self.dag, self.groups

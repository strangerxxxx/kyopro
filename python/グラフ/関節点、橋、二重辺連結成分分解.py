def bridge_detection(edges, start=0):
    # verified: https://onlinejudge.u-aizu.ac.jp/status/users/stranger/submissions/1/GRL_3_A/judge/6931857/Python3
    # verified: https://onlinejudge.u-aizu.ac.jp/status/users/stranger/submissions/1/GRL_3_B/judge/6931108/Python3
    # verified: https://judge.yosupo.jp/submission/102227
    n = len(edges)
    ord = [-1] * n
    order = ord[:]
    low = ord[:]
    q = [(start, -1)]
    cnt = 0
    edge = [[] for _ in range(n)]
    cycle_graph = [[] for _ in range(n)]
    # dfs_graph = [[] for _ in range(n)]
    while q:
        i, pos = q.pop()
        if ord[i] >= 0:
            edge[i].append(pos)
            continue
        if pos >= 0:
            edge[pos].append(i)
            # dfs_graph[pos].append(i)
        ord[i] = cnt
        order[cnt] = i
        cnt += 1
        for j in edges[i]:
            if j == pos or ord[j] >= 0:
                continue
            q.append((j, i))
    for i, j in enumerate(reversed(order)):
        low[j] = n - i - 1
        for k in edge[j]:
            if ord[k] < low[j]:
                low[j] = ord[k]
            if low[k] >= 0 and low[k] < low[j]:
                low[j] = low[k]
    bridges = []
    for i, j in enumerate(edge):
        for k in j:
            if ord[i] < low[k]:
                bridges.append(sorted((i, k)))
            else:
                cycle_graph[i].append(k)
                cycle_graph[k].append(i)
    return bridges, cycle_graph
    # articulation_points = []
    # for i in range(n):
    #     if i == start:
    #         if len(dfs_graph[i]) >= 2:
    #             articulation_points.append(i)
    #         continue
    #     for j in dfs_graph[i]:
    #         if ord[i] <= low[j]:
    #             articulation_points.append(i)
    #             break
    # return articulation_points


def contract(bridges, cycle_graph):
    n = len(cycle_graph)
    cnt = 0
    visited = [False] * n
    group = [-1] * n
    for u in range(n):
        if not visited[u]:
            group[u] = cnt
            visited[u] = True
            q = [u]
            while q:
                v = q.pop()
                for w in cycle_graph[v]:
                    if not visited[w]:
                        visited[w] = True
                        group[w] = cnt
                        q.append(w)
            cnt += 1
    contracted_graph = [[] for _ in range(cnt)]
    for u, v in bridges:
        contracted_graph[group[u]].append(group[v])
        contracted_graph[group[v]].append(group[u])
    return contracted_graph


def all_bridge_detection(edges):
    class UnionFind:
        def __init__(self, n: int) -> None:
            self.n = n
            self.parent = [-1] * n
            self.groups = n

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
            self.groups -= 1
            return True

        def all_group_members(self) -> dict:
            from collections import defaultdict
            d = defaultdict(list)
            for i in range(self.n):
                p = self.find(i)
                d[p].append(i)
            return d
    n = len(edges)
    uf = UnionFind(n)
    for i, j in enumerate(edges):
        for k in j:
            uf.union(i, k)
    bridges = []
    cycle_graphs = [[] for _ in range(len(edges))]
    for v in uf.all_group_members().values():
        d = {j: i for i, j in enumerate(v)}
        edge = [[] for _ in range(len(v))]
        for i, j in enumerate(v):
            for k in edges[j]:
                edge[i].append(d[k])
        b, c = bridge_detection(edge)
        for i, j in b:
            bridges.append((v[i], v[j]))
        for i, j in enumerate(c):
            for k in j:
                cycle_graphs[v[i]].append(v[k])
    return bridges, cycle_graphs


if __name__ == "__main__":
    graph = [[6], [5, 3], [3, 4, 6], [6, 1, 2], [2], [1], [3, 0, 2]]
    # for i, j in enumerate(graph):
    #     for k in j:
    #         if i < k:
    #             print(i, k)
    print(bridge_detection(graph))
    print(contract(*bridge_detection(graph)))
    # bridges, cycle_graph = bridge_detection(G, start=0)
    graph = [[6], [5, 3], [3, 4, 6], [6, 1, 2], [
        2], [1], [3, 0, 2], [8, 9], [7, 9], [7, 8], [11], [10]]
    print(all_bridge_detection(graph))

def bridge_detection(edges, start=0):
    # verified: https://onlinejudge.u-aizu.ac.jp/status/users/stranger/submissions/1/GRL_3_B/judge/6931108/Python3
    n = len(edges)
    ord = [-1] * n
    order = ord[:]
    low = ord[:]
    q = [(start, -1)]
    cnt = 0
    edge = [[] for _ in range(n)]
    cycle_graph = [[] for _ in range(n)]
    while q:
        i, pos = q.pop()
        if ord[i] >= 0:
            edge[i].append(pos)
            continue
        if pos >= 0:
            edge[pos].append(i)
        ord[i] = cnt
        order[cnt] = i
        cnt += 1
        for j in reversed(edges[i]):
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


if __name__ == "__main__":
    graph = [[1, 2], [0, 3, 4], [0, 7], [1, 4], [1, 3, 5, 6], [
        4, 6], [4, 5], [2, 8, 11], [7, 9, 10], [8], [8, 11], [7, 10]]
    print(bridge_detection(graph))
    print(contract(*bridge_detection(graph)))

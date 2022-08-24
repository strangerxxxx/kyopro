def articulation_detection(graph, start=0):
    # 関節点
    import sys
    RECURSION_LIMIT = 10 ** 6
    sys.setrecursionlimit(RECURSION_LIMIT)
    n = len(graph)
    order = [-1] * n
    cnt = -1
    articulation_points = []

    def dfs(u, prev):
        nonlocal cnt
        cnt += 1
        low_pt = order[u] = cnt
        f_cnt = art_flag = 0
        for v in graph[u]:
            if v == prev:
                continue
            if order[v] == -1:
                v_low_pt = dfs(v, u)
                art_flag |= v_low_pt >= order[u]
                low_pt = min(v_low_pt, low_pt)
                f_cnt += 1
            else:
                low_pt = min(low_pt, order[v])
        if len(graph[u]) > 1 and ((prev != -1 and art_flag) or (prev == -1 and f_cnt > 1)):
            articulation_points.append(u)
        return low_pt
    dfs(start, -1)
    return articulation_points


def bridge_detection(graph, start=0):
    # 橋、二重辺連結成分
    import sys
    RECURSION_LIMIT = 10 ** 6
    sys.setrecursionlimit(RECURSION_LIMIT)
    n = len(graph)
    order = [-1] * n
    bridges = []
    cycle_graph = [set() for _ in range(n)]
    cnt = -1

    def dfs(u, prev):
        nonlocal cnt
        cnt += 1
        low_pt = order[u] = cnt
        for v in graph[u]:
            if v == prev:
                continue
            if order[v] == -1:
                v_low_pt = dfs(v, u)
                if v_low_pt > order[u]:
                    bridges.append(tuple(sorted([u, v])))
                else:
                    cycle_graph[u].add(v)
                    cycle_graph[v].add(u)
                low_pt = min(low_pt, v_low_pt)
            else:
                low_pt = min(low_pt, order[v])
                cycle_graph[u].add(v)
                cycle_graph[v].add(u)
        return low_pt
    dfs(start, -1)
    return sorted(bridges), cycle_graph


def contract(bridges, cycle_graph):
    import sys
    RECURSION_LIMIT = 10 ** 6
    sys.setrecursionlimit(RECURSION_LIMIT)
    n = len(cycle_graph)
    cnt = -1
    visited = [0] * n
    group = [-1] * n

    def dfs(u, cnt):
        visited[u] = 1
        for v in cycle_graph[u]:
            if visited[v] == 0:
                group[v] = cnt
                dfs(v, cnt)

    for u in range(n):
        if visited[u] == 0:
            cnt += 1
            group[u] = cnt
            dfs(u, cnt)

    contracted_graph = [[] for _ in range(cnt + 1)]
    for u, v in bridges:
        contracted_graph[group[u]] += [group[v]]
        contracted_graph[group[v]] += [group[u]]
    return contracted_graph


if __name__ == "__main__":
    graph = [[6], [5, 3], [3, 4, 6], [6, 1, 2], [2], [1], [3, 0, 2]]
    print(articulation_detection(graph))
    print(bridge_detection(graph))
    print(contract(*bridge_detection(graph)))
    # bridges, cycle_graph = bridge_detection(G, start=0)

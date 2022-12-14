def strongly_connected_components_scipy():
    import numpy as np
    from scipy.sparse import csr_matrix
    from scipy.sparse.csgraph import connected_components
    n, m = map(int, input().split())
    edge = np.array([input().split() for _ in range(m)], dtype=int).T
    a = csr_matrix((np.ones(m, dtype=np.int64), (edge - 1)), shape=(n, n))
    result = connected_components(a, directed=True, connection='strong')
    print(result)


def strongly_connected_components(n, arrays):
    edge = [[] for _ in range(n)]
    redge = [[] for _ in range(n)]
    for a, b in arrays:
        edge[a].append(b)
        redge[b].append(a)
    used = [False] * n
    order = []
    for i in range(n):
        if not used[i]:
            stack = [i]
            while stack:
                point = stack.pop()
                if point < 0:
                    order.append(~point)
                    continue
                if used[point]:
                    continue
                used[point] = True
                stack.append(~point)
                for nextp in edge[point]:
                    if not used[nextp]:
                        stack.append(nextp)
    group_count = 0
    group_index = [-1] * n
    for i in reversed(order):
        if group_index[i] == -1:
            stack = [i]
            group_index[i] = group_count
            while stack:
                point = stack.pop()
                for nextp in redge[point]:
                    if group_index[nextp] == -1:
                        group_index[nextp] = group_count
                        stack.append(nextp)
            group_count += 1
    return group_count, group_index


def strongly_connected_components2(n, arrays):
    edge = [[] for _ in range(n)]
    redge = [[] for _ in range(n)]
    for a, b in arrays:
        edge[a].append(b)
        redge[b].append(a)
    used = [False] * n
    order = []
    for i in range(n):
        if not used[i]:
            stack = [i]
            while stack:
                point = stack.pop()
                if point < 0:
                    order.append(~point)
                    continue
                if used[point]:
                    continue
                used[point] = True
                stack.append(~point)
                for nextp in edge[point]:
                    if not used[nextp]:
                        stack.append(nextp)
    group_count = 0
    groups = []
    res = [-1] * n
    for i in reversed(order):
        if res[i] == -1:
            stack = [i]
            res[i] = group_count
            groups.append([i])
            while stack:
                point = stack.pop()
                for nextp in redge[point]:
                    if res[nextp] == -1:
                        res[nextp] = group_count
                        groups[-1].append(nextp)
                        stack.append(nextp)
            group_count += 1
    return group_count, groups


def construct_dag(n, arrays, groups):
    edge = [[] for _ in range(n)]
    for a, b in arrays:
        edge[a].append(b)
    inv_groups = [None] * n
    for index, i in enumerate(groups):
        for j in i:
            inv_groups[j] = index
    dag = [set() for _ in range(len(groups))]
    for index, i in enumerate(groups):
        for j in i:
            for k in edge[j]:
                if inv_groups[k] != index:
                    dag[index].add(inv_groups[k])
    return dag

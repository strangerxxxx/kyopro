def strongly_connected_components_scipy():
    import numpy as np
    from scipy.sparse import csr_matrix
    from scipy.sparse.csgraph import connected_components
    n, m = map(int, input().split())
    edge = np.array([input().split() for _ in range(m)], dtype=int).T
    a = csr_matrix((np.ones(m, dtype=np.int64), (edge - 1)), shape=(n, n))
    result = connected_components(a, directed=True, connection='strong')
    print(result)


def strongly_connected_components():
    n, m = map(int, input().split())
    edge = [[] for _ in range(n)]
    redge = [[] for _ in range(n)]
    for _ in range(m):
        a, b = map(int, input().split())
        a -= 1
        b -= 1
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
    group = 0
    res = [-1] * n
    for i in reversed(order):
        if res[i] == -1:
            stack = [i]
            res[i] = group
            while stack:
                point = stack.pop()
                for nextp in redge[point]:
                    if res[nextp] == -1:
                        res[nextp] = group
                        stack.append(nextp)
            group += 1
    return group, res


def strongly_connected_components2():
    n, m = map(int, input().split())
    edge = [[] for _ in range(n)]
    redge = [[] for _ in range(n)]
    for _ in range(m):
        a, b = map(int, input().split())
        a -= 1
        b -= 1
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
    group = 0
    groups = []
    res = [-1] * n
    for i in reversed(order):
        if res[i] == -1:
            stack = [i]
            res[i] = group
            groups.append([i])
            while stack:
                point = stack.pop()
                for nextp in redge[point]:
                    if res[nextp] == -1:
                        res[nextp] = group
                        groups[-1].append(nextp)
                        stack.append(nextp)
            group += 1
    return groups

def TopoSort(n, edges, start=0):
    # 行きがけの順と親を含まない木を返す
    parent = [None] * n
    queue = [start]
    tree = [[] for _ in range(n)]
    order = []
    parent[start] = -1
    while queue:
        i = queue.pop()
        order.append(i)
        # for a in reversed(sorted(edges[i])):
        for a in edges[i]:
            if parent[a] is None:
                tree[i].append(a)
                queue.append(a)
                parent[a] = i
    return order, tree, parent

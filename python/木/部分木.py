def patialTree(i: int, opposite: int, edges):
    n = len(edges)
    tree = [[] for _ in range(n)]
    q = [(i, opposite)]
    while q:
        j, k = q.pop()
        for child in edges[j]:
            if child == k:
                continue
            tree[j].append(child)
            tree[child].append(j)
            q.append((child, j))
    return tree

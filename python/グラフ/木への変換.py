def convert_to_tree(n, edge, start=0):
    tree = [[] for _ in range(n)]
    from collections import deque
    q = deque([start])
    parents = [None] * n
    parents[start] = -1
    while q:
        i = q.popleft()
        for j in edge[i]:
            if parents[j] is None:
                tree[i].append(j)
                q.append(j)
                parents[j] = i
    return tree, parents

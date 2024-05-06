def center_of_tree(edges: list[list[int]], start: int = 0) -> int:
    n = len(edges)
    q = [~start, start]
    child_size = [1] * n
    parent = [-1] * n
    while q:
        i = q.pop()
        if i >= 0:
            for j in edges[i]:
                if j != parent[i]:
                    q.append(~j)
                    q.append(j)
                    parent[j] = i
        else:
            for j in edges[~i]:
                if j != parent[~i]:
                    child_size[~i] += child_size[j]
    i = start
    while True:
        for j in edges[i]:
            if j != parent[i] and child_size[j] > n // 2:
                i = j
                break
        else:
            return i

def treeDepth(n: int, edge, start: int = 0, need_path: bool = False) -> int:
    from collections import deque
    childs = [[] for _ in range(n)]
    for i, j in edge:
        childs[i - 1].append(j - 1)
        childs[j - 1].append(i - 1)

    def bfs(start: int, need_path: bool = False):
        q = deque()
        last = start
        before = [None] * n
        before[start] = -1
        q.append((start, 1))
        while q:
            p, distance = q.popleft()
            last = p
            for i in childs[p]:
                if before[i] is None:
                    q.append((i, distance + 1))
                    before[i] = p
        if not need_path:
            return last, distance
        p = last
        path = [p]
        while before[p] >= 0:
            p = before[p]
            path.append(p)
        return last, distance, path[::-1]
    p1, _ = bfs(start, False)
    if need_path:
        _, d2, path = bfs(p1, True)
        return d2, path
    _, d2 = bfs(p1, False)
    return d2


def treeHeight(n: int, edge, start: int = None) -> int:
    childs = [[] for _ in range(n)]
    for i, j, d in edge:
        childs[i - 1].append((j - 1, d))
        childs[j - 1].append((i - 1, d))

    def bfs(start: int = 0, need_path: bool = False):
        q = []
        last = start
        before = [None] * n
        q.append((0, start, -1))
        farp, fard = None, 0
        while q:
            distance, p, pos = q.pop()
            before[p] = pos
            if distance > fard:
                farp, fard = p, distance
            for i, dist in childs[p]:
                if i == pos:
                    continue
                q.append((distance + dist, i, p))
        if not need_path:
            return farp, fard
        p = farp
        path = [p]
        while before[p] >= 0:
            p = before[p]
            path.append(p)
        return farp, fard, path[::-1]
    if start is None:
        start, _ = bfs(0)
    _, d = bfs(start)
    return d

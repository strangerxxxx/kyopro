def treeDepth(n: int, edges, start: int = 0, need_path: bool = False):
    # 木の深さ(重みなし)
    from collections import deque

    def bfs(start: int, need_path: bool = False):
        q = deque()
        last = start
        before = [None] * n
        before[start] = -1
        q.append((start, 1))
        while q:
            p, distance = q.popleft()
            last = p
            for i in edges[p]:
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

    if n == 1:
        if need_path:
            return 0, [0]
        return 0
    p1, _ = bfs(start, False)
    if need_path:
        _, d2, path = bfs(p1, True)
        return d2, path
    _, d2 = bfs(p1, False)
    return d2


def treeHeight(n: int, edges, start: int = None) -> int:
    # 木の深さ(重みあり)
    # verified: https://bit.ly/3qqtlGI
    def bfs(start: int = 0, need_path: bool = False):
        q = []
        before = [None] * n
        q.append((0, start, -1))
        farp, fard = start, 0
        while q:
            distance, p, pos = q.pop()
            before[p] = pos
            if distance > fard:
                farp, fard = p, distance
            for i, dist in edges[p]:
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

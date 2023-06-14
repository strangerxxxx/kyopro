def shortest():
    from scipy.sparse.csgraph import shortest_path
    from scipy.sparse.csgraph import floyd_warshall, dijkstra
    from scipy.sparse import csr_matrix, coo_matrix
    import numpy as np

    # l = [[0, 1, 2, 0], [0, 0, 0, 1], [3, 0, 0, 3], [0, 0, 0, 0]]
    # a = np.array(l)
    n, m = map(int, input().split())
    edge = np.array([input().split() for _ in range(m)], dtype=np.int64).T
    a = coo_matrix((edge[2], (edge[:2] - 1)), shape=(n, n)).tocsr()
    # a = coo_matrix((edge[2], (edge[:2] - 1)), shape=(n, n)).tocsr()
    # a = coo_matrix((np.ones(m, dtype=np.int64), (edge - 1)), shape=(n, n)).tocsr()  # 重みなし
    s = shortest_path(a, directed=False)
    # s = shortest_path(a, indices=0, directed=False)  # 頂点0からのみの最短距離
    # s = shortest_path(a)  # 有向グラフ
    # s, p = shortest_path(a, return_predecessors=True)  # 最短経路p
    # s = shortest_path(a, unweighted=True)  # 重みなし
    # get_path(start, goal, p)  # 下のdefも必要


def get_path(start, goal, pred):
    return get_path_row(start, goal, pred[start])


def get_path_row(start, goal, pred_row):
    path = []
    i = goal
    while i != start and i >= 0:
        path.append(i)
        i = pred_row[i]
    if i < 0:
        return []
    path.append(i)
    return path[::-1]


def warshall():
    # ワーシャル–フロイド法 全ての二頂点に対する最短距離
    def warshall_floyd(d, n=None, inf=float("inf")):
        if n is None:
            n = len(d)
        for k in range(n):
            for i in range(n):
                for j in range(n):
                    x = d[i][k] + d[k][j]
                    if x < d[i][j]:
                        d[i][j] = x
        return d

    n, m = map(int, input().split())
    inf = float("inf")
    d = [[inf] * n for _ in range(n)]
    for i in range(n):
        d[i][i] = 0
    for _ in range(m):
        x, y, z = map(int, input().split())
        d[x - 1][y - 1] = z
        d[y - 1][x - 1] = z
    cost = warshall_floyd(d, n, inf)
    print(cost)


def warshall_route():
    # ワーシャル–フロイド法経路あり
    def warshall_floyd(d, n=None, inf=float("inf")):
        if n is None:
            n = len(d)
        p = [[-inf] * n for _ in range(n)]
        for i in range(n):
            for j in range(n):
                if 0 < d[i][j] < inf:
                    p[i][j] = i
        for k in range(n):
            for i in range(n):
                for j in range(n):
                    x = d[i][k] + d[k][j]
                    if x < d[i][j]:
                        d[i][j] = x
                        p[i][j] = k
        return d, p

    n, m = map(int, input().split())
    inf = float("inf")
    d = [[inf] * n for _ in range(n)]
    for i in range(n):
        d[i][i] = 0
    for _ in range(m):
        x, y, z = map(int, input().split())
        d[x - 1][y - 1] = z
        d[y - 1][x - 1] = z
    cost = warshall_floyd(d, n, inf)
    print(cost)


def Dijkstras_heapq():
    # ダイクストラ法Queue版 O(ElogV)
    def Dijkstra(start, n, edge, inf=float("inf")):
        import heapq

        d = [inf] * n
        d[start] = 0
        queue = []
        for e in edge[start]:
            heapq.heappush(queue, e)
        while queue:
            dist, point = heapq.heappop(queue)
            if d[point] != inf:
                continue
            d[point] = dist
            for nextdist, nextp in edge[point]:
                if d[nextp] == inf:
                    heapq.heappush(queue, (dist + nextdist, nextp))
        return d

    n, m = map(int, input().split())
    inf = float("inf")
    edge = [[] for _ in range(n)]
    for _ in range(m):
        x, y, z = map(int, input().split())
        edge[x - 1].append((z, y - 1))
        edge[y - 1].append((z, x - 1))
    print(Dijkstra(0, n, edge, inf))


def Dijkstras_route():
    # ダイクストラ法Queue版 経路あり O(ElogV)
    def Dijkstra(start, n, edge, inf=float("inf")):
        import heapq

        d = [inf] * n
        d[start] = 0
        queue = []
        path = [[] for _ in range(n)]  # 全パス
        path[start] = [start]  # 全パス
        # path = [-9999] * n
        for e in edge[start]:
            heapq.heappush(queue, e + [start])
        while queue:
            dist, point, before = heapq.heappop(queue)
            if d[point] != inf:
                continue
            d[point] = dist
            path[point] = path[before] + [point]  # 全パス
            # path[point] = before
            for nextdist, nextp in edge[point]:
                if d[nextp] == inf:
                    heapq.heappush(queue, [dist + nextdist, nextp, point])
        return d, path

    n, m = map(int, input().split())
    inf = float("inf")
    edge = [[] for _ in range(n)]
    for _ in range(m):
        x, y, z = map(int, input().split())
        edge[x - 1].append([z, y - 1])
        edge[y - 1].append([z, x - 1])
    print(Dijkstra(0, n, edge, inf))


def Dijkstras_01():
    # 01-BFS
    def Dijkstra(start, n, edge, inf=float("inf")):
        from collections import deque

        d = [inf] * n
        queue = deque()
        queue.append((0, start))
        while queue:
            dist, point = queue.popleft()
            if d[point] <= dist:
                continue
            d[point] = dist
            for nextdist, nextp in edge[point]:
                distance = dist + nextdist
                if d[nextp] <= distance:
                    continue
                if nextdist == 0:
                    queue.appendleft((distance, nextp))
                else:
                    queue.append((distance, nextp))
        return d

    n, m = map(int, input().split())
    inf = float("inf")
    edge = [[] for _ in range(n)]
    for _ in range(m):
        x, y, z = map(int, input().split())
        edge[x - 1].append((z, y - 1))
        edge[y - 1].append((z, x - 1))
    print(Dijkstra(0, n, edge, inf))


def Dijkstras_onedist():
    # ダイクストラ法(距離が1) O(ElogV)
    def Dijkstra(start, n, edge, inf=float("inf")):
        from collections import deque

        d = [inf] * n
        queue = deque()
        queue.append((0, start))
        while queue:
            dist, point = queue.popleft()
            if d[point] != inf:
                continue
            d[point] = dist
            for nextp in edge[point]:
                if d[nextp] == inf:
                    queue.append((dist + 1, nextp))
        return d

    n, m = map(int, input().split())
    inf = float("inf")
    edge = [[] for _ in range(n)]
    for _ in range(m):
        x, y = map(int, input().split())
        edge[x - 1].append(y - 1)
        edge[y - 1].append(x - 1)
    print(Dijkstra(0, n, edge, inf))


def Dijkstras_dict():
    # ダイクストラ法dict版 O(ElogV)
    def Dijkstra(s, g):
        import heapq

        d = {}
        d[s] = 0
        queue = []
        for q in edge[s]:
            heapq.heappush(queue, q)
        while queue:
            dist, point = heapq.heappop(queue)
            if point in d:
                continue
            d[point] = dist
            if point == g:
                return dist
            for nextdist, nextp in edge[point]:
                if not nextp in d:
                    heapq.heappush(queue, (dist + nextdist, nextp))
        return d

    import sys

    input = sys.stdin.readline
    from collections import defaultdict

    n, m = map(int, input().split())
    edge = defaultdict(set)
    for _ in range(m):
        p, q, c = map(int, input().split())
        p, q = p - 1, q - 1
        edge[(p, -1)].add((1, (p, c)))
        edge[(q, -1)].add((1, (q, c)))
        edge[(p, c)].add((0, (p, -1)))
        edge[(q, c)].add((0, (q, -1)))
        edge[(p, c)].add((0, (q, c)))
        edge[(q, c)].add((0, (p, c)))
    print(Dijkstra((0, -1), (n - 1, -1)))


def BellmanFord():
    # ベルマンフォード法 abc061_d
    def BF(n, start, edge, inf=float("inf")):
        d = [inf] * n
        d[start] = 0
        for i in range(n * 2):
            updated = False
            for before, after, dist in edge:
                if before != inf and d[before] + dist < d[after]:
                    updated = True
                    if i < n:
                        d[after] = d[before] + dist
                    else:
                        d[after] = -inf
            if not updated:
                break
        return d

    n, m = map(int, input().split())
    inf = float("inf")
    edge = [list(map(int, input().split())) for _ in range(m)]
    edge = [[x - 1, y - 1, -z] for x, y, z in edge]
    print(-BF(n, 0, edge, inf)[-1])

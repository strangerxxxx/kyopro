def topo():
    def topological_sort(n: int, edge):
        from collections import deque
        in_cnt = [0] * n
        outs = [[] for _ in range(n)]
        for a, b in edge:
            in_cnt[b] += 1
            outs[a].append(b)
        res = []
        queue = deque([i for i, x in enumerate(in_cnt) if x == 0])
        while queue:
            v = queue.popleft()
            res.append(v)
            for v2 in outs[v]:
                in_cnt[v2] -= 1
                if in_cnt[v2] == 0:
                    queue.append(v2)
        if len(res) == n:
            return res
        else:
            return None
    n, m = map(int, input().split())
    edge = [tuple(map(int, input().split())) for _ in range(m)]
    ans = topological_sort(n, edge)
    print(*ans if ans else -1)


def topo2():
    def topological_sort(n: int, outs):
        from collections import deque
        in_cnt = [0] * n
        for a in edge:
            for b in a:
                in_cnt[b] += 1
        res = []
        queue = deque([i for i, x in enumerate(in_cnt) if x == 0])
        while queue:
            v = queue.popleft()
            res.append(v)
            for v2 in outs[v]:
                in_cnt[v2] -= 1
                if in_cnt[v2] == 0:
                    queue.append(v2)
        if len(res) == n:
            return res
        else:
            return None
    n, m = map(int, input().split())
    edge = [tuple(map(int, input().split())) for _ in range(m)]
    outs = [[] for _ in range(n)]
    for a, b in edge:
        outs[a].append(b)
    ans = topological_sort(n, outs)
    print(*ans if ans else -1)


def topo_dfs():
    def topological_sort_dfs(n, edge):
        reached = [False] * n
        q = []
        res = []
        starts = set(range(n))
        for i in edge:
            for j in i:
                starts.discard(j)
        for i in starts:
            if not reached[i]:
                q.append(i)
                while q:
                    point = q.pop()
                    if point >= 0:
                        if reached[point]:
                            continue
                        reached[point] = True
                        q.append(~point)
                        for nextp in edge[point]:
                            if not reached[nextp]:
                                q.append(nextp)
                    else:
                        res.append(~point)
        if len(res) == n:
            return res[::-1]
        else:
            return None
    n, m = map(int, input().split())
    edge = [[] for _ in range(n)]
    for _ in range(m):
        s, t = map(int, input().split())
        edge[s - 1].append(t - 1)
    topological_sort_dfs(n, edge)


def topo_n():
    # トポロジカルソートの候補数
    n, m = map(int, input().split())
    from collections import defaultdict
    edge = defaultdict(int)
    # ノードIDと対応するビット位置は繰り返し求めるので事前計算しておく
    jbs = [(j, 1 << j) for j in range(n)]
    for _ in range(m):
        a, b = map(int, input().split())
        edge[a - 1] += 1 << (b - 1)
    dp = [0] * (1 << n)
    dp[0] = 1
    for i in range(1 << n):
        for j, jb in jbs:
            if (i & jb) == 0 and (i & edge[j]) == 0:
                dp[i | jb] += dp[i]
    print(dp[-1])

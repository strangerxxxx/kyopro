def has_cycle(edge, n: int) -> bool:
    # 閉路検査
    in_cnt = [0] * n
    outs = [[] for _ in range(n)]
    for a, b in edge:
        in_cnt[b] += 1
        outs[a].append(b)
    res = 0
    queue = [i for i in range(n) if in_cnt[i] == 0]
    while queue:
        v = queue.pop()
        res += 1
        for v2 in outs[v]:
            in_cnt[v2] -= 1
            if in_cnt[v2] == 0:
                queue.append(v2)
    if res == n:
        return False
    else:
        return True


def cycle_detection_directed(edges):
    # verify: https://judge.yosupo.jp/submission/141814
    n = len(edges)
    seen = [False] * n
    finished = [False] * n
    for start in range(n):
        if seen[start]:
            continue
        q = [~start, start]
        res = []
        while q:
            i = q.pop()
            if i >= 0:
                if finished[i]:
                    continue
                seen[i] = True
                res.append(i)
                for j in edges[i]:
                    if seen[j] and not finished[j]:
                        return res[res.index(j) :]
                    if not seen[j]:
                        q.append(~j)
                        q.append(j)
            else:
                finished[~i] = True
                if res[-1] == ~i:
                    res.pop()
    return []


def cycle_detection_undirected(edges):
    # 長さ2以下のサイクルは検出しない
    # verify: https://judge.yosupo.jp/submission/141819
    n = len(edges)
    seen = [False] * n
    finished = [False] * n
    for start in range(n):
        if seen[start]:
            continue
        q = [(~start, -1), (start, -1)]
        res = []
        while q:
            i, pos = q.pop()
            if i >= 0:
                if finished[i]:
                    continue
                seen[i] = True
                res.append(i)
                for j in edges[i]:
                    if j == pos:
                        continue
                    if seen[j] and not finished[j]:
                        return res[res.index(j) :]
                    if not seen[j]:
                        q.append((~j, i))
                        q.append((j, i))
            else:
                finished[~i] = True
                if res[-1] == ~i:
                    res.pop()
    return []


def namori_decompositon(edges):
    in_cnt = [len(x) for x in edges]
    q = [i for i, x in enumerate(in_cnt) if x == 1]
    u = UnionFind(len(edges))
    while q:
        i = q.pop()
        for j in edges[i]:
            u.union(i, j)
            in_cnt[j] -= 1
            if in_cnt[j] == 1:
                q.append(j)
    return u

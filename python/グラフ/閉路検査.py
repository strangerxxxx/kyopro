def has_cycle(edge, n: int) -> bool:
    # 閉路検査
    from collections import deque
    in_cnt = [0] * n
    outs = [[] for _ in range(n)]
    for a, b in edge:
        in_cnt[b] += 1
        outs[a].append(b)
    res = []
    queue = deque([i for i in range(n) if in_cnt[i] == 0])
    while queue:
        v = queue.popleft()
        res.append(v)
        for v2 in outs[v]:
            in_cnt[v2] -= 1
            if in_cnt[v2] == 0:
                queue.append(v2)
    if len(res) == n:
        return False
    else:
        return True
